#pragma once

// NOTE: make sure you include this file with
//  #define JAYSON_IMPL
// in at least one _source_ file

#include <concepts>
#include <format>
#include <functional>
#include <iomanip>
#include <map>
#include <span>
#include <sstream>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <lexible.hh>

namespace jayson {

struct nil
{};

class val;
using array = std::vector<val>;
using obj = std::map<std::string, val, std::less<>>;
using val_impl = std::variant<nil, double, bool, std::string, array, obj>;

struct val_typename_visitor
{
  std::string_view operator()(nil const) const { return "nil"; }
  std::string_view operator()(double const) const { return "number"; }
  std::string_view operator()(bool const) const { return "bool"; }
  std::string_view operator()(std::string const) const { return "string"; }
  std::string_view operator()(array const) const { return "array"; }
  std::string_view operator()(obj const) const { return "object"; }
};

class val : public val_impl
{

  template<typename T>
  T as_else()
  {
    if (std::holds_alternative<T>(*this))
      return std::get<T>(this);
  }

public:
  using val_impl::variant;
  static val parse(std::string_view);

  std::string serialize(bool pretty = false);

  template<typename T>
  T& as()
  {
    return std::get<T>(*this);
  }

  template<typename T>
  T const& as() const
  {
    return std::get<T>(*this);
  }

  template<typename T>
  T& as_else(std::function<T()> or_else)
  {
    if (std::holds_alternative<T>(*this))
      return std::get<T>(*this);
    else
      *this = or_else();
    return this->as<T>();
  }

  template<typename T, typename EXCEPTION = std::runtime_error>
  T& as_else_throw(auto... to_throw)
  {
    if (std::holds_alternative<T>(*this))
      return std::get<T>(*this);
    else
      throw EXCEPTION(to_throw...);
  }

  template<typename T, typename EXCEPTION = std::runtime_error>
  T const& as_else_throw(auto... to_throw) const
  {
    if (std::holds_alternative<T>(*this))
      return std::get<T>(*this);
    else
      throw EXCEPTION(to_throw...);
  }
};

template<auto N>
struct comptime_str
{
  constexpr comptime_str(char const (&str)[N])
  {
    std::copy(str, str + N - 1, data);
  }

  char data[N]{};

  constexpr operator std::string_view() const { return { data, data + N - 1 }; }
};

template<comptime_str FIELD_NAME, auto FIELD_PTR, bool REQUIRED = true>
struct obj_field
{
  std::string_view static constexpr name = FIELD_NAME;
  auto static constexpr ptr = FIELD_PTR;
  bool static constexpr required = REQUIRED;
};

template<typename T>
concept is_number =
  requires { requires std::integral<T> or std::floating_point<T>; };

template<typename T>
concept has_jayson_descriptor_fields = requires { typename T::jayson_fields; };

struct type_consolidator
{
  static double consol(is_number auto const);
  static std::string consol(std::convertible_to<std::string_view> auto const);
  static obj consol(has_jayson_descriptor_fields auto const);
  template<typename T>
  static array consol(std::vector<T> const);

  template<typename T>
  using get = decltype(consol(std::declval<T>()));
};

template<has_jayson_descriptor_fields T>
void
deserialize(val const& from, T& into)
{
  using fields = T::jayson_fields;

  if (not std::holds_alternative<obj>(from))
    throw std::runtime_error(
      "when attempting to deserialize a jayson value into a data structure, "
      "expected an object and did not find one");

  obj const& object = from.as<obj>();

  std::apply(
    [&]<typename... Ts>(Ts&&... args) {
      (
        [&]<typename FIELD>(FIELD) {
          auto const from_field = object.find(Ts::name);

          if (from_field == object.end()) {
            if (FIELD::required)
              throw std::runtime_error(
                std::format("unable to find required field {} in jayson object",
                            FIELD::name));
            else
              return;
          }

          deserialize(from_field->second, into.*Ts::ptr);
        }(args),
        ...);
    },
    fields());
}

template<is_number T>
void
deserialize(val const& from, T& into)
{
  if (not std::holds_alternative<double>(from))
    throw std::runtime_error(std::format(
      "expected number while deserializing a jayson object, found <{}>",
      std::visit(val_typename_visitor(), from)));

  into = from.as<double>();
}

void inline deserialize(val const& from, std::string& into)
{
  if (not std::holds_alternative<std::string>(from))
    throw std::runtime_error(std::format(
      "expected string while deserializing a jayson object, found <{}>",
      std::visit(val_typename_visitor(), from)));

  into = from.as<std::string>();
}

template<typename T>
void inline deserialize(val const& from, std::vector<T>& into)
{
  if (not std::holds_alternative<array>(from))
    throw std::runtime_error(std::format(
      "expected an array while deserializing a jayson object, found <{}>",
      std::visit(val_typename_visitor(), from)));

  auto const& arr = from.as<array>();

  for (auto const& t : arr) {
    T n;

    if (not std::holds_alternative<type_consolidator::get<T>>(t))
      throw std::runtime_error(std::format(
        "expected an {} while deserializing a jayson object, found <{}>",
        std::visit(val_typename_visitor(), val(type_consolidator::get<T>())),
        std::visit(val_typename_visitor(), t)));

    deserialize(t, n);
    into.push_back(std::move(n));
  }
}

template<typename T>
  requires(has_jayson_descriptor_fields<T>)
auto inline serialize(T const& t);

template<typename T>
auto inline serialize(std::vector<T> const& t)
{
  array out;
  for (auto const& v : t)
    out.push_back(serialize(v));
  return out;
}

template<typename T>
  requires(not has_jayson_descriptor_fields<T>)
auto inline serialize(T const& t)
{
  return val((type_consolidator::get<T>)(t));
}

template<typename T>
  requires(has_jayson_descriptor_fields<T>)
auto inline serialize(T const& t)
{
  obj out;

  std::apply(
    [&]<typename... FIELDS>(FIELDS&&...) {
      (out.insert_or_assign(std::string(FIELDS::name),
                            serialize(t.*FIELDS::ptr)),
       ...);
    },
    typename T::jayson_fields());

  return out;
}

};

#ifdef JAYSON_IMPL
namespace jayson {

class _jayson_impl
{

  enum class TokenType
  {
    LEXIBLE_EOF,

    String,
    Number,

    TrueLiteral,
    FalseLiteral,
  };

  static constexpr std::string_view string_literal_regex = R"()";

  struct Token
  {
    enum class Type
    {
      LeftBrace,
      RightBrace,
      LeftBracket,
      RightBracket,
      Colon,
      Comma,

      Number,
      String,
      True,
      False,
      Null,
    };

    Type m_type;
    std::span<char const> m_span;
  };

  [[gnu::always_inline]]
  static inline bool isspace(char const c)
  {
    return c == ' ' or c == '\n';
  }

  [[gnu::always_inline]] static inline bool isdigit(char const c)
  {
    return c >= '0' and c <= '9';
  }

  [[gnu::always_inline]] static inline bool isalpha(char const c)
  {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
  }

  static std::vector<Token> tokenate(std::string_view src)
  {
    std::vector<Token> tokens;
    tokens.reserve(src.size() / 25);

    unsigned idx = 0;
    unsigned tmp = 0;

    auto skip_whitespace = [&] [[gnu::always_inline]] () {
      while (isspace(src[idx]))
        idx++;
    };

    auto lex_number = [&] {
      auto const leftside_begin = idx;
      if (src[idx] == '+' or src[idx] == '-') {
        idx++;

        if (not isdigit(src[idx]))
          throw std::runtime_error(
            "invalid tokens after + or - symbol in number");
      }

      while (isdigit(src[idx]))
        idx++;

      if (src[idx] == '.') {
        idx++;
        while (isdigit(src[idx]))
          idx++;
      }

      if (src[idx] == 'e') {
        idx++;
        while (isdigit(src[idx]))
          idx++;
      }

      return std::string_view{ &src[leftside_begin], &src[idx] };
    };

    while (idx < src.size()) {
      skip_whitespace();
      if (src[idx] == 0)
        break;

      switch (src[idx]) {
        case '{':
          tokens.push_back(
            { Token::Type::LeftBrace, { &src[idx], &src[++idx] } });
          break;

        case '}':
          tokens.push_back(
            { Token::Type::RightBrace, { &src[idx], &src[++idx] } });
          break;

        case '[':
          tokens.push_back(
            { Token::Type::LeftBracket, { &src[idx], &src[++idx] } });
          break;

        case ']':
          tokens.push_back(
            { Token::Type::RightBracket, { &src[idx], &src[++idx] } });
          break;

        case ':':
          tokens.push_back({ Token::Type::Colon, { &src[idx], &src[++idx] } });
          break;

        case ',':
          tokens.push_back({ Token::Type::Comma, { &src[idx], &src[++idx] } });
          break;

        case '"':
          tmp = ++idx;
          while (src[idx] != '"') {
            if (src[idx] == '\\' and src[idx + 1] == '\"')
              idx++;
            idx++;
          }
          tokens.push_back({ Token::Type::String, { &src[tmp], &src[idx] } });
          idx++;
          break;

        default: {
          if (isdigit(src[idx]) or src[idx] == '-' or src[idx] == '+')
            tokens.push_back({ Token::Type::Number, lex_number() });
          else if (isalpha(src[idx])) {
            tmp = idx;
            while (idx < src.size() and isalpha(src[idx]))
              idx++;
            std::string_view view(&src[tmp], &src[idx]);
            if (view == "true")
              tokens.push_back({ Token::Type::True, view });
            else if (view == "false")
              tokens.push_back({ Token::Type::False, view });
            else if (view == "null")
              tokens.push_back({ Token::Type::Null, view });
            else
              goto err;
          } else {
          err:
            std::string_view t{ &src[std::max<int>(idx - 10, 0)],
                                &src[std::min<int>(src.size(), idx + 10)] };
            throw std::runtime_error(
              std::format("unknown symbol found in lexer: {} {} : {}, near: {}",
                          idx,
                          (int)src[idx],
                          (char)src[idx],
                          t));
          }
        } break;
      }
    }

    return tokens;
  }

  static void debug_dump_token(auto const& token)
  {
    char const* s;
    switch (token.m_type) {
      case Token::Type::LeftBrace:
        s = "LeftBrace";
        break;
      case Token::Type::RightBrace:
        s = "RightBrace";
        break;
      case Token::Type::LeftBracket:
        s = "LeftBracket";
        break;
      case Token::Type::RightBracket:
        s = "RightBracket";
        break;
      case Token::Type::Colon:
        s = "Colon";
        break;
      case Token::Type::Comma:
        s = "Comma";
        break;
      case Token::Type::Number:
        s = "Number";
        break;
      case Token::Type::String:
        s = "String";
        break;
      case Token::Type::True:
        s = "True";
        break;
      case Token::Type::False:
        s = "False";
        break;
      case Token::Type::Null:
        s = "Null";
        break;
    }
    printf("'%.*s': %s\n", (int)token.m_span.size(), token.m_span.data(), s);
  }

  static val parse_object(std::span<Token const>& tok)
  {
    obj vals;

    while (tok.front().m_type != Token::Type::RightBrace) {
      if (tok.front().m_type != Token::Type::String)
        throw std::runtime_error(
          "expected string as field name in json object");

      std::string_view name(tok.front().m_span);
      tok = tok.subspan(1);

      if (tok.front().m_type != Token::Type::Colon)
        throw std::runtime_error(
          "expected colon after field name in json object");
      tok = tok.subspan(1);

      vals.emplace(name, parse_extern(tok));

      if (tok.front().m_type == Token::Type::Comma)
        tok = tok.subspan(1);
    }
    tok = tok.subspan(1);

    return val(std::in_place_type<obj>, vals);
  }

  static val parse_array(std::span<Token const>& tok)
  {
    std::vector<val> vals;

    while (tok.front().m_type != Token::Type::RightBracket) {
      vals.push_back(parse_extern(tok));
      if (tok.front().m_type == Token::Type::Comma)
        tok = tok.subspan(1);
    }

    // skip the right bracket
    tok = tok.subspan(1);
    return val(std::in_place_type<std::vector<val>>, vals);
  }

  static val parse_extern(std::span<Token const>& tok)
  {
    auto const front = tok.front();
    tok = tok.subspan(1);
    double d;

    switch (front.m_type) {
      case Token::Type::LeftBrace:
        return parse_object(tok);

      case Token::Type::LeftBracket:
        return parse_array(tok);

      case Token::Type::Number:
        std::from_chars(&*front.m_span.begin(), &*front.m_span.end(), d);
        return val(std::in_place_type<double>, d);

      case Token::Type::String:
        return val(std::in_place_type<std::string>,
                   std::string(front.m_span.begin(), front.m_span.end()));

      case Token::Type::False:
      case Token::Type::True:
        return val(std::in_place_type<bool>, front.m_type == Token::Type::True);

      case Token::Type::Null:
        return val(std::in_place_type<nil>);

      case Token::Type::RightBrace:
      case Token::Type::RightBracket:
      case Token::Type::Comma:
      case Token::Type::Colon:
        throw std::runtime_error(
          "unexpected symbol found while parsing json value");
    }

    std::unreachable();
  }

  struct dump_visitor
  {
    std::stringstream& m_ss;

    dump_visitor(std::stringstream& str)
      : m_ss(str) {};

    void operator()(bool b) { m_ss << (b ? "true" : "false"); }

    void operator()(double d)
    {
      std::string str;
      str = std::to_string(d);
      m_ss << str;
    }

    void operator()(std::string const& str) { m_ss << '\"' << str << '\"'; }

    void operator()(nil const&) { m_ss << "null"; }

    void operator()(array const& arr)
    {
      std::stringstream ss_2;

      ss_2 << '[';
      for (unsigned i = 0; auto const& v : arr) {
        std::visit(dump_visitor(ss_2), v);
        if (i++ != arr.size() - 1)
          ss_2 << ',';
      }

      m_ss << ss_2.str() << ']';
    }

    void operator()(obj const& map)
    {
      std::stringstream ss_2;

      ss_2 << '{';
      for (unsigned i = 0; auto const& v : map) {
        ss_2 << '\"' << v.first << "\":";
        std::visit(dump_visitor(ss_2), v.second);
        if (i++ != map.size() - 1)
          ss_2 << ',';
      }

      m_ss << ss_2.str() << '}';
    }
  };

  struct pretty_dump_visitor : dump_visitor
  {
    int m_indent;

    using dump_visitor::operator();

    pretty_dump_visitor(std::stringstream& str, int indent)
      : dump_visitor(str)
      , m_indent(indent)
    {
    }

    auto get_indent() { return m_indent * 4; }

    void operator()(array const& arr)
    {
      std::stringstream ss_2;

      m_indent++;
      ss_2 << "[\n";
      for (unsigned i = 0; auto const& v : arr) {
        ss_2 << std::setw(get_indent()) << "";
        std::visit(pretty_dump_visitor(ss_2, m_indent), v);
        if (i++ != arr.size() - 1)
          ss_2 << ',';

        ss_2 << '\n';
      }
      m_indent--;

      m_ss << ss_2.str() << std::setw(get_indent()) << "" << ']';
    }

    void operator()(obj const& map)
    {
      std::stringstream ss_2;

      m_indent++;
      ss_2 << "{\n";
      for (unsigned i = 0; auto const& v : map) {
        ss_2 << std::setw(get_indent()) << "";
        ss_2 << '\"' << v.first << '\"' << ": ";
        std::visit(pretty_dump_visitor(ss_2, m_indent), v.second);
        if (i++ != map.size() - 1)
          ss_2 << ',';
        ss_2 << '\n';
      }
      m_indent--;

      m_ss << ss_2.str() << std::setw(get_indent()) << "" << '}';
    }
  };

  friend class val;
};

val
val::parse(std::string_view src)
{
  val val;

  std::vector<_jayson_impl::Token> toks = _jayson_impl::tokenate(src);
  std::span<_jayson_impl::Token const> tok_span(toks);

  if (tok_span.size() == 0)
    return nil{};

  return _jayson_impl::parse_extern(tok_span);
}

std::string
val::serialize(bool pretty)
{
  std::stringstream ss;
  if (pretty)
    std::visit(_jayson_impl::pretty_dump_visitor(ss, 0), *this);
  else
    std::visit(_jayson_impl::dump_visitor(ss), *this);
  return ss.str();
}

};
#endif
