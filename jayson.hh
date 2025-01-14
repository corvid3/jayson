#pragma once

#include <cctype>
#include <format>
#include <iomanip>
#include <map>
#include <span>
#include <sstream>
#include <stdexcept>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

struct jayson_nil
{};

class jayson_val;
using jayson_array = std::vector<jayson_val>;
using jayson_map = std::map<std::string, jayson_val>;
using jayson_val_impl =
  std::variant<jayson_nil, double, bool, std::string, jayson_array, jayson_map>;
class jayson_val : public jayson_val_impl
{
public:
  using jayson_val_impl::variant;
  static jayson_val parse(std::string_view);

  std::string serialize(bool pretty = false);
};

#ifdef JAYSON_IMPL

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

std::vector<Token>
tokenate(std::string_view src)
{
  std::vector<Token> tokens;
  unsigned idx = 0;
  unsigned tmp = 0;

  auto skip_whitespace = [&] {
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
          throw std::runtime_error(std::format(
            "unknown symbol found in jayson lexer: {} {} : {}, near: {}",
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

static void
debug_dump_token(auto const& token)
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

static jayson_val
parse(std::span<Token const>& tok);

static jayson_val
parse_object(std::span<Token const>& tok)
{
  std::map<std::string, jayson_val> vals;

  while (tok.front().m_type != Token::Type::RightBrace) {
    if (tok.front().m_type != Token::Type::String)
      throw std::runtime_error("expected string as field name in json object");

    std::string_view name(tok.front().m_span);
    tok = tok.subspan(1);

    if (tok.front().m_type != Token::Type::Colon)
      throw std::runtime_error(
        "expected colon after field name in json object");
    tok = tok.subspan(1);

    vals.emplace(name, parse(tok));

    if (tok.front().m_type == Token::Type::Comma)
      tok = tok.subspan(1);
  }
  tok = tok.subspan(1);

  return jayson_val(std::in_place_type<jayson_map>, vals);
}

static jayson_val
parse_array(std::span<Token const>& tok)
{
  std::vector<jayson_val> vals;

  while (tok.front().m_type != Token::Type::RightBracket) {
    vals.push_back(parse(tok));
    if (tok.front().m_type == Token::Type::Comma)
      tok = tok.subspan(1);
  }

  // skip the right bracket
  tok = tok.subspan(1);
  return jayson_val(std::in_place_type<std::vector<jayson_val>>, vals);
}

static jayson_val
parse(std::span<Token const>& tok)
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
      return jayson_val(std::in_place_type<double>, d);

    case Token::Type::String:
      return jayson_val(std::in_place_type<std::string>,
                        std::string(front.m_span.begin(), front.m_span.end()));

    case Token::Type::False:
    case Token::Type::True:
      return jayson_val(std::in_place_type<bool>,
                        front.m_type == Token::Type::True);

    case Token::Type::Null:
      return jayson_val(std::in_place_type<jayson_nil>);

    case Token::Type::RightBrace:
    case Token::Type::RightBracket:
    case Token::Type::Comma:
    case Token::Type::Colon:
      throw std::runtime_error(
        "unexpected symbol found while parsing json value");
  }

  std::unreachable();
}

jayson_val
jayson_val::parse(std::string_view src)
{
  jayson_val val;

  std::vector<Token> toks = tokenate(src);
  std::span<Token const> tok_span(toks);
  return ::parse(tok_span);
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

  void operator()(jayson_nil const& nil) { m_ss << "null"; }

  void operator()(jayson_array const& arr)
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

  void operator()(jayson_map const& map)
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

  void operator()(jayson_array const& arr)
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

  void operator()(jayson_map const& map)
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

std::string
jayson_val::serialize(bool pretty)
{
  std::stringstream ss;
  if (pretty)
    std::visit(pretty_dump_visitor(ss, 0), *this);
  else
    std::visit(dump_visitor(ss), *this);
  return ss.str();
}

#endif // JAYSON_IMPL
