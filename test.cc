#include <optional>
#include <tuple>
#define JAYSON_IMPL

#include "jayson.hh"
#include <fstream>
#include <iostream>
#include <sstream>

struct inner
{
  int x;
  std::array<int, 2> y;

  using jayson_fields = std::tuple<jayson::obj_field<"mi", &inner::x>,
                                   jayson::obj_field<"lo", &inner::y>>;
};

struct test
{
  std::optional<int> x;
  std::string b;
  std::vector<inner> in;
  std::map<std::string, int> map;

  using jayson_fields = std::tuple<jayson::obj_field<"x", &test::x>,
                                   jayson::obj_field<"boogus", &test::b>,
                                   jayson::obj_field<"in", &test::in>,
                                   jayson::obj_field<"map", &test::map>>;
};

enum class Foo
{
  Bar,
  Baz
};

struct FooDescriptor
{
  static std::optional<Foo> deserialize(std::string_view what)
  {
    return Foo::Bar;
  }

  static std::string serialize(Foo what) { return "bar"; }
};

struct t2
{
  // std::set<int> m;
  Foo m;

  using jayson_fields =
    std::tuple<jayson::enum_field<"m", &t2::m, FooDescriptor>>;
  // static constexpr bool jayson_explicitly_constructible = true;
};

int
main()
{
  test m;

  t2 s;
  s = jayson::deserialize<t2>(jayson::val::parse(R"({"m": "bar"})"));
  jayson::serialize(s);

  // auto const js = jayson::val::parse(
  //   R"({ "x": 1, "boogus": "m", "in": [{"mi": 2, "lo": [1, "hi"]}] })");

  // jayson::deserialize(js, m);

  // std::cout << std::format("{}, {}", *m.x, m.b);

  // auto ser = jayson::serialize(m);

  // test m2;

  // jayson::deserialize(ser, m2);

  // std::cout << std::format("{}, {}", *m2.x, m2.b);
}
