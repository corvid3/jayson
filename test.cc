#include <tuple>
#define JAYSON_IMPL

#include "jayson.hh"
#include <fstream>
#include <iostream>
#include <sstream>

struct inner
{
  int x;
  std::tuple<int, std::string> y;

  using jayson_fields = std::tuple<jayson::obj_field<"mi", &inner::x>,
                                   jayson::obj_field<"lo", &inner::y>>;
};

struct test
{
  int x;
  std::string b;
  std::vector<inner> in;

  using jayson_fields = std::tuple<jayson::obj_field<"x", &test::x>,
                                   jayson::obj_field<"boogus", &test::b>,
                                   jayson::obj_field<"in", &test::in>>;
};

int
main()
{
  test m;

  auto const js = jayson::val::parse(
    R"({ "x": 1, "boogus": "m", "in": [{"mi": 2, "lo": [1, "hi"]}] })");

  jayson::deserialize(js, m);

  std::cout << std::format("{}, {}", m.x, m.b);

  auto ser = jayson::serialize(m);

  test m2;

  jayson::deserialize(ser, m2);

  std::cout << std::format("{}, {}", m2.x, m2.b);
}
