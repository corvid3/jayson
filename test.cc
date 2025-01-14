#define JAYSON_IMPL

#include "jayson.hh"
#include <fstream>
#include <iostream>
#include <sstream>

int
main()
{
  std::ifstream data("./test.json");
  std::stringstream ss;
  ss << data.rdbuf();

  jayson_val value = jayson_val::parse(ss.str());
  auto dump = value.serialize();
  jayson_val value_reparsed = jayson_val::parse(dump);
  // printf("%s\n", dump.c_str());
}
