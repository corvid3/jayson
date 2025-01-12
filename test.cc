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
  printf("%s\n", ss.str().data());

  jayson_val value = jayson_val::parse(ss.str());
  auto dump = value.debug_dump();
  printf("%s\n", dump.c_str());
}
