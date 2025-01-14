#define JAYSON_IMPL

#include "jayson.hh"
#include <fstream>
#include <iostream>
#include <sstream>

void
open(auto path)
{
  std::ifstream data(path);
  std::stringstream ss;
  ss << data.rdbuf();

  jayson_val value = jayson_val::parse(ss.str());
}

int
main()
{
  open("./twitter.json");
  open("./canada.json");
  open("./citm_catalog.json");
}
