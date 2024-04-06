#define P(MSG) std::cout << MSG << std::endl

#include <iostream>

#include <cmph/cmph.hpp>

struct LIFETIME
{
  LIFETIME() { P("LIFETIME()"); }
  ~LIFETIME() { P("~LIFETIME()"); }
  LIFETIME(LIFETIME const&) noexcept { P("LIFETIME(LIFETIME CONST&)"); }
  LIFETIME(LIFETIME&&) noexcept { P("LIFETIME(LIFETIME&&)"); }
};

int main()
{
  using namespace cmph::kvp_literals;

  auto map = cmph::makeMap<int>("One"_kvp(1), "Two"_kvp(2), "Three"_kvp(3), "Four"_kvp(4),
                                "Five"_kvp(5), "Six"_kvp(6), "Seven"_kvp(7), "Eight"_kvp(8));

  std::cout << map.find("One")->second << std::endl;
  std::cout << map.find("Two")->second << std::endl;
  std::cout << map.find("Three")->second << std::endl;
  std::cout << map.find("Four")->second << std::endl;
  std::cout << map.find("Five")->second << std::endl;
  std::cout << map.find("Six")->second << std::endl;
  std::cout << map.find("Seven")->second << std::endl;
  std::cout << map.find("Eight")->second << std::endl;
}
