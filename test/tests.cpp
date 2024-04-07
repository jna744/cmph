#define BOOST_TEST_MODULE CMPH tests
#define BOOST_TEST_DYN_LINK
#include <boost/test/unit_test.hpp>
#include <cmph/cmph.hpp>

using namespace cmph;

template <typename Policy = DefaultPolicy>
static auto create()
{
  return makeMap<int>("One"_kvp(1), "Two"_kvp(2), "Three"_kvp(3), "Four"_kvp(4), "Five"_kvp(5),
                      "Six"_kvp(6), "Seven"_kvp(7), "Eight"_kvp(8));
}

BOOST_AUTO_TEST_CASE(contains)
{
  auto map = create();
  BOOST_CHECK(map.contains("One"));
  BOOST_CHECK(map.contains("Eight"));
  BOOST_CHECK(!map.contains("Nine"));
}

BOOST_AUTO_TEST_CASE(at)
{
  auto map = create();
  BOOST_CHECK(map.at("One") == 1);
  BOOST_CHECK(map.at("Eight") == 8);
}

BOOST_AUTO_TEST_CASE(at_exception)
{
  auto map = create();
  bool thrown = false;
  try
  {
    int& value = map.at("Nine");
  }
  catch (std::out_of_range const&)
  {
    thrown = true;
  }
  BOOST_CHECK(thrown);
}