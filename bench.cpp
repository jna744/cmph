#include <benchmark/benchmark.h>

#include <cmph/cmph.hpp>

using namespace std::string_view_literals;

static auto keys = std::array{"One"sv, "Two"sv,   "Three"sv, "Four"sv, "Five"sv,
                              "Six"sv, "Seven"sv, "Eight"sv, "Nine"sv, "Ten"sv};

static void cmph_Map(benchmark::State& state)
{
  using namespace cmph::kvp_literals;

  constexpr auto map =
      cmph::makeMap<int>("One"_kvp(1), "Two"_kvp(2), "Three"_kvp(3), "Four"_kvp(4), "Five"_kvp(5),
                         "Six"_kvp(6), "Seven"_kvp(7), "Eight"_kvp(8));

  for (auto _ : state)
    for (auto k : keys)
    {
      bool v = map.contains(k);
      benchmark::DoNotOptimize(v);
    }
}
BENCHMARK(cmph_Map);

static void std_map(benchmark::State& state)
{
  auto const map = std::map<std::string_view, int>({{"One", 1},
                                                    {"Two", 2},
                                                    {"Three", 3},
                                                    {"Four", 4},
                                                    {"Five", 5},
                                                    {"Six", 6},
                                                    {"Seven", 7},
                                                    {"Eight", 8}}

  );
  for (auto _ : state)
    for (auto k : keys)
    {
      bool v = map.contains(k);
      benchmark::DoNotOptimize(v);
    }
}
BENCHMARK(std_map);

static void std_unordered_map(benchmark::State& state)
{
  auto const map = std::unordered_map<std::string_view, int>({{"One", 1},
                                                              {"Two", 2},
                                                              {"Three", 3},
                                                              {"Four", 4},
                                                              {"Five", 5},
                                                              {"Six", 6},
                                                              {"Seven", 7},
                                                              {"Eight", 8}});
  for (auto _ : state)
    for (auto k : keys)
    {
      bool v = map.contains(k);
      benchmark::DoNotOptimize(v);
    }
}
BENCHMARK(std_unordered_map);

BENCHMARK_MAIN();
