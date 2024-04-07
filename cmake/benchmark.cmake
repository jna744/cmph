include(FetchContent)

fetchcontent_declare(
        benchmark
        GIT_REPOSITORY https://github.com/google/benchmark.git
        GIT_TAG 3441176 # v 1.8.3
)

fetchcontent_getproperties(Benchmark)
if (NOT benchmark_POPULATED)
    fetchcontent_populate(benchmark)
    set(BENCHMARK_ENABLE_TESTING Off)
    add_subdirectory(${benchmark_SOURCE_DIR} ${benchmark_BINARY_DIR} EXCLUDE_FROM_ALL)
endif ()