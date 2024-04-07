cmake_minimum_required(VERSION 3.13)
project(cmph)

option(CMPH_TEST "Build cmph tests" On) # Todo
option(CMPH_BENCH "Build cmph benchmark" On)

add_library(cmph INTERFACE
        include/cmph/cmph.hpp)

target_include_directories(cmph INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
)

if (CMPH_TEST)
    enable_testing()
    add_subdirectory(test)
endif()

if (CMPH_BENCH)
    find_package(benchmark)
    if (NOT benchmark_FOUND)
        include(cmake/benchmark.cmake)
    endif()
    add_executable(bench bench.cpp)
    target_link_libraries(bench benchmark::benchmark cmph)
    set_property(TARGET bench PROPERTY CXX_STANDARD 20)
endif()


