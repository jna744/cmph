cmake_minimum_required(VERSION 3.26)
project(cmph)

set(CMAKE_CXX_STANDARD 20)

add_library(cmph INTERFACE
        include/cmph/cmph.hpp)

target_include_directories(cmph INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
)

find_package(benchmark REQUIRED)

add_executable(main main.cpp)
target_link_libraries(main cmph)

add_executable(bench bench.cpp)
target_link_libraries(bench benchmark::benchmark cmph)
