find_package(Boost COMPONENTS unit_test_framework)

if (NOT Boost_FOUND)
    message(WARNING "CMPH testing enabled, but boost unit_test_framework not found. Runtime tests skipped.")
else()
    add_executable(cmph_tests tests.cpp)
    target_link_libraries(cmph_tests PUBLIC cmph Boost::unit_test_framework)
    add_test(NAME cmph_tests COMMAND cmph_tests)
    set_property(TARGET cmph_tests PROPERTY CXX_STANDARD 20)
endif()

add_executable(cmph_constexpr_tests constexpr_tests.cpp)
set_property(TARGET cmph_constexpr_tests PROPERTY CXX_STANDARD 20)