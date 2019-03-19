find_package(Python REQUIRED)

add_subdirectory(funit)
add_subdirectory(tests)

install(PROGRAMS funitproc DESTINATION ${dest}/bin)

add_test(unit_test_processor
         python -m unittest discover
         --start-directory=${CMAKE_CURRENT_SOURCE_DIR})
