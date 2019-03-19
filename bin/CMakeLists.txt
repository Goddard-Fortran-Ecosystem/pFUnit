find_package(Python REQUIRED)

add_subdirectory(tests)

install(PROGRAMS pFUnitParser.py parseDirectiveArgs.py DESTINATION ${dest}/bin)

add_test(unit_test_processor
         python -m unittest discover
         --start-directory=${CMAKE_CURRENT_SOURCE_DIR})
