add_subdirectory(funit)
add_subdirectory(tests)

install(PROGRAMS funitproc DESTINATION ${dest}/bin)

add_test(NAME unit_test_processor
         COMMAND ${Python_EXECUTABLE} -m unittest discover
         --start-directory=${CMAKE_CURRENT_SOURCE_DIR})
