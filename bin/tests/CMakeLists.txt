find_package(Python REQUIRED)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/outputs)

file(GLOB test_cases
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/inputs
     ${CMAKE_CURRENT_SOURCE_DIR}/inputs/*.pf)
foreach( case ${test_cases})
  get_filename_component(name ${case} NAME_WE)
  add_test(processor_test_${name}
           ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_SOURCE_DIR}/run-test.cmake
           -Dname=${name})
endforeach()
