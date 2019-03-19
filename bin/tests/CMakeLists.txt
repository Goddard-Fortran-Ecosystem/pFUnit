find_package(Python REQUIRED)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/outputs)

file(GLOB test_cases
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/inputs
     ${CMAKE_CURRENT_SOURCE_DIR}/inputs/*.pf)
foreach( case ${test_cases})
  get_filename_component(name ${case} NAME_WE)

  set(input_file "${CMAKE_CURRENT_SOURCE_DIR}/inputs/${name}.pf")
  set(output_file "${CMAKE_CURRENT_BINARY_DIR}/outputs/${name}.F90")
  set(expected_file "${CMAKE_CURRENT_SOURCE_DIR}/expected/${name}.F90")

  get_filename_component(executable_file
                         "${CMAKE_CURRENT_SOURCE_DIR}/../funitproc" REALPATH)
  add_test(processor_test_${name} ${CMAKE_COMMAND}
           -D executable_file=${executable_file}
           -D input_file=${input_file}
           -D output_file=${output_file}
           -D expected_file=${expected_file}
           -P ${CMAKE_CURRENT_SOURCE_DIR}/run-test.cmake)
endforeach()
