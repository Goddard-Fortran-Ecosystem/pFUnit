file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/outputs)

file(GLOB test_cases
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/inputs
     ${CMAKE_CURRENT_SOURCE_DIR}/inputs/*.pf)
foreach(case IN LISTS test_cases)
  get_filename_component(name ${case} NAME_WE)

  set(input_file "${CMAKE_CURRENT_SOURCE_DIR}/inputs/${name}.pf")
  set(output_file "${CMAKE_CURRENT_BINARY_DIR}/outputs/${name}.F90")
  set(expected_file "${CMAKE_CURRENT_SOURCE_DIR}/expected/${name}.F90")

  get_filename_component(executable_file
                         "${CMAKE_CURRENT_SOURCE_DIR}/../funitproc" REALPATH)
  add_test(NAME processor_test_${name}
           COMMAND ${CMAKE_COMMAND}
           -DPython_EXECUTABLE=${Python_EXECUTABLE}
           -Dexecutable_file=${executable_file}
           -Dinput_file=${input_file}
           -Doutput_file=${output_file}
           -Dexpected_file=${expected_file}
           -P ${CMAKE_CURRENT_SOURCE_DIR}/run-test.cmake)
endforeach()
