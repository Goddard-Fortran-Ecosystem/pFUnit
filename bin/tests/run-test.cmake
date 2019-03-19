set(input_file "${CMAKE_CURRENT_SOURCE_DIR}/inputs/${name}.pf")
set(output_file "${CMAKE_CURRENT_BINARY_DIR}/outputs/${name}.F90")
set(expected_file "${CMAKE_CURRENT_SOURCE_DIR}/expected/${name}.F90")

execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/../funitproc
                        ${input_file} ${output_file}
                RESULT_VARIABLE proc_rc
                ERROR_VARIABLE proc_err OUTPUT_VARIABLE proc_err)
if(NOT ${proc_rc} EQUAL 0)
  message(SEND_ERROR "Failed to run processor: (${proc_rc})${proc_err}")
  return(${proc_rc})
endif()

execute_process(COMMAND ${CMAKE_COMMAND} -E compare_files
                        ${output_file} ${expected_file}
                RESULT_VARIABLE compare_rc)
if(NOT ${compre_rc} EQUAL 0)
  message(SEND_ERROR "${output_file} differs from ${expected_file}")
  return(1)
endif()
