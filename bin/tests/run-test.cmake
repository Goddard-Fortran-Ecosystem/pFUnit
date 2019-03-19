execute_process(COMMAND ${executable_file} ${input_file} ${output_file}
                RESULT_VARIABLE proc_rc
                ERROR_VARIABLE proc_err OUTPUT_VARIABLE proc_err)
if(NOT ${proc_rc} EQUAL 0)
  message(SEND_ERROR "Failed to run processor: (${proc_rc}) ${proc_err}")
  return(${proc_rc})
endif()

message(STATUS "tool ${executable_file}")

execute_process(COMMAND ${CMAKE_COMMAND} -E compare_files
                        ${output_file} ${expected_file}
                RESULT_VARIABLE compare_rc)
if(NOT ${compre_rc} EQUAL 0)
  message(SEND_ERROR "${output_file} differs from ${expected_file}")
  return(1)
endif()
