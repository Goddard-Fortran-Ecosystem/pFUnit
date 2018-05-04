macro (CHECK_FORTRAN_SOURCE_RUN file var)

  try_run (
    run compile
    ${CMAKE_BINARY_DIR}
    ${file}
    CMAKE_FLAGS "-DCOMPILE_DEFINITIONS=${CMAKE_REQUIRED_DEFINITIONS}"
    RUN_OUTPUT_VARIABLE ${var}
    )

  # Successful runs return "0", which is opposite of CMake sense of "if":
  if (NOT run)
    string(STRIP ${${var}} ${var})
    if (NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "Performing Test ${var}: SUCCESS (value=${${var}})")
    endif ()
    
    add_definitions(-D${var}=${${var}})
    
  else ()
    
    if (NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "Performing Test ${var}: FAILURE")
    endif ()
    
  endif ()

endmacro (CHECK_FORTRAN_SOURCE_RUN)


macro (CHECK_FORTRAN_SOURCE_COMPILE file var)

  try_compile (
    code_compiles
    ${CMAKE_BINARY_DIR}
    ${file}
    CMAKE_FLAGS "-DCOMPILE_DEFINITIONS=${CMAKE_REQUIRED_DEFINITIONS}"
    )

  if (${code_compiles})

    set(${var} SUCCESS)
    if (NOT CMAKE_REQUIRED_QUIET)
      message (STATUS "Performing Test ${var}: SUCCESS")
    endif ()

    add_definitions(-D${var})

  else ()

      if (NOT CMAKE_REQUIRED_QUIET)
	message (STATUS "Performing Test ${var}: BUILD FAILURE")
      endif ()

  endif()

endmacro (CHECK_FORTRAN_SOURCE_COMPILE)
