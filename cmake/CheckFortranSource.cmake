macro (CHECK_FORTRAN_SOURCE_COMPILE file var)

  if (NOT CMAKE_REQUIRED_QUIET)
    message (STATUS "Performing Test ${var}")
  endif ()

if (${ARGN})
  try_compile (
    ${var}
    ${CMAKE_BINARY_DIR}
    ${file}
    CMAKE_FLAGS "-DCOMPILE_DEFINITIONS=${CMAKE_REQUIRED_DEFINITIONS}"
    )
else ()
  try_compile (
    ${var}
    ${CMAKE_BINARY_DIR}
    ${file}
    CMAKE_FLAGS "-DCOMPILE_DEFINITIONS=${CMAKE_REQUIRED_DEFINITIONS}"
    )
endif ()

  if (${var})
    if (NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "Performing Test ${var}: SUCCESSS")
    endif ()

    add_definitions(-D${var})

  else ()

    if (NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "Performing Test ${var}: FAILURE")
    endif ()

  endif ()

endmacro (CHECK_FORTRAN_SOURCE_COMPILE)


macro (CHECK_FORTRAN_SOURCE_RUN file var)

  if (NOT CMAKE_REQUIRED_QUIET)
    message (STATUS "Performing Test ${var}")
  endif ()

  try_run (
    code_runs
    code_compiles
    ${CMAKE_BINARY_DIR}
    ${file}
    )

  if (${code_compiles})
    if (${code_runs} EQUAL 0)

      if (NOT CMAKE_REQUIRED_QUIET)
	message (STATUS "Performing Test ${var}: SUCCESS")
      endif ()

      add_definitions(-D${var})

      set (${var} 1)

    else ()

      if (NOT CMAKE_REQUIRED_QUIET)
	message (STATUS "Performing Test ${var}: RUN FAILURE")
      endif ()

    endif ()

  else ()

      if (NOT CMAKE_REQUIRED_QUIET)
	message (STATUS "Performing Test ${var}: BUILD FAILURE")
      endif ()

  endif()

endmacro (CHECK_FORTRAN_SOURCE_RUN)
