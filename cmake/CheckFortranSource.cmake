function (CHECK_FORTRAN_SOURCE_RUN file var)
  if (NOT DEFINED ${var})
    try_run (
      run compile
      ${CMAKE_BINARY_DIR}
      ${file}
      CMAKE_FLAGS "-DCOMPILE_DEFINITIONS=${CMAKE_REQUIRED_DEFINITIONS}"
      RUN_OUTPUT_VARIABLE ${var}
      )

    # Successful runs return "0", which is opposite of CMake sense of "if":
    if (NOT compile OR run)
      if (NOT CMAKE_REQUIRED_QUIET)
        message(STATUS "Performing Test ${var}: FAILURE")
      endif ()
      return()
    endif()

    string(STRIP "${${var}}" ${var})
    if (NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "Performing Test ${var}: SUCCESS (value=${${var}})")
    endif ()
    set(${var} ${${var}} CACHE STRING "" FORCE)

  endif()
  add_definitions(-D${var}=${${var}})
endfunction (CHECK_FORTRAN_SOURCE_RUN)


function (CHECK_FORTRAN_SOURCE_COMPILE file var)
  if (NOT DEFINED ${var})
    try_compile (
      code_compiles
      ${CMAKE_BINARY_DIR}
      ${file}
      CMAKE_FLAGS "-DCOMPILE_DEFINITIONS=${CMAKE_REQUIRED_DEFINITIONS}"
      )

    if (NOT ${code_compiles})
      if (NOT CMAKE_REQUIRED_QUIET)
        message (STATUS "Performing Test ${var}: BUILD FAILURE")
      endif ()
      return()
    endif()

    set(${var} SUCCESS)
    if (NOT CMAKE_REQUIRED_QUIET)
      message (STATUS "Performing Test ${var}: SUCCESS")
    endif ()

    set(${var} ${${var}} CACHE STRING "" FORCE)
  endif()
  add_definitions(-D${var})
endfunction (CHECK_FORTRAN_SOURCE_COMPILE)
