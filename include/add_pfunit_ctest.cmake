# Function     : add_pfunit_ctest 
#
# Description : Helper function for compiling and adding pFUnit tests
#               to the CTest testing framework. Any libraries needed
#               in testing should be linked to manually.
#
# Arguments    : - test_name: Name of the test package
#
# Example usage: enable_testing()
#                add_pfunit_test (myTests
#                   TEST_SOURCES testMyLib.pf
#                   OTHER_SOURCES other.F90 yet_another.c
#                   REGISTRY test_suites.inc             
#                   LINK_LIBRARIES mylib
#                   EXTRA_USE ...
#                   EXTRA_INITIALIZE ...
#                   EXTRA_FINALIZE ...
#                   MAX_PES 5
#                   )
#
# Note: If REGISTRY is not provided, then a default testSuites.inc
#       will be created based on the PFUNIT_SOURCES file names.  It is
#       assumed that the module name is the same as the file basename.
#       For example, the file testSomething.pf should contain the
#       module testSomething.
#                
#
# Compile the tests:   make myTests
# Run the tests with CTest: ctest -R myTests --verbose
#

include (add_pfunit_sources)

function (add_pfunit_ctest test_package_name)
  set (oneValueArgs REGISTRY MAX_PES EXTRA_USE EXTRA_INITIALIZE EXTRA_FINALIZE)
  set (multiValueArgs TEST_SOURCES OTHER_SOURCES LINK_LIBRARIES)
  cmake_parse_arguments (PF_TEST "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  set (test_sources_f90)
  set (test_suites_inc "")
  foreach (pf_file ${PF_TEST_TEST_SOURCES})

    get_filename_component (basename ${pf_file} NAME_WE)
    set (f90_file "${basename}.F90")
    list (APPEND test_sources_f90 ${f90_file})
    set (test_suites_inc "${test_suites_inc}ADD_TEST_SUITE(${basename}_suite)\n")
    add_pfunit_sources(test_sources_f90 ${PF_TEST_TEST_SOURCES})

  endforeach()

  add_executable (${test_package_name}
    ${test_sources_f90}
    ${PF_TEST_OTHER_SOURCES}
    ${PFUNIT_DRIVER}
    )

  if (PF_TEST_REGISTRY)
    set (test_suite_inc_file ${PF_TEST_REGISTRY})
  else ()
    set (should_write_inc_file True)
    set (test_suite_inc_file "${CMAKE_CURRENT_BINARY_DIR}/${test_package_name}.inc")
    # Check if .inc file already has been generated. If so, only write new
    # file if contents has changed. This avoid tests recompiling after reconfiguring cmake.
    if (EXISTS ${test_suite_inc_file})
      file (READ ${test_suite_inc_file} existing_file)
      if (${existing_file} STREQUAL ${test_suites_inc})
	set (should_write_inc_file False)
      endif (${existing_file} STREQUAL ${test_suites_inc})
    endif ()

    if (${should_write_inc_file})
      file (WRITE ${test_suite_inc_file} ${test_suites_inc})
    endif (${should_write_inc_file})
  endif ()

  target_compile_definitions (${test_package_name} PRIVATE -D_TEST_SUITES="${test_suite_inc_file}")
  target_include_directories (${test_package_name} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

  if (PF_TEST_EXTRA_USE)
    target_compile_definitions (${test_package_name} PRIVATE -DPFUNIT_EXTRA_USE=${PF_TEST_EXTRA_USE})
  endif()

  if (PF_TEST_EXTRA_INITIALIZE)
    target_compile_definitions (${test_package_name} PRIVATE -DPFUNIT_EXTRA_INITIALIZE=${PF_TEST_EXTRA_INITIALIZE})
  endif()

  if (PF_TEST_EXTRA_FINALIZE)
    target_compile_definitions (${test_package_name} PRIVATE -DPFUNIT_EXTRA_FINALIZE=${PF_TEST_EXTRA_FINALIZE})
  endif()
  
  if (PF_TEST_LINK_LIBRARIES)
    target_link_libraries (${test_package_name} ${PF_TEST_LINK_LIBRARIES})
  endif ()

  #################################################
  # Define test in CTest system                   #
  #################################################
  if (PF_TEST_MAX_PES)
    target_link_libraries (${test_package_name} pfunit)
    if (MPIEXEC MATCHES ".*openmpi*")
      list(APPEND MPIEXEC_PREFLAGS "--oversubscribe")
    endif()
    add_test (NAME ${test_package_name}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMAND mpirun ${MPIEXEC_PREFLAGS} -np ${PF_TEST_MAX_PES} ${test_package_name}
      )
  else()
    target_link_libraries (${test_package_name} funit)
    add_test (NAME ${test_package_name}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMAND ${test_package_name}
      )
  endif()
  
  set_property (TEST ${test_package_name}
    PROPERTY FAIL_REGULAR_EXPRESSION "Encountered 1 or more failures/errors during testing"
    )

endfunction()
