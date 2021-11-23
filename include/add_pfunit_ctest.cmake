# Function     : add_pfunit_ctest
#
# Description : Helper function for compiling and adding pFUnit tests
#               to the CTest testing framework. Any libraries needed
#               in testing should be linked to manually.
#
# Arguments    : - test_name: Name of the test package
#
# Example usage: enable_testing()
#                add_pfunit_ctest (myTests
#                   TEST_SOURCES testMyLib.pf
#                   OTHER_SOURCES other.F90 yet_another.c
#                   REGISTRY test_suites.inc
#                   LINK_LIBRARIES mylib
#                   EXTRA_USE ...
#                   EXTRA_INITIALIZE ...
#                   EXTRA_FINALIZE ...
#                   MAX_PES 5
#                   SKIP_ADD_TEST 0  
#                   )
#
# TEST_SOURCES items with relative paths are treated as relative to
# CMAKE_CURRENT_SOURCE_DIR, and the processed file is placed into a
# file with the same basename but relative to
# CMAKE_CURRENT_BINARY_DIR.  (And with .F90 suffix, of course.)
#
# TEST_SOURCES items with absolute paths are trated as being within
# the build tree, and the processed file is placed in the same directory.
# (And with the .F90 suffix, of course.)
#
# Note: If REGISTRY is not provided, then a default testSuites.inc
#       will be created based on the PFUNIT_SOURCES file names.  It is
#       assumed that the module name is the same as the file basename.
#       For example, the file testSomething.pf should contain the
#       module testSomething.
#
#
#
#
# Compile the tests:   make myTests
# Run the tests with CTest: ctest -R myTests --verbose
#

include (add_pfunit_sources)

if (PFUNIT_BUILD_SHARED AND BUILD_SHARED_LIBS)
  set(_PFUNIT_LIBRARIES PFUNIT::pfunit_shared)
  set(_FUNIT_LIBRARIES PFUNIT::funit_shared)
else()
  set(_PFUNIT_LIBRARIES PFUNIT::pfunit)
  set(_FUNIT_LIBRARIES PFUNIT::funit)
endif()

function (add_pfunit_ctest test_package_name)
  set (oneValueArgs REGISTRY SKIP_ADD_TEST MAX_PES EXTRA_USE EXTRA_INITIALIZE EXTRA_FINALIZE)
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

  
  if (PF_TEST_EXTRA_USE)
    set(PFUNIT_EXTRA_USE ${PF_TEST_EXTRA_USE})
  endif()
  set(driver "${test_package_name}_driver.F90")
  configure_file(${PFUNIT_DRIVER}.in ${driver})

  add_executable (${test_package_name}
    ${test_sources_f90}
    ${PF_TEST_OTHER_SOURCES}
    ${driver}
    )
  set (mod_dir ${CMAKE_CURRENT_BINARY_DIR}/mod/${test_package_name})
  set_target_properties (${test_package_name} PROPERTIES Fortran_MODULE_DIRECTORY ${mod_dir})
  target_include_directories(${test_package_name} PRIVATE ${mod_dir})

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
  if (PF_TEST_MAX_PES AND NOT PFUNIT_SKIP_MPI)
    target_link_libraries (${test_package_name} ${_PFUNIT_LIBRARIES})
    if (NOT PFUNIT_MPI_USE_MPIEXEC)
      set(MPIEXEC mpirun)
      set(MPIEXEC_NUMPROC_FLAG "-np")
    endif()
    if (MPIEXEC MATCHES ".*openmpi*")
      list(APPEND MPIEXEC_PREFLAGS "--oversubscribe")
    endif()
    if (NOT PF_TEST_SKIP_ADD_TEST)
      add_test (NAME ${test_package_name}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${PF_TEST_MAX_PES} ${CMAKE_CURRENT_BINARY_DIR}/${test_package_name}
        )
    endif (NOT PF_TEST_SKIP_ADD_TEST)
  else()
    target_link_libraries (${test_package_name} ${_FUNIT_LIBRARIES})
    if (NOT PF_TEST_SKIP_ADD_TEST)
      add_test (NAME ${test_package_name}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${test_package_name}
        )
    endif (NOT PF_TEST_SKIP_ADD_TEST)
  endif()

  if (NOT PF_TEST_SKIP_ADD_TEST)
    set_property (TEST ${test_package_name}
      PROPERTY FAIL_REGULAR_EXPRESSION "Encountered 1 or more failures/errors during testing"
      )
  endif (NOT PF_TEST_SKIP_ADD_TEST)
endfunction()
