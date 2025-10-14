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
#                   LABELS ...
#                   MAX_PES 5
#                   WORKING_DIRECTORY working_directory
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

function (add_pfunit_ctest test_package_name)
  set (oneValueArgs REGISTRY MAX_PES EXTRA_USE EXTRA_INITIALIZE EXTRA_FINALIZE WORKING_DIRECTORY)
  set (multiValueArgs TEST_SOURCES OTHER_SOURCES LINK_LIBRARIES LABELS)
  cmake_parse_arguments (PF_TEST "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  set (test_sources_f90)
  set (test_suites_inc "")

  # Create contents of the test_suites files
  foreach (pf_file ${PF_TEST_TEST_SOURCES})
    get_filename_component (basename ${pf_file} NAME_WE)
    set (test_suites_inc "${test_suites_inc}ADD_TEST_SUITE(${basename}_suite)\n")

  endforeach()

  # Preprocess test files
  # F90 files are set in test_sources_f90
  add_pfunit_sources(test_sources_f90 ${PF_TEST_TEST_SOURCES})

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

  # LLVM Flang apparently does not like the <...> syntax for includes?
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "LLVMFlang")
    target_compile_definitions (${test_package_name} PRIVATE _TEST_SUITES="${test_suite_inc_file}")
  else()
    target_compile_definitions (${test_package_name} PRIVATE _TEST_SUITES=\<${test_suite_inc_file}\>)
  endif()

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

  if (PF_TEST_WORKING_DIRECTORY)
    set(workdir ${PF_TEST_WORKING_DIRECTORY})
  else ()
    set(workdir ${CMAKE_CURRENT_BINARY_DIR})
  endif ()

  #################################################
  # Define test in CTest system                   #
  #################################################
  if (PF_TEST_MAX_PES AND PFUNIT_MPI_FOUND)
    target_link_libraries (${test_package_name} ${PFUNIT_LIBRARIES})
    if (NOT MPIEXEC_EXECUTABLE)
      if (PFUNIT_MPI_USE_MPIEXEC)
        set(MPIEXEC_EXECUTABLE ${PFUNIT_MPI_USE_MPIEXEC})
      else() # best guess
        set(MPIEXEC_EXECUTABLE mpirun)
      endif()
    endif()
    if (NOT MPIEXEC_NUMPROC_FLAG)
      if (PFUNIT_MPI_USE_MPIEXEC)
        set(MPIEXEC_EXECUTABLE ${PFUNIT_MPI_USE_MPIEXEC})
      else() # best guess
        set(MPIEXEC_NUMPROC_FLAG "-np")
      endif()
    endif()
    if (MPIEXEC_EXECUTABLE MATCHES ".*openmpi*")
      list(APPEND MPIEXEC_PREFLAGS "--oversubscribe")
    endif()
    add_test (NAME ${test_package_name}
      WORKING_DIRECTORY ${workdir}
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${PF_TEST_MAX_PES} ${CMAKE_CURRENT_BINARY_DIR}/${test_package_name} --verbose
      )
  else()
    target_link_libraries (${test_package_name} ${PFUNIT_SERIAL_LIBRARIES})
    add_test (NAME ${test_package_name}
      WORKING_DIRECTORY ${workdir}
      COMMAND ${test_package_name} --verbose
      )
  endif()

  set_property (TEST ${test_package_name}
    PROPERTY FAIL_REGULAR_EXPRESSION "Encountered 1 or more failures/errors during testing"
    )

  if (PF_TEST_LABELS)
    set_property (TEST ${test_package_name}
      PROPERTY LABELS ${PF_TEST_LABELS}
    )
  endif()

endfunction()
