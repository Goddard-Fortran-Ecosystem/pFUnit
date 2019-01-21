include_directories (${pFUnit_SOURCE_DIR}/include)
include_directories (${pFUnit_BINARY_DIR}/src)
include_directories(${CMAKE_BINARY_DIR}/mod)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})



set (PARALLEL_TEST_SRCS)
list(APPEND PARALLEL_TEST_SRCS Test_MpiContext.F90)
list(APPEND PARALLEL_TEST_SRCS Test_MpiException.F90)
list(APPEND PARALLEL_TEST_SRCS Test_MpiTestCase.F90)
list(APPEND PARALLEL_TEST_SRCS Test_MpiParameterizedTestCase.F90)

set (pf_tests
  Test_MPI_stub.pf
  Test_MpiHamcrest.pf
  )

set(new_tests)
foreach (file ${pf_tests})
  get_filename_component(basename ${file} NAME_WE)
  set(f90_src ${basename}.F90)
  add_custom_command (
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${f90_src}
    COMMAND python
    ARGS ${CMAKE_SOURCE_DIR}/bin/pFUnitParser.py ${CMAKE_CURRENT_SOURCE_DIR}/${file} ${f90_src}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${file}
    )
  list(APPEND new_tests ${f90_src})
endforeach()



add_library(pfunittests EXCLUDE_FROM_ALL STATIC ${PARALLEL_TEST_SRCS})
target_link_libraries(pfunittests pfunit funit)

add_executable (parallel_tests.x EXCLUDE_FROM_ALL parallel_tests.F90)
target_link_libraries(parallel_tests.x pfunittests pfunit funit ${MPI_Fortran_LIBRARIES})

add_library(new_ptests EXCLUDE_FROM_ALL ${new_tests})
target_link_libraries(new_ptests pfunit hamcrest funit ${MPI_Fortran_LIBRARIES})
add_executable (new_ptests.x EXCLUDE_FROM_ALL ${CMAKE_SOURCE_DIR}/include/driver.F90)
set_source_files_properties(${CMAKE_SOURCE_DIR}/include/driver.F90 PROPERTIES COMPILE_DEFINITIONS _TEST_SUITES="testSuites.inc")
target_link_libraries(new_ptests.x new_ptests)

#set(REMOTE_EXE remote.x)
#add_executable (${REMOTE_EXE} parallelRemoteProgram.F90)

# Fix for openmpi 1.8.8 which complains about forking due to selftests of
# robust runner.
if (MPIEXEC MATCHES ".*openmpi/1.8.8/.*")
  set (MPIEXEC_PREFLAGS ${MPIEXEC_PREFLAGS} --mca mpi_warn_on_fork 0)
endif ()

if (MPI_USE_MPIEXEC)
  #
  # Add the target for executing the tests by calling on CMake's FindMPI to find the
  # standard mpiexec.  Not the default because we've found the hardcoded mpirun more portable.
  # There may be situations, however, where the FindMPI-based target will work better.
  #
  add_test(NAME mpi-tests
    COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 4 ${MPIEXEC_PREFLAGS} tests/parallel_tests.x
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
else()
  #
  # Add the target for executing the tests with a hardcoded call to mpirun.
  #
  add_test(NAME mpi-tests
    COMMAND mpirun ${MPIEXEC_PREFLAGS} -np 4 ./parallel_tests.x
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
  #
endif()
add_dependencies(tests parallel_tests.x)

add_test(NAME new_ptests
    COMMAND mpirun ${MPIEXEC_PREFLAGS} -np 4 ./new_ptests.x
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
set_property (TEST new_ptests
    PROPERTY FAIL_REGULAR_EXPRESSION "Encountered 1 or more failures/errors during testing"
    )

add_dependencies(tests new_ptests.x)
