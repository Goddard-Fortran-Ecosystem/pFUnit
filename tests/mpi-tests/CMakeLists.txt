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

add_library(pfunittests EXCLUDE_FROM_ALL STATIC ${SERIAL_TEST_SRCS} ${PARALLEL_TEST_SRCS} ${OTHER_SOURCES})
target_link_libraries(pfunittests pfunit funit)

add_executable (parallel_tests.x EXCLUDE_FROM_ALL parallel_tests.F90)
target_link_libraries(parallel_tests.x pfunittests pfunit funit ${MPI_Fortran_LIBRARIES})
message("Found these MPI_Fortran_LIBRARIES: ${MPI_Fortran_LIBRARIES}")

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
