add_subdirectory(core-tests)
add_subdirectory(hamcrest-tests)

if (MPI)
  add_subdirectory(mpi-tests)
endif ()
