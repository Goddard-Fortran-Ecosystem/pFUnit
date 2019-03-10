add_subdirectory(core-tests)
add_subdirectory(hamcrest-tests)

if (MPI_Fortran_FOUND)
  add_subdirectory(mpi-tests)
endif ()
