add_subdirectory (core)
add_subdirectory (hamcrest)

if (MPI_Fortran_FOUND)
  add_subdirectory (mpi)
endif ()

if (ESMF)
  add_subdirectory (esmf)
endif ()

install (EXPORT PFUNIT DESTINATION "${dest}/cmake")
