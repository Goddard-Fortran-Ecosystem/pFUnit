add_subdirectory (core)
add_subdirectory (hamcrest)

if (MPI)
  add_subdirectory (mpi)
endif ()

if (ESMF)
  add_subdirectory (esmf)
endif ()

