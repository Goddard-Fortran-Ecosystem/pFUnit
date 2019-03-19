add_subdirectory (funit)

if (MPI_Fortran_FOUND)
  add_subdirectory (pfunit)
endif ()

if (ESMF_FOUND)
  add_subdirectory (efunit)
endif ()

