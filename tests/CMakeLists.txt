include (PreprocessMacro)

add_subdirectory(funit-core)
add_subdirectory(fhamcrest)

if (MPI_Fortran_FOUND)
  add_subdirectory(pfunit)
endif ()
