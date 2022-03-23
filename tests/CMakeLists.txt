set (PFUNIT_PARSER "${Python_EXECUTABLE}" "${CMAKE_CURRENT_LIST_DIR}/../bin/funitproc")
set (PFUNIT_DRIVER "${CMAKE_CURRENT_LIST_DIR}/../include/driver.F90")
set (PFUNIT_BUILD_SHARED ${BUILD_SHARED_LIBS})
set (PFUNIT_MPI_USE_MPIEXEC ${MPI_USE_MPIEXEC})
set (PFUNIT_MPI_FOUND ${MPI_Fortran_FOUND})
include (add_pfunit_sources)
include (add_pfunit_ctest)


add_subdirectory(funit-core)
if (NOT SKIP_FHAMCREST)
  add_subdirectory(fhamcrest)
endif ()

if (MPI_Fortran_FOUND)
  add_subdirectory(pfunit)
endif ()
