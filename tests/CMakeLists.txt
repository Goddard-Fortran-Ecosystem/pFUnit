find_package (PythonInterp REQUIRED)
set (PFUNIT_PARSER "${PYTHON_EXECUTABLE}" "${CMAKE_CURRENT_LIST_DIR}/../bin/funitproc")
set (PFUNIT_DRIVER "${CMAKE_CURRENT_LIST_DIR}/../include/driver.F90")
include (add_pfunit_sources)
include (add_pfunit_ctest)


add_subdirectory(funit-core)
if (NOT SKIP_FHAMCREST)
  add_subdirectory(fhamcrest)
endif ()

if (MPI_Fortran_FOUND)
  add_subdirectory(pfunit)
endif ()
