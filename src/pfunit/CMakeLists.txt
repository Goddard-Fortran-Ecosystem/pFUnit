set(pfunit_targets pfunit)
add_library (pfunit)
add_library (PFUNIT::pfunit ALIAS pfunit)
if (BUILD_SHARED_LIBS)
  add_library (pfunit_shared SHARED)
  add_library (PFUNIT::pfunit_shared ALIAS pfunit_shared)
  set_target_properties(pfunit_shared PROPERTIES OUTPUT_NAME pfunit)
  list(APPEND pfunit_targets pfunit_shared)
endif ()

# Header-only target so we can add MPI defines to both pfunit-core and
# pfunit. These are definitions that allow compatibility between the
# `mpi` and `mpi_f08` modules.
add_library (pfunit-mpi-defines INTERFACE)
add_library (PFUNIT::pfunit-mpi-defines ALIAS pfunit-mpi-defines)
target_include_directories (pfunit-mpi-defines INTERFACE
  $<BUILD_INTERFACE:${PFUNIT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${dest}/include>
  )
if (ENABLE_MPI_F08)
  target_compile_definitions (pfunit-mpi-defines INTERFACE PF_USE_MPI_F08)
endif()

add_subdirectory (core)

foreach(pfunit_target IN LISTS pfunit_targets)
  set_property (TARGET ${pfunit_target} PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)

  target_include_directories (${pfunit_target} PUBLIC ${MPI_Fortran_INCLUDE_PATH})

  target_include_directories (${pfunit_target} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
    $<BUILD_INTERFACE:${PFUNIT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:${dest}/include>
  )

  target_sources (${pfunit_target} PRIVATE $<TARGET_OBJECTS:pfunit-core>)
  target_sources (${pfunit_target} PRIVATE pFUnit.F90 pfunit_main.F90)
  target_link_libraries (${pfunit_target} PUBLIC funit ${MPI_Fortran_LIBRARIES})
  target_link_libraries (${pfunit_target} PRIVATE pfunit-mpi-defines)

  install (DIRECTORY  ${CMAKE_CURRENT_BINARY_DIR}/mod/ DESTINATION ${dest}/include)
  install (TARGETS ${pfunit_target} EXPORT PFUNIT DESTINATION ${dest}/lib)
endforeach()

install (TARGETS pfunit-mpi-defines EXPORT PFUNIT DESTINATION ${dest}/lib)
