set(pfunit_targets pfunit)
add_library (pfunit)
add_library (PFUNIT::pfunit ALIAS pfunit)
if (BUILD_SHARED)
  add_library (pfunit_shared SHARED)
  add_library (PFUNIT::pfunit_shared ALIAS pfunit_shared)
  set_target_properties(pfunit_shared PROPERTIES OUTPUT_NAME pfunit)
  list(APPEND pfunit_targets pfunit_shared)
endif ()

add_subdirectory (core)

foreach(pfunit_target ${pfunit_targets})
  set_target_properties (${pfunit_target} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)

  target_include_directories (${pfunit_target} PUBLIC ${MPI_Fortran_INCLUDE_PATH})

  target_include_directories (${pfunit_target} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
    $<BUILD_INTERFACE:${PFUNIT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:${dest}/include>
  )

  target_sources (${pfunit_target} PRIVATE $<TARGET_OBJECTS:pfunit-core>)
  target_sources (${pfunit_target} PRIVATE pFUnit.F90 pfunit_main.F90)
  target_link_libraries (${pfunit_target} PUBLIC funit ${MPI_Fortran_LIBRARIES})

  install (DIRECTORY  ${CMAKE_CURRENT_BINARY_DIR}/mod/ DESTINATION ${dest}/include)
  install (TARGETS ${pfunit_target} EXPORT PFUNIT DESTINATION ${dest}/lib)
endforeach()
