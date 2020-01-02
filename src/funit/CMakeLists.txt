# We declare the library here, but sources are generally added in
# subdirs.  Further, do to a limitation in the scope for generated
# files, we must "include" the CMakeLists.txt in each subdir rather
# than use add_subdirectory().

add_library (funit)
set(funit_targets funit)
if (BUILD_SHARED)
  add_library (funit_shared SHARED)
  set_target_properties(funit_shared PROPERTIES OUTPUT_NAME funit)
  list(APPEND funit_targets funit_shared)
endif()

add_subdirectory (core)
add_subdirectory (asserts)
if (NOT SKIP_FHAMCREST)
  add_subdirectory (fhamcrest)
endif()

foreach(funit_target ${funit_targets})
  set_target_properties (${funit_target}
    PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
  target_include_directories (${funit_target} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
    $<BUILD_INTERFACE:${PFUNIT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:${dest}/include>
  )

  # Subdirectories declare simple OBJECT directories to capture sources
  # funit is the "real" library.
  add_dependencies (${funit_target} funit-core)
  add_dependencies (${funit_target} asserts)
  if (NOT SKIP_FHAMCREST)
    add_dependencies (${funit_target} fhamcrest)
  endif ()

  if (NOT SKIP_ROBUST)
    target_compile_definitions(${funit_target} PRIVATE Robust)
  endif ()

  target_sources(${funit_target} PRIVATE
    $<TARGET_OBJECTS:funit-core>
    $<TARGET_OBJECTS:asserts>
    $<TARGET_OBJECTS:fhamcrest>)
  if (OpenMP_Fortran_FOUND)
    target_link_libraries (${funit_target} PUBLIC OpenMP::OpenMP_Fortran)
  endif ()

  target_link_libraries (${funit_target} PUBLIC external-pfunit)
  install (TARGETS ${funit_target} EXPORT PFUNIT DESTINATION ${dest}/lib)
endforeach ()

target_sources(funit PRIVATE
  FUnit.F90 funit_main.F90
  )

install (DIRECTORY  ${CMAKE_CURRENT_BINARY_DIR}/mod/ DESTINATION ${dest}/include)
install (EXPORT PFUNIT DESTINATION "${dest}/cmake")
