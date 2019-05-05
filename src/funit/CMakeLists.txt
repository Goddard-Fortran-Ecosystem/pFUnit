# We declare the library here, but sources are generally added in
# subdirs.  Further, do to a limitation in the scope for generated
# files, we must "include" the CMakeLists.txt in each subdir rather
# than use add_subdirectory().
add_library (funit)

set_target_properties (funit PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories (funit PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
  $<BUILD_INTERFACE:${PFUNIT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${dest}/include>
)

target_link_libraries(funit PUBLIC external-pfunit)


if (BUILD_SHARED)
  get_target_properties (srcs funit SOURCES)
  add_library (funit_shared SHARED ${srcs})
  target_link_libraries (funit_shared PUBLIC external-pfunit)
endif ()


# Subdirectories declare simple OBJECT directories to capture sources
# funit is the "real" library.
add_subdirectory (core)
add_subdirectory (asserts)
add_dependencies (funit funit-core)
add_dependencies (funit asserts)
if (NOT SKIP_FHAMCREST)
  add_subdirectory (fhamcrest)
  add_dependencies (funit fhamcrest)
endif ()

target_sources(funit PRIVATE FUnit.F90 funit_main.F90)
target_sources(funit PRIVATE $<TARGET_OBJECTS:funit-core>)
target_sources(funit PRIVATE $<TARGET_OBJECTS:asserts>)
target_sources(funit PRIVATE $<TARGET_OBJECTS:fhamcrest>)
if (OpenMP_Fortran_FOUND)
  target_link_libraries (funit PUBLIC OpenMP::OpenMP_Fortran)
endif ()


install (DIRECTORY  ${CMAKE_CURRENT_BINARY_DIR}/mod/ DESTINATION ${dest}/include)
install (EXPORT PFUNIT DESTINATION "${dest}/cmake")
install (TARGETS funit EXPORT PFUNIT DESTINATION ${dest}/lib)
if (BUILD_SHARED)
    install (TARGETS funit_shared EXPORT PFUNIT DESTINATION lib)
endif()
