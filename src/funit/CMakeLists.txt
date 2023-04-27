# We declare the library here, but sources are generally added in
# subdirs.  Further, do to a limitation in the scope for generated
# files, we must "include" the CMakeLists.txt in each subdir rather
# than use add_subdirectory().
set(FUNIT_MODULE_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}/mod)

function(set_funit_target_properties funit_target)
  target_include_directories(${funit_target} PUBLIC
    $<BUILD_INTERFACE:${FUNIT_MODULE_BUILD_DIR}>
    $<INSTALL_INTERFACE:${dest}/include>
  )
  set_target_properties(${funit_target}
    PROPERTIES Fortran_MODULE_DIRECTORY ${FUNIT_MODULE_BUILD_DIR})
endfunction()

function(funit_target_link_pfunit funit_target)
  if (NOT SKIP_ROBUST)
    target_compile_definitions(${funit_target} PRIVATE Robust)
  endif ()

  if (OpenMP_Fortran_FOUND)
    target_link_libraries (${funit_target} PUBLIC OpenMP::OpenMP_Fortran)
  endif ()

  if (NOT TARGET GFTL::gftl)
    message(FATAL_ERROR "Could not find gFTL. This should not happen.")
  endif ()

  if (NOT TARGET GFTL_SHARED::gftl-shared)
    message(FATAL_ERROR "Could not find gFTL-shared. This should not happen.")
  endif ()

  target_link_libraries (${funit_target} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared FARGPARSE::fargparse)
  target_include_directories(${funit_target} PUBLIC
    $<BUILD_INTERFACE:${PFUNIT_SOURCE_DIR}/include>
    )
endfunction()

# Add dependency directories
set(funit_deps funit-core asserts)
add_subdirectory (core)
add_subdirectory (asserts)
if (NOT SKIP_FHAMCREST)
  add_subdirectory (fhamcrest)
  list(APPEND funit_deps fhamcrest)
endif()

# Add object library for 'main' functions
add_library(funit-main OBJECT
  FUnit.F90
  funit_main.F90
  )
if (SKIP_FHAMCREST)
  target_compile_definitions(funit-main PRIVATE -DSKIP_FHAMCREST)
endif ()

set_funit_target_properties(funit-main)
funit_target_link_pfunit(funit-main)
add_dependencies(funit-main ${funit_deps})
list(APPEND funit_deps funit-main)

# Create list of objects to be added to libraries
foreach (funit_dep ${funit_deps})
  list(APPEND funit_target_objects "$<TARGET_OBJECTS:${funit_dep}>")
endforeach()

# Create exported funit libraries
add_library (funit)
add_library (PFUNIT::funit ALIAS funit)
set(funit_targets funit)
if (BUILD_SHARED_LIBS)
  add_library (funit_shared SHARED)
  add_library (PFUNIT::funit_shared ALIAS funit_shared)
  set_target_properties(funit_shared PROPERTIES OUTPUT_NAME funit)
  list(APPEND funit_targets funit_shared)
endif()

foreach(funit_target IN LISTS funit_targets)
  set_funit_target_properties(${funit_target})
  funit_target_link_pfunit(${funit_target})

  # Subdirectories declare simple OBJECT directories to capture sources
  # funit is the "real" library.
  add_dependencies(${funit_target} ${funit_deps})
  target_sources(${funit_target} PRIVATE ${funit_target_objects})

  install (TARGETS ${funit_target} EXPORT PFUNIT DESTINATION ${dest}/lib)
endforeach ()

install (DIRECTORY  ${CMAKE_CURRENT_BINARY_DIR}/mod/ DESTINATION ${dest}/include)
install (EXPORT PFUNIT DESTINATION "${dest}/cmake")
