add_library (pfunit)

set_target_properties (pfunit PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)

target_include_directories (pfunit PUBLIC ${MPI_Fortran_INCLUDE_PATH})

target_include_directories (pfunit PUBLIC 
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
  $<BUILD_INTERFACE:${PFUNIT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${dest}/include>
)

if (BUILD_SHARED)
  add_library (pfunit_shared SHARED)
  set_target_properties(pfunit_shared PROPERTIES OUTPUT_NAME pfunit)
endif ()

add_subdirectory (core)

target_sources (pfunit PRIVATE $<TARGET_OBJECTS:pfunit-core>)
target_sources (pfunit PRIVATE pFUnit.F90 pfunit_main.F90)
target_link_libraries (pfunit PUBLIC funit ${MPI_Fortran_LIBRARIES})

install (DIRECTORY  ${CMAKE_CURRENT_BINARY_DIR}/mod/ DESTINATION ${dest}/include)
install (TARGETS pfunit EXPORT PFUNIT DESTINATION ${dest}/lib)
if (BUILD_SHARED)
    install (TARGETS pfunit_shared EXPORT PFUNIT DESTINATION ${dest}/lib)
endif()

