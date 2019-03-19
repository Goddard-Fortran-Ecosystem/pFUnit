set(generated_sources)

get_directory_property(dir_defs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMPILE_DEFINITIONS)
set(python_defs)
foreach (def ${dir_defs})
  list(APPEND python_defs "-${def}")
endforeach()

function (expand in out)
  set_source_files_properties (${CMAKE_CURRENT_BINARY_DIR}/${out} PROPERTIES GENERATED 1)
  set (generated_sources ${generated_sources} ${CMAKE_CURRENT_BINARY_DIR}/${out} PARENT_SCOPE)
  add_custom_command(
    OUTPUT ${out}
    COMMAND ${PYTHON_EXECUTABLE} ${PFUNIT_SOURCE_DIR}/tools/overload_template.py --rank=${rank} -i ${in} -o ${out} ${python_defs}
    DEPENDS ${in} ${PFUNIT_SOURCE_DIR}/tools/overload_template.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT Generating ${out}
    )
endfunction()

foreach(type Logical Integer Real Complex)
  foreach(rank RANGE ${MAX_ASSERT_RANK})
    expand (${CMAKE_CURRENT_SOURCE_DIR}/Assert_${type}.tmpl Assert${type}_${rank}d.F90)
  endforeach()
endforeach()

expand (${CMAKE_CURRENT_SOURCE_DIR}/FormatIntrinsic.tmpl FormatIntrinsic.F90)

foreach(rank RANGE ${MAX_ASSERT_RANK})
  expand (${CMAKE_CURRENT_SOURCE_DIR}/Norms.tmpl Norms_${rank}d.F90)
endforeach()


set (overloads_file "${CMAKE_CURRENT_BINARY_DIR}/AssertOverloads.F90")
list(APPEND generated_sources ${overloads_file})
set_source_files_properties (${overloads_file} PROPERTIES GENERATED 1)
set_source_files_properties (Asset.F90 PROPERTIES GENERATED 1)
file (WRITE ${overloads_file} "! This file was automatically generated \n")
file (WRITE ${overloads_file} "module pf_AssertOverloads \n")
foreach (rank RANGE ${MAX_ASSERT_RANK})
  file (APPEND ${overloads_file} "use pf_AssertLogical_${rank}d\n")
  file (APPEND ${overloads_file} "use pf_AssertInteger_${rank}d\n")
  file (APPEND ${overloads_file} "use pf_AssertReal_${rank}d\n")
  file (APPEND ${overloads_file} "use pf_AssertComplex_${rank}d\n")
  file (APPEND ${overloads_file} "use pf_Norms_${rank}d\n")
endforeach ()

file (APPEND ${overloads_file} "end module pf_AssertOverloads \n")


add_library (asserts OBJECT
  AssertUtilities.F90
  Assert.F90
  AssertBasic.F90
  AssertString.F90
  )

set_target_properties (asserts PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../mod)
target_sources (asserts PRIVATE ${generated_sources})
target_link_libraries (asserts funit-core)

target_include_directories (asserts PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../mod>
  $<BUILD_INTERFACE:${PFUNIT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${dest}/include>
)
