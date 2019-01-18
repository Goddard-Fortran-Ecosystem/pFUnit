set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

get_directory_property(dir_defs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMPILE_DEFINITIONS)
include_directories(${CMAKE_BINARY_DIR}/mod)

set(python_defs)
foreach (def ${dir_defs})
  list(APPEND python_defs "-${def}")
endforeach()

set(templates IsEqual TypeSafeMatcher)
foreach(basename ${templates})
  set(oname ${basename}.F90)
  set(iname ${CMAKE_CURRENT_SOURCE_DIR}/${basename}.tmpl)
  add_custom_command(
    OUTPUT ${oname}
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/bin/overload_template.py --rank=0 -i ${iname} -o ${oname} ${python_defs}
    DEPENDS ${iname} ${CMAKE_SOURCE_DIR}/bin/overload_template.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT Generating ${oname}
    )
endforeach()




set (srcs
  MatcherDescription.F90
  BaseDescription.F90
  StringDescription.F90
  MatcherAssert.F90

  # Matchers
  AbstractMatcher.F90
  Is.F90
  IsNot.F90
  IsEqual.F90
  TypeSafeMatcher.F90
  SubstringMatcher.F90
  AnyOf.F90
  StringContains.F90
  StringStartsWith.F90
  StringEndsWith.F90
  
  hamcrest.F90
  )

add_library(hamcrest STATIC ${srcs})
target_link_libraries(hamcrest funit)

