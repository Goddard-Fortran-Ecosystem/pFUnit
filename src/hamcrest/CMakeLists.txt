set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

get_directory_property(dir_defs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMPILE_DEFINITIONS)
include_directories(${CMAKE_BINARY_DIR}/mod)

set(python_defs)
foreach (def ${dir_defs})
  list(APPEND python_defs "-${def}")
endforeach()

set (srcs
  Array.F90
  MatcherAssert.F90
  MatcherDescription.F90
  BaseDescription.F90
  StringDescription.F90
  MatcherAssert.F90

  # Matchers
  AbstractMatcher.F90
  BaseMatcher.F90
  TypeSafeMatcher.F90
  FeatureMatcher.F90
  DescribedAs.F90
  Is.F90
  IsNot.F90
  IsEqual.F90
  AnyOf.F90
  SubstringMatcher.F90
  StringContains.F90
  StringStartsWith.F90
  StringEndsWith.F90
  Every.F90

  IsArrayWithSize.F90
  Matchable.F90
  hamcrest.F90
  )

add_library(hamcrest STATIC ${srcs})
target_link_libraries(hamcrest funit)

