get_directory_property(dir_defs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMPILE_DEFINITIONS)

set(python_defs)
foreach (def ${dir_defs})
  list(APPEND python_defs "-${def}")
endforeach()

set (srcs
  Array.F90
  MatcherAssert.F90

  SurrogateDescription.F90
  MatcherDescription.F90
  SelfDescribing.F90
  TypeSafeSelfDescribing.F90
  SelfDescribingVector.F90
  BaseDescription.F90
  StringDescription.F90
  MatcherAssert.F90

  # Matchers
  AbstractMatcher.F90
  MatcherVector.F90
  BaseMatcher.F90
  TypeSafeMatcher.F90
  FeatureMatcher.F90
  DescribedAs.F90
  Is.F90
  IsNot.F90
  IsEqual.F90
  IsNear.F90
  IsRelativelyNear.F90
  AnyOf.F90
  AllOf.F90
  SubstringMatcher.F90
  StringContains.F90
  StringStartsWith.F90
  StringEndsWith.F90
  Every.F90

  IsArrayWithSize.F90
  Matchable.F90
  fHamcrest.F90
  )

add_library(fhamcrest OBJECT ${srcs})
set_target_properties (fhamcrest PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/../mod)

target_include_directories (fhamcrest PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../mod>
  $<INSTALL_INTERFACE:${dest}/include>
)
target_link_libraries(fhamcrest PUBLIC funit-core)



