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
  hamcrest.F90
  )

add_library(hamcrest STATIC ${srcs})
target_include_directories (hamcrest PUBLIC
  $<BUILD_INTERFACE:${PFUNIT_BINARY_DIR}/mod>
  $<INSTALL_INTERFACE:${dest}/include>
)
target_link_libraries(hamcrest PUBLIC funit)

install (DIRECTORY  ${PFUNIT_BINARY_DIR}/mod/ DESTINATION ${dest}/include)

install (TARGETS hamcrest EXPORT PFUNIT DESTINATION ${dest}/lib)
if (BUILD_SHARED)
    install (TARGETS hamcrest_shared EXPORT PFUNIT DESTINATION ${dest}/lib)
endif()


