get_directory_property(dir_defs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMPILE_DEFINITIONS)

set(python_defs)
foreach (def ${dir_defs})
  list(APPEND python_defs "-${def}")
endforeach()

set (srcs
  AbstractArrayWrapper.F90
  MatcherAssert.F90

  SurrogateDescription.F90
  MatcherDescription.F90
  SelfDescribing.F90
  TypeSafeSelfDescribing.F90
  SelfDescribingVector.F90
  BaseDescription.F90
  StringDescription.F90

  # Matchers
  AbstractMatcher.F90
  MatcherVector.F90
  BaseMatcher.F90
  TypeSafeMatcher.F90
  FeatureMatcher.F90
  DescribedAs.F90
  Is.F90
  IsNot.F90
  IsTrueOrFalse.F90
  IsEqual.F90
  RelationalMatcher.F90
  RelationalMatcherV2.F90
  RelationalMatcherV3.F90
  CharRelationalMatcher.F90
  Int32RelationalMatcher.F90
  Int64RelationalMatcher.F90
  Real32RelationalMatcher.F90
  Real64RelationalMatcher.F90
  Real128RelationalMatcher.F90
  IsGreater.F90
  IsGreaterOrEqual.F90
  IsLess.F90
  IsLessOrEqual.F90
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

set (generated_sources)
function (expand in out)
  set (generated_sources ${generated_sources} ${CMAKE_CURRENT_BINARY_DIR}/${out} PARENT_SCOPE)
  add_custom_command (
    OUTPUT ${out}
    COMMAND ${Python_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/multirank.py -i ${in} -o ${out} --rank=${rank}
    DEPENDS ${in} ${CMAKE_CURRENT_SOURCE_DIR}/multirank.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT Generating ${out}
    )
endfunction()

foreach(rank RANGE 1 ${MAX_ASSERT_RANK})
  expand (${CMAKE_CURRENT_SOURCE_DIR}/ArrayWrapper.tmpl ArrayWrapper_${rank}d.F90)
endforeach()


set (overloads_file "${CMAKE_CURRENT_BINARY_DIR}/ArrayWrapper.F90")
list(APPEND generated_sources ${overloads_file})
set_source_files_properties (${overloads_file} PROPERTIES GENERATED 1)
set_source_files_properties (Asset.F90 PROPERTIES GENERATED 1)
file (WRITE ${overloads_file} "! This file was automatically generated \n")
file (WRITE ${overloads_file} "module pf_ArrayWrapper \n")
foreach (rank RANGE 1 ${MAX_ASSERT_RANK})
  file (APPEND ${overloads_file} "use pf_ArrayWrapper_${rank}d\n")
endforeach ()

file (APPEND ${overloads_file} "end module pf_ArrayWrapper \n")

add_library(fhamcrest OBJECT ${srcs} ${generated_sources})
# Set Fortran module directories
set_funit_target_properties (fhamcrest)

target_compile_definitions (fhamcrest PRIVATE -DMAX_ASSERTRANK=${MAX_ASSERT_RANK})
target_link_libraries(fhamcrest PUBLIC funit-core)

