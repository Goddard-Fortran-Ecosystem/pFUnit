set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

include_directories(${CMAKE_BINARY_DIR}/mod)

set (srcs
  AbstractMatcher.F90
  MatcherDescription.F90
  StringDescription.F90
  MatcherAssert.F90
  )

add_library(hamcrest STATIC ${srcs})
add_dependencies(hamcrest funit)

