# Build subprojects in a standardized way.

macro (build_submodule submodule)

  set(options)
  set(oneValueArgs TARGET PROJECT)
  set(multiValueArgs)
  cmake_parse_arguments(build_submodule "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  find_package(${build_submodule_PROJECT} QUIET)

  if (NOT ${build_submodule_PROJECT}_FOUND)
    if (NOT TARGET ${build_submodule_TARGET})

      # from https://cliutils.gitlab.io/modern-cmake/chapters/projects/submodule.html
      find_package(Git QUIET)
      if (GIT_FOUND AND EXISTS "${PROJECT_SOURCE_DIR}/.git")
	option (GIT_SUBMODULE "Check submodules during build" ON)
	if (GIT_SUBMODULE)
	  execute_process (
	    COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
	    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	    RESULT_VARIABLE GIT_SUBMODULE_RESULT
	    )
	  if (NOT GIT_SUBMODULE_RESULT EQUAL "0")
	    message(FATAL_ERROR "git submodule update --init failed with ${GIT_SUBMODULE_RESULT}, please checkout submodules")
	  endif ()
	endif ()
      endif ()
      add_subdirectory(${submodule})
    endif ()
  endif()

endmacro()
