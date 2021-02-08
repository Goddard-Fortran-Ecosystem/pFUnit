# Build subprojects in a standardized way.

macro (build_submodule submodule)

  set(options)
  set(oneValueArgs TARGET PROJECT)
  set(multiValueArgs)
  cmake_parse_arguments(build_submodule "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  find_package(${build_submodule_PROJECT} QUIET)

  if (${build_submodule_PROJECT}_FOUND)
    message(STATUS "Using package '${build_submodule_PROJECT}'")
  else()
    if (TARGET ${build_submodule_TARGET})
      message(STATUS "Using target '${build_submodule_TARGET}'")
    else()
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
      get_filename_component(submodule_source_dir ${submodule} ABSOLUTE BASE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
      get_filename_component(submodule_name ${submodule} NAME)
      get_filename_component(submodule_binary_dir ${submodule_name} ABSOLUTE BASE_DIR ${PROJECT_BINARY_DIR}/extern)
      message(STATUS "Using submodule ${submodule_source_dir}")
      add_subdirectory(${submodule_source_dir} ${submodule_binary_dir})
    endif ()
  endif()
endmacro()
