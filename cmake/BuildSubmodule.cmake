# Build subprojects in a standardized way.
macro (build_submodule name)

  set(options)
  set(oneValueArgs DESCRIPTION)
  set(multiValueArgs DEPENDENCIES TARGETS)
  cmake_parse_arguments(build_submodule "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  string(TOUPPER ${name} NAME)
  set(${NAME} "" CACHE PATH ${build_submodule_DESCRIPTION})
  
  if (${NAME}) # use default
    set(${name}_install_dir ${${NAME}})
  else()
    set(${name}_source_dir ${CMAKE_CURRENT_SOURCE_DIR}/${name})
    set(${name}_install_dir ${CMAKE_CURRENT_BINARY_DIR}/${name}/install)
    
    include(${CMAKE_ROOT}/Modules/ExternalProject.cmake)
    file(GLOB all_files ${${name}_source_dir}/*)
    list(LENGTH all_files n_files)
    
    if(n_files LESS_EQUAL 3)
      # git clone command did not use --recurse-submodules
      set(repository https://github.com/Goddard-Fortran-Ecosystem/${name}.git)
      set(download_command git submodule init)
      set(update_command git submodule update)
    else()
      set(repository "")
      set(download_command "")
      set(update_command "")
    endif()

    message("External project ... ${name}")
    if(build_submodule_DEPENDENCIES)
      foreach(dependency ${build_submodule_DEPENDENCIES})
	string(TOUPPER ${dependency} DEPENDENCY)
	set(extra_args "${extra_args};-D${DEPENDENCY}=${${dependency}_install_dir}")
      endforeach()
    endif()
    message("extra: ${extra_args}")
    ExternalProject_Add(${name}
      GIT_REPOSITORY ${repository}
      DOWNLOAD_COMMAND ${download_command}
      UPDATE_COMMAND ${update_command}
      PREFIX ${CMAKE_CURRENT_BINARY_DIR}/${name}
      SOURCE_DIR ${${name}_source_dir}
      INSTALL_DIR ${${name}_install_dir}
      BUILD_COMMAND make
      CMAKE_ARGS ${extra_args}
      INSTALL_COMMAND make install)

    if (build_submodule_DEPENDENCIES)
      ExternalProject_Add_StepDependencies(${name} configure ${build_submodule_DEPENDENCIES})
    endif()
    if (build_submodule_TARGETS)
      foreach(lib ${build_submodule_TARGETS})
	add_library(lib_${lib} STATIC IMPORTED)
	message("adding library lib_${lib}")
	set_target_properties(lib_${lib} PROPERTIES IMPORTED_LOCATION ${${name}_install_dir}/lib/lib${lib}.a)
      endforeach()
    endif()
      
  endif()
  
endmacro()
