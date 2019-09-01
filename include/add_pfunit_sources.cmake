#set (PFUNIT_PREPROCESSOR python ${PFUNIT_SOURCE_DIR}/bin/funitproc)

# - Pass a list of files through the pFUnit macro processor
#
# ADD_PFUNIT_SOURCES( out_var source1 ... sourceN )
#
#    out_var    A list containing all the output file names, suitable
#                    to be passed to add_executable or add_library.
#
# Source arguments are treated differently depending on whether they are
# given as absolute or relative paths.
#
# 1) Relative paths are assumed to be relative to
#    CMAKE_CURRENT_SOURCE_DIR.  The corresponding ouput file will
#    be placed in the same relative location but with a base dir of
#    CMAKE_CURRENT_BINARY_DIR.
#
# 2) Absolute paths are assumed to already be within the build tree and
#    allow projects to generate tests.   For these the output file
#    is in the same directory as the source.
#
# Example:
#    add_pfunit_sources( SRCS src/test1.cxx.pfunit src/test2.cxx.pfunit )
#    add_executable( test ${SRCS} )
#

function( ADD_PFUNIT_SOURCES out_var )

  set( out_files )
  foreach( file ${ARGN} )

    get_filename_component (basename "${file}" NAME_WE)
    get_filename_component( abs_file "${file}" ABSOLUTE )
    
    # replace the extension with .F90 to determine the output file name
  
    if (IS_ABSOLUTE ${file}) # it is in build tree, and out_file is sibling

      get_filename_component (file_dir "${file}" DIRECTORY BASE_DIR ${CMAKE_CURRENT_BINARY_DIR})
      set (out_file ${file_dir}/${basename}.F90)

    else () # it is relative to source tree

      # Determine name ouf output file in build tree
      file( RELATIVE_PATH rel_file "${CMAKE_CURRENT_SOURCE_DIR}" "${abs_file}" )
      get_filename_component (file_dir "${rel_file}" DIRECTORY BASE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
      set (out_file ${CMAKE_CURRENT_BINARY_DIR}/${file_dir}/${basename}.F90)

      # create the output directory if it doesn't exist
      get_filename_component( dir "${out_file}" PATH )
      if( NOT IS_DIRECTORY "${dir}" )
	file( MAKE_DIRECTORY "${dir}" )
      endif( NOT IS_DIRECTORY "${dir}" )

    endif()

    # append the output file to the list of outputs
    list( APPEND out_files "${out_file}" )
    
    # now add the custom command to generate the output file
    add_custom_command( OUTPUT "${out_file}"
      COMMAND ${PFUNIT_PARSER} "${abs_file}" "${out_file}"
      DEPENDS "${abs_file}"
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      )
    
  endforeach ( )
  
  # set the output list in the calling scope
  set( ${out_var} ${${out_var}} ${out_files} PARENT_SCOPE )

endfunction( ADD_PFUNIT_SOURCES )


