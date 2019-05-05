#set (PFUNIT_PREPROCESSOR python ${PFUNIT_SOURCE_DIR}/bin/funitproc)

# - Pass a list of files through the pFUnit macro processor
#
# ADD_PFUNIT_SOURCES( out_var source1 ... sourceN )
#
#    out_var    A list containing all the output file names, suitable
#                    to be passed to add_executable or add_library.
#
# The suffix of the source files are stripped from the output
# file name. The output files are placed in the same relative location
# to CMAKE_CURRENT_BINARY_DIR as they are to CMAKE_CURRENT_SOURCE_DIR.
#
# Example:
#    add_pfunit_sources( SRCS src/test1.cxx.pfunit src/test2.cxx.pfunit )
#    add_executable( test ${SRCS} )
function( ADD_PFUNIT_SOURCES out_var )
     set( out_files )
     foreach( file ${ARGN} )
         # first we might need to make the input file absolute
         get_filename_component( abs_file "${file}" ABSOLUTE )
         # get the relative path of the file to the current source dir
         file( RELATIVE_PATH rel_file "${CMAKE_CURRENT_SOURCE_DIR}" "${abs_file}" )

         # replace the extension with .F90 to determine the output file name
	 get_filename_component (extension ${file} EXT)
         string( REGEX REPLACE "${extension}" ".F90" out_file "${CMAKE_CURRENT_BINARY_DIR}/${rel_file}" )
         # append the output file to the list of outputs
         list( APPEND out_files "${out_file}" )
         # create the output directory if it doesn't exist
         get_filename_component( dir "${out_file}" PATH )
         if( NOT IS_DIRECTORY "${dir}" )
             file( MAKE_DIRECTORY "${dir}" )
         endif( NOT IS_DIRECTORY "${dir}" )
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


