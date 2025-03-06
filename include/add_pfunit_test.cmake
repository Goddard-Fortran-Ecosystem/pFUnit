# Function     : add_pfunit_test
#
# Description  : Helper function for compiling and adding pFUnit tests to the CTest testing framework. Any libraries needed
#                in testing should be linked to manually.
#                IMPORTANT! This function will only work if the test source filename is the same as the module inside it!
#                For example, the file testSomething.pf should contain the module testSomething.
#
# Arguments    : - test_package_name: Name of the test package
#                - test_sources     : List of pf-files to be compiled
#                - extra_sources    : List of extra Fortran source code used for testing (if none, input empty string "")
#                - extra_sources_c    : List of extra C/C++ source code used for testing (if none, input empty string "")
#
# Example usage: enable_testing()
#                set (TEST_SOURCES
#                   testMyLib.pf
#                    )
#                add_pfunit_test (myTests "${TEST_SOURCES} "" "")
#                target_link_libraries (myTests myLibrary) #Assuming "myLibrary" is already defined
#
#                Compile the tests:   make myTests
#                Run the tests with CTest: ctest -R myTests --verbose

function (add_pfunit_test test_package_name test_sources extra_sources extra_sources_c)

    if (NOT test_sources)
        message (WARNING "No test sources defined for '${test_package_name}', ignoring...")
        return ()
    endif (NOT test_sources)

    #################################################
    # Preprocessing                                 #
    #################################################
    set (SRC_GEN_DIR ${CMAKE_CURRENT_BINARY_DIR}/src_gen/${test_package_name})
    execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${SRC_GEN_DIR}/)

    set (TEST_SUITES_INC "")
    foreach (file ${test_sources})
        get_filename_component (basename ${file} NAME_WE)
        set (fsrc "${SRC_GEN_DIR}/${basename}.F90")
        list (APPEND test_sources_f90 ${fsrc})
        set (TEST_SUITES_INC "${TEST_SUITES_INC}ADD_TEST_SUITE(${basename}_suite)\n")
    endforeach()

    set (TEST_SUITE_INC_FILE ${SRC_GEN_DIR}/testSuites.inc)
    set (SHOULD_WRITE_INC_FILE True)

    # Check if .inc file already has been generated. If so, only write new
    # file if contents has changed. This avoid tests recompiling after reconfiguring cmake.
    if (EXISTS ${TEST_SUITE_INC_FILE})
        file (READ ${TEST_SUITE_INC_FILE} existing_file)
        if (${existing_file} STREQUAL ${TEST_SUITES_INC})
            set (SHOULD_WRITE_INC_FILE False)
        endif (${existing_file} STREQUAL ${TEST_SUITES_INC})
    endif (EXISTS ${TEST_SUITE_INC_FILE})

    if (${SHOULD_WRITE_INC_FILE})
        file (WRITE ${TEST_SUITE_INC_FILE} ${TEST_SUITES_INC})
    endif (${SHOULD_WRITE_INC_FILE})

    list (LENGTH test_sources len)
    math(EXPR n "${len} - 1")
    foreach (i RANGE ${n})
        list (GET test_sources ${i} pf_file)
        list (GET test_sources_f90 ${i} f90_file)
        add_custom_command(
            OUTPUT ${f90_file}
            COMMAND ${PFUNIT_PARSER} ${pf_file} ${f90_file}
            MAIN_DEPENDENCY ${pf_file}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            #COMMENT "Generating '${f90_file}' from '${pf_file}'"
            )
    endforeach()

    #################################################
    # Define executable and any auxiliary library   #
    #################################################

    # Main executable
    add_executable (${test_package_name}
        ${test_sources_f90}
        ${extra_sources}
        ${PFUNIT_DRIVER}
        ${PFUNIT_TESTUTILS}
        )

    # Define directory of Fortran mod-files for main executable
    execute_process(COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/include/${test_package_name})
    set_property (TARGET ${test_package_name}
        PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/${test_package_name})

    # C files library, if relevant
    if (NOT extra_sources_c STREQUAL "")
        add_library (${test_package_name}_c STATIC ${extra_sources_c})
        target_link_libraries (${test_package_name} ${test_package_name}_c)
    endif ()

    # Define dependencies
    find_package(OpenMP)
    target_link_libraries (${test_package_name} ${PFUNIT_LIBRARIES})
    target_include_directories (${test_package_name} PRIVATE ${PFUNIT_INCLUDE_DIRS})
    target_include_directories (${test_package_name} PRIVATE ${SRC_GEN_DIR})
    target_include_directories (${test_package_name} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/include/${test_package_name})

    # Define TestSuites
    target_compile_definitions (${test_package_name} PRIVATE -D_TEST_SUITES="${TEST_SUITE_INC_FILE}")

    # Test utility preprocessing
    set_property ( SOURCE ${PFUNIT_TESTUTILS}
        APPEND
        PROPERTY COMPILE_DEFINITIONS "__PROJECT_DIR__='${CMAKE_CURRENT_SOURCE_DIR}'"
        )

    if(MSVC)
        set_property (TARGET ${test_package_name}
            PROPERTY LINK_FLAGS " /INCREMENTAL:NO ")
    endif(MSVC)

    if (UNIX)
        set_property (TARGET ${test_package_name}
            PROPERTY LINKER_LANGUAGE Fortran)
    endif (UNIX)

    #################################################
    # Define test in CTest system                   #
    #################################################
    add_test (NAME ${test_package_name}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${test_package_name}
        )
    set_property (TEST ${test_package_name}
        PROPERTY FAIL_REGULAR_EXPRESSION "Encountered 1 or more failures/errors during testing"
        )

endfunction (add_pfunit_test test_package_name test_sources extra_sources extra_sources_c)
