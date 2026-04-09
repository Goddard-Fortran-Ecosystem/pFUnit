# This script is executed at build time to generate the test suite .inc file
# 
# Required variables (passed via -D):
#   PF_FILES_LIST - Path to a file containing list of .pf file paths (one per line)
#   OUTPUT_FILE   - Path to the .inc file to generate

# Read the list of .pf files from the file
file(STRINGS "${PF_FILES_LIST}" pf_files_list)

# Initialize output content
set(test_suites_inc "")

# Process each .pf file
foreach(pf_file ${pf_files_list})
  get_filename_component(basename "${pf_file}" NAME_WE)
  set(test_suites_inc "${test_suites_inc}ADD_TEST_SUITE(${basename}_suite)\n")
endforeach()

# Write the output file
file(WRITE "${OUTPUT_FILE}" "${test_suites_inc}")
