include(build_submodule)
build_submodule(gFTL PROJECT GFTL TARGET gftl)
build_submodule(gFTL-shared PROJECT GFTL_SHARED TARGET gftl-shared)
build_submodule(fArgParse PROJECT FARGPARSE TARGET fargparse)

add_library (external-pfunit INTERFACE)
target_link_libraries (external-pfunit INTERFACE gftl gftl-shared fargparse)
install (TARGETS external-pfunit EXPORT PFUNIT DESTINATION "${dest}/lib")

