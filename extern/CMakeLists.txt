include(build_submodule)
build_submodule(gFTL PROJECT GFTL TARGET gftl)
build_submodule(gFTL-shared PROJECT GFTL_SHARED TARGET gftl-shared)
build_submodule(fArgParse PROJECT FARGPARSE TARGET fargparse)

add_library (ext INTERFACE)
target_link_libraries (ext INTERFACE gftl gftl-shared fargparse)

