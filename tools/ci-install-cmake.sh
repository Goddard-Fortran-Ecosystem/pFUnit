#!/bin/sh

set -e

cmake_ver="$1"

if [ ! -d "${HOME}/local/cmake/bin" ] ; then
   wget https://github.com/Kitware/CMake/releases/download/v${cmake_ver}/cmake-${cmake_ver}.tar.gz
   tar -xzf cmake-${cmake_ver}.tar.gz && rm cmake-${cmake_ver}.tar.gz
   cd cmake-${cmake_ver}
   mkdir build && cd build
   cmake .. -DCMAKE_INSTALL_PREFIX=${HOME}/local/cmake
   make -j$(nproc)
   make install/strip
   cd ../.. && rm -r cmake-${cmake_ver}
fi