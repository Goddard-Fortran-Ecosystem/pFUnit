#!/bin/sh
# source: mpi4py
# https://github.com/mpi4py/mpi4py/blob/master/conf/travis/install-mpi.sh

set -e

MPI_IMPL="$1"
MPI_VER="$2"
os=`uname`

if [ ! -d "${HOME}/local/${MPI_IMPL}/bin" ]
then
   mkdir -p ${HOME}/MPI/src/openmpi && cd ${HOME}/MPI/src/openmpi
   wget --no-check-certificate https://download.open-mpi.org/release/open-mpi/v${MPI_VER%.*}/${MPI_IMPL}-${MPI_VER}.tar.bz2
   tar xjf ${MPI_IMPL}-${MPI_VER}.tar.bz2 && rm ${MPI_IMPL}-${MPI_VER}.tar.bz2
   cd ${MPI_IMPL}-${MPI_VER}
   ./configure --prefix=${HOME}/local/${MPI_IMPL} --disable-wrapper-rpath --disable-wrapper-runpath --with-hwloc=internal --with-libevent=internal
   make -j $(nproc)
   make install-strip
   cd .. && rm -r ${MPI_IMPL}-${MPI_VER}
   exit 0
else
   echo "Using cached ${MPI_IMPL} directory";
   exit 0
fi
