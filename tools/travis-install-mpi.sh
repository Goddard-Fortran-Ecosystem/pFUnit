#!/bin/sh
# source: mpi4py
# https://github.com/mpi4py/mpi4py/blob/master/conf/travis/install-mpi.sh

set -e

MPI_IMPL="$1"
os=`uname`

case "$MPI_IMPL" in
openmpi-3.1.0)
   if [ ! -d "${HOME}/local/${MPI_IMPL}/bin" ]
   then
      mkdir -p ${HOME}/MPI/src/openmpi && cd ${HOME}/MPI/src/openmpi
      wget --no-check-certificate https://download.open-mpi.org/release/open-mpi/v3.1/openmpi-3.1.0.tar.bz2
      tar xjf openmpi-3.1.0.tar.bz2
      cd openmpi-3.1.0
      ./configure --prefix=${HOME}/local/${MPI_IMPL} --disable-wrapper-rpath --disable-wrapper-runpath && make -j 4 && make install && make clean
      cd ../../
      exit 0
   else
      echo "Using cached ${MPI_IMPL} directory";
      exit 0
   fi
   ;;
*)
   echo "Unknown MPI implementation: $MPI_IMPL"
   exit 1
   ;;
esac
