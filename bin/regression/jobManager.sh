#!/bin/bash
#PBS -l select=1:mpiprocs=16
#PBS -l walltime=00:30:00
#PBS -W group_list=k3002
#PBS -N pFUnit
#PBS -j eo

# This script manages the jobs in a batch environment.
# It gets called from mainRegress.sh.
# It is tailored to work on NCCS's DISCOVER machine.

umask 022

function abortNotify {
   if [ $# -eq 1 ]; then
      if [ -e $EmailLog ]; then
         cat $EmailLog >> $DebugLog
      fi
      echo -e "$1" >> $DebugLog
   fi 
   exit -1
}


function setModule {
   if [ $# -lt 3 ]; then
      abortNotify "Invalid number of arguments ($#) in function 'setModule'"
   fi

   local fortranCompiler=$1
   local version=$2
   local parallel=$3

   . /usr/share/modules/init/bash
   if [ $? -ne 0 ]; then
      abortNotify "Problem with starting up the module environment"
   fi

   moduleList=''
   moduleMPI=''
   
   if [ "$fortranCompiler" == "INTEL" ]; then
      if [ "$version" == "13.1" ]; then
         moduleFortran='comp/intel-13.1.3.192'
      elif [ "$version" == "13.0" ]; then
         moduleFortran='comp/intel-13.0.1.117'
      elif [ "$version" == "14.0" ]; then
         moduleFortran='comp/intel-14.0.1.106'
      else
         msg="$fortranCompiler version $version is not supported yet"
         echo -e "$msg\n\n" >> $DebugLog
      fi
      if [[ "$parallel" == "mpi" || "$parallel" == "hybrid" ]]; then
         moduleMPI=' mpi/impi-4.1.1.036'
      fi
   elif [ "$fortranCompiler" == "PGI" ]; then
      moduleFortran='comp/pgi-13.9.0'
      if [[ "$parallel" == "mpi"  || "$parallel" == "hybrid" ]]; then
         moduleMPI=' other/mpi/openmpi/1.7.3-pgi-13.9.0'
      fi
   elif [ "$fortranCompiler" == "NAG" ]; then
      moduleFortran='comp/nag-5.3-907'
      if [[ "$parallel" == "mpi"  || "$parallel" == "hybrid" ]]; then
         moduleMPI=' other/mpi/openmpi/1.6.5-nag-5.3-907'
      fi
   elif [ "$fortranCompiler" == "GNU" ]; then
      if [ "$version" == "4.9.0" ]; then
         moduleFortran='other/comp/gcc-4.9.0'
         if [[ "$parallel" == "mpi"  || "$parallel" == "hybrid" ]]; then
           moduleMPI=' other/mpi/openmpi/1.7.3-gcc-4.9.0'
         fi
      elif [ "$version" == "4.8.1" ]; then
         moduleFortran='other/comp/gcc-4.8.1'
         if [[ "$parallel" == "mpi"  || "$parallel" == "hybrid" ]]; then
           moduleMPI=' other/mpi/openmpi/1.7.2-gcc-4.8.1-shared'
         fi
      else
         msg="$fortranCompiler version $version is not supported yet"
         echo -e "$msg\n\n" >> $DebugLog
      fi
   else
      abortNotify "$fortranCompiler is not supported yet"
   fi
   moduleList="other/cmake-2.8.11.2 $moduleFortran $moduleMPI"

   module purge
   if [ -n "$moduleList" ]; then  
      module load $moduleList
      if [ $? -ne 0 ]; then
         abortNotify "Failure loading "$moduleList
      fi
   fi
   echo " -- module list: "$moduleList
}

function doMake {
   if [ $# -lt 4 ]; then
      abortNotify "Invalid number of arguments ($#) in function 'doCmake'"
   fi
   local COM=$1
   local VER=$2
   local PAR=$3
   local MAK=$4

   # Avoid repeating some MPI combinations
   if [[ "$PAR" == "mpi"  || "$PAR" == "hybrid" ]]; then
      if [[ "$VER" == "13.0" && "$COM" == "INTEL" ]]; then
         return 0
      fi
      if [[ "$VER" == "13.1" && "$COM" == "INTEL" ]]; then
         return 0
      fi
   fi
   if [ "$BRANCH" == "pfunit_2.1.0" ]; then
      if [ "$VER" == "4.8.1" ]; then
         return 0
      fi
   fi

   # Default
   USEMPI="NO"
   USEOPENMP="NO"
   if [ "$PAR" == "omp" ]; then
      USEOPENMP="YES"
   fi
   if [ "$PAR" == "mpi" ]; then
      USEMPI="YES"
   fi
   if [ "$PAR" == "hybrid" ]; then
      USEMPI="YES"
      USEOPENMP="YES"
   fi

   makeLog=$LOG_DIR/${MAK}_${COM}_${VER}_PAR-${PAR}.log

   MAKE=/usr/bin/make

   if [ "$MAK" == "CMAKE" ]; then
     mkdir -p ${COM}_${VER}_PAR-${PAR}; cd ${COM}_${VER}_PAR-${PAR}
     echo " -- cmake -DMPI=$USEMPI -DOPENMP=$USEOPENMP ../"
     cmake -DMPI=$USEMPI -DOPENMP=$USEOPENMP ../ 1> $makeLog 2>&1
     $MAKE -j 8 tests 1>> $makeLog 2>&1
   else
     $MAKE --quiet distclean F90_VENDOR=$COM 1> $makeLog 2>&1
     echo " -- $MAKE tests F90_VENDOR=$COM MPI=$USEMPI OPENMP=$USEOPENMP"
     $MAKE -j 8 tests F90_VENDOR=$COM MPI=$USEMPI OPENMP=$USEOPENMP 1> $makeLog 2>&1
   fi
   result=`find . -name tests.x`
   if [ "$result" == "" ]; then
      echo " --- tests.x not found" 
      touch $SCR_DIR/.fail
      printf "$format" $COM $VER $PAR " | " "Build error" >> $EmailLog
      cd $SCR_DIR/$BRANCH
      return 0
   fi

   anyError=`grep 'make: ***' $makeLog`
   if [ -n "$anyError" ]; then
      echo " --- some tests failed" 
      touch $SCR_DIR/.fail
      printf "$format" $COM $VER $PAR " | " "Tests failed" >> $EmailLog
      cd $SCR_DIR/$BRANCH
      return 0
   fi

   printf "$format" $COM $VER $PAR " | " "OK" >> $EmailLog
   cd $SCR_DIR/$BRANCH

}

# -------------------------------------------------------------------
# MAIN PROGRAM
# -------------------------------------------------------------------

DebugLog=$LOG_DIR/debug.log
EmailLog=$LOG_DIR/email.log

declare -a F90_VERSIONS
declare -a GNU_VERSIONS
declare -a INTEL_VERSIONS

# We support two build types
MAKE_TYPE=(CMAKE GMAKE)

# Three compilers
COMPILERS=(GNU INTEL NAG)

# Compiler versions are separated into those that work
# with pfunit_2.1.0 and those that work with the master,
# development and release-3.0 branches.
INTEL_VERSIONS_master=(13.1 14.0)
INTEL_VERSIONS_2_1_0=(13.0 13.1 14.0)
GNU_VERSIONS_master=(4.9.0)
GNU_VERSIONS_2_1_0=(4.8.1 4.9.0)
NAG_VERSIONS=(5.3-907)

# serial, mpi, omp, omp+mpi
PARALLEL=(off mpi omp hybrid)

curDate=`date +"%Y_%m_%d"`

length=49
header="%-10s %-10s %-10s %3s %-12s\n"
format="%-10s %-10s %-10s %3s %-12s\n"

cd $SCR_DIR/$BRANCH

# BRANCH is an input argument to mainRegress.sh
if [[ "$BRANCH" == "pfunit_2.1.0" ]]; then
  GNU_VERSIONS=( "${GNU_VERSIONS_2_1_0[@]}" )
  INTEL_VERSIONS=( "${INTEL_VERSIONS_2_1_0[@]}" )
else
  GNU_VERSIONS=( "${GNU_VERSIONS_master[@]}" )
  INTEL_VERSIONS=( "${INTEL_VERSIONS_master[@]}" )
fi

for eachMake in "${MAKE_TYPE[@]}"; do
  echo "$eachMake"  >> $EmailLog; echo "=====" >> $EmailLog
  printf "$header" "Compiler" "Version" "Parallel" " | " "Result" >> $EmailLog
  printf -v line '%*s' "$length"; echo ${line// /-} >> $EmailLog

  for eachFC in "${COMPILERS[@]}"; do
    if [ "$eachFC" == "INTEL" ]; then
      F90_VERSIONS=( "${INTEL_VERSIONS[@]}" )
    elif [ "$eachFC" == "GNU" ]; then 
      F90_VERSIONS=( "${GNU_VERSIONS[@]}" )
    elif [ "$eachFC" == "NAG" ]; then 
      F90_VERSIONS=( "${NAG_VERSIONS[@]}" )
    fi

    for version in "${F90_VERSIONS[@]}"; do

      for eachPara in "${PARALLEL[@]}"; do
        logFile=$LOG_DIR/pFUnit_${eachFC}_${version}_MPI-${eachPara}.log
        echo " - TEST: $eachMake with Compiler=$eachFC, Version=$version, Parallel=$eachPara"
        setModule $eachFC $version $eachPara
        doMake $eachFC $version $eachPara $eachMake
      done # eachPara

    done # eachVersion
    F90_VERSIONS=( )

  done #eachCompiler
  echo >> $EmailLog

done #eachMake

exit 0
