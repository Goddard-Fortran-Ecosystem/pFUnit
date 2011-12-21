
#----------------------------------------------------------------------------------
# The following variables can be customized by a user for their project.
#----------------------------------------------------------------------------------
PFUNIT ?=			      # location of _installed_ pFUnit files
PFUNIT_TEST_DIRECTORIES ?=./tests     # location of test files
PFUNIT_TEST_PATTERNS    ?=test% Test% # list of patterns that distinguish modules with tests
PFUNIT_TEST_EXECUTABLE  ?=tests.x     # name of test executable
PFUNIT_APP_OBJECTS      ?=            # object or library for software under test (the app)
PFUNIT_SUITE_LIST       ?=suite_list
PFUNIT_WRAP_SUFFIX      ?=_wrap
# variable that compilation rule uses
PFUNIT_FFLAGS_VARIABLE  ?=F90FLAGS

#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# The following variables must be set according to the development environment.
# $I and $MOD are the same for most Fortran compilers, but the mechanism to place
# quotes around a token in the C preprocessor is rather variable.
#----------------------------------------------------------------------------------
# Defaults assume Intel compier "ifort"
FC  ?=ifort
I   ?=I
MOD ?=I
PFUNIT_CPP_STRINGIFY    ?=QUOTES        # other options are NESTED_QUOTES & POUND
#----------------------------------------------------------------------------------

#----------------------------------------------------------------------------------
# MPI options - default is for serial usage.
#----------------------------------------------------------------------------------
USE_MPI ?=NO       # set to yes to run/build MPI tests
MPI_FC  ?=mpif90
MPIRUN  ?=mpirun   # MPI launcher
PFUNIT_NPES ?=1        # number of mpi processes required to run tests
#----------------------------------------------------------------------------------

include $(PFUNIT)/include/hidden.makefile

