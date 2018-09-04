export FUFLAGS=-markers

F90 ?=ftn
MPIF90 ?= ftn

I=-I
M=-I
L=-L

# Cray compiler only exists on Cray machines so no Windows support.
FFLAGS += -G0 -K trap=fp,unf

ifeq ($(USEOPENMP),YES)
FFLAGS += -h omp
endif


# Common command line options.

# The Cray compiler always creates a <file>.i file when run in preprocessor
# mode. None of this stuff seems to be used so I'm commenting it out for
# the moment.
#
# F90_PP_ONLY = -eF
# F90_PP_OUTPUT = ; mv <file>i 

CPPFLAGS +=-DCray
FPPFLAGS +=-DCray

