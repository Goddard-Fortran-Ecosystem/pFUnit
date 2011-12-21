# Important: specify a serial PFUNIT installation.

# Check for the PFUNIT environment variable
ifndef PFUNIT
$(error The environment variable "PFUNIT" must be given a value which is the \
path for the installation of pFUnit.)
endif

F90 ?= ifort
FC = $(F90)

ROOT_DIR = ..
LIB_DIR = $(ROOT_DIR)/lib
MOD_DIR = $(ROOT_DIR)/mod

F90FLAGS = -I. -I$(MOD_DIR)

PFUNIT_TEST_DIRECTORIES =.
PFUNIT_LFLAGS           = -L$(LIB_DIR) -lsuminverse

.PHONY: all clean distclean

%.o : %.F90
	$(FC) -c $(F90FLAGS) $<

all:	tests

clean:
	$(RM) *.o
	$(RM) *.mod
	$(RM) .pFUnitLog

distclean: clean
	$(MAKE) pfunitdistclean

export PFUNIT
include $(PFUNIT)/include/pFUnit.makefile
