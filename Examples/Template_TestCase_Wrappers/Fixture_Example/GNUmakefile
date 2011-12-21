# Important: specify a serial PFUNIT installation.
F90 ?= ifort
FC = $(F90)

PFUNIT_TEST_DIRECTORIES =.
PFUNIT_APP_OBJECTS=  # none in this simple example

.PHONY:	all

all:	tests

%.o : %.F90
	$(FC) -c $(F90FLAGS) $(PFUNIT_FFLAGS) $<

clean:
	$(RM) *.o
	$(RM) *.mod
	$(RM) .pFUnitLog

distclean: clean
	$(MAKE) pfunitdistclean


export PFUNIT
include $(PFUNIT)/include/pFUnit.makefile

