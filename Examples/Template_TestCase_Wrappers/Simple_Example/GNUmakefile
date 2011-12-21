# Important: specify a serial PFUNIT installation.
F90 ?= ifort
FC = $(F90)
PFUNIT_TEST_DIRECTORIES =.
PFUNIT_APP_OBJECTS=VectorOperations.o

%.o : %.F90
	$(FC) -c $(F90FLAGS) $<

.PHONY: all clean distclean

all:	tests

clean:
	$(RM) *.o
	$(RM) *.mod
	$(RM) .pFUnitLog

distclean: clean
	$(MAKE) pfunitdistclean

export PFUNIT
include $(PFUNIT)/include/pFUnit.makefile
