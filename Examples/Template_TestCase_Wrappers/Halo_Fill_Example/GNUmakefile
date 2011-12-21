# Important: specify PFUNIT installation that was built with MPI.
PFUNIT_USE_MPI=YES

MPI_FC= mpif90
PFUNIT_NPES=6
FC=mpif90
PFUNIT_TEST_DIRECTORIES =.
PFUNIT_APP_OBJECTS=Halo_mod.o

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


