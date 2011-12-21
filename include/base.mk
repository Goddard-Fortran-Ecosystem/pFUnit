SRC_DIR     =$(TOP)/source
TESTS_DIR   =$(TOP)/tests
INCLUDE_DIR =$(TOP)/include
LIB_DIR     =$(TOP)/source
MOD_DIR     =$(TOP)/source

include $(INCLUDE_DIR)/intel.mk


F90FLAGS += $I$(INCLUDE_DIR)

ifneq ($(MPI),YES)
  MPIF90=$(F90)
else
  MPIF90 ?=mpif90
endif

%.o: %.F90
	$(MPIF90) -c $(F90FLAGS) -o $@ $<


.PHONY: clean distclean

clean:
	$(RM) *.o *.mod

distclean: clean
	$(RM) *a *.x *~
