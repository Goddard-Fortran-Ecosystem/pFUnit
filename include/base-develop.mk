SRC_DIR     =$(TOP)/source
TESTS_DIR   =$(TOP)/tests
INCLUDE_DIR =$(TOP)/include
LIB_DIR     =$(TOP)/source
MOD_DIR     =$(TOP)/source

include $(INCLUDE_DIR)/$(COMPILER).mk

F90FLAGS += $I$(INCLUDE_DIR)

ifneq ($(MPI),YES)
  FC=$(F90)
else
  FC=$(MPIF90)
endif

ifeq ($(F90_HAS_CPP),YES)
%.o: %.F90
	$(FC) -c $(F90FLAGS) $(CPPFLAGS) -o $@ $<
else
%.o:%.F90
	@$(CPP) $(CPPFLAGS) $(CPPFLAGS) $< > $*_cpp.F90
	$(FC) -c $(F90FLAGS)  $*_cpp.F90 -o $@
	$(RM) $*_cpp.F90
endif

.PHONY: clean distclean

clean:
	$(RM) *.o *.mod *.i90 *~ *_cpp.F90

distclean: clean
	$(RM) *.a *.x
