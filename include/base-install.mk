
SRC_DIR     =$(PFUNIT)/source
TESTS_DIR   =$(PFUNIT)/tests
INCLUDE_DIR =$(PFUNIT)/include
LIB_DIR     =$(PFUNIT)/lib
MOD_DIR     =$(PFUNIT)/mod

# Set the required file extensions.
include $(INCLUDE_DIR)/extensions.mk

# Include the compiler-specific options.
include $(INCLUDE_DIR)/$(COMPILER).mk

F90FLAGS += $I$(INCLUDE_DIR)

ifneq ($(MPI),YES)
  FC=$(F90)
else
  FC=$(MPIF90)
endif

ifeq ($(F90_HAS_CPP),YES)
%$(OBJ_EXT): %.F90
	$(FC) -c $(F90FLAGS) $(CPPFLAGS) -o $@ $<
else
%$(OBJ_EXT):%.F90
	@$(CPP) $(CPPFLAGS) $(CPPFLAGS) $< > $*_cpp.F90
	$(FC) -c $(F90FLAGS)  $*_cpp.F90 -o $@
	$(RM) $*_cpp.F90
endif

.PHONY: clean distclean

clean:
	$(RM) *$(OBJ_EXT) *.mod *.i90 *~ *_cpp.F90 *.tmp

distclean: clean
	$(RM) *$(LIB_EXT) *$(EXE_EXT)
