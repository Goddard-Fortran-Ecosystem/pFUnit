# Set the extension variables to their Linux defaults.
OBJ_EXT ?= .o
LIB_EXT ?= .a
EXE_EXT ?= .x

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
	$(RM) *$(LIB_EXT) *$(EXE_EXT) dependencies.inc
