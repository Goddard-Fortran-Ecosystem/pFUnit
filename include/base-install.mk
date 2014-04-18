
INCLUDE_DIR =$(PFUNIT)/include
LIB_DIR     =$(PFUNIT)/lib
MOD_DIR     =$(PFUNIT)/mod

# These two are not part of an installation.
# SRC_DIR     =$(PFUNIT)/source
# TESTS_DIR   =$(PFUNIT)/tests

# Read in compile configuration to help set flags like -gomp for GNU.
include $(INCLUDE_DIR)/configuration.mk

# Set the required file extensions.
include $(INCLUDE_DIR)/extensions.mk

# F90 Vendor common elements (override below)
# FFLAGS ?=
D=-D
I=-I
MOD=-I
F90_HAS_CPP=YES
DEBUG_FLAGS =-g

# Include the compiler-specific options.
COMPILER ?= COMPILER_NOT_SET
include $(INCLUDE_DIR)/$(COMPILER).mk

FFLAGS += $I$(INCLUDE_DIR)

ifeq ($(BUILDROBUST),YES)
  FPPFLAGS += $DBUILD_ROBUST
  CPPFLAGS += -DBUILD_ROBUST
endif

# include/driver.F90 needs both BUILD_ROBUST
ifneq ($(USEMPI),YES)
  FC=$(F90)
else
  FC=$(MPIF90)
endif

ifeq ($(F90_HAS_CPP),YES)
%$(OBJ_EXT): %.F90
	$(FC) -c $(FFLAGS) $(CPPFLAGS) -o $@ $<
else
%$(OBJ_EXT):%.F90
	@$(CPP) $(CPPFLAGS) $(CPPFLAGS) $< > $*_cpp.F90
	$(FC) -c $(FFLAGS)  $*_cpp.F90 -o $@
	$(RM) $*_cpp.F90
endif

.PHONY: clean distclean echo

clean: local-base0-clean

local-base0-clean:
	$(RM) *$(OBJ_EXT) *.mod *.i90 *~ *_cpp.F90 *.tmp *.s
	$(RM) -r *.dSYM

distclean: local-base0-distclean

local-base0-distclean: clean
	$(RM) *$(LIB_EXT) *$(EXE_EXT)

echo:
	@echo COMPILER: $(COMPILER)
	@echo FC:	$(FC)
	@echo USEMPI:   $(USEMPI)
	@echo FFLAGS:   $(FFLAGS)
	@echo FPPFLAGS: $(FPPFLAGS)
	@echo CPPFLAGS: $(CPPFLAGS)


