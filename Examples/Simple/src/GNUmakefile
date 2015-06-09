.PHONY: clean
include $(PFUNIT)/include/base.mk

# Support for Windows/CYGWIN
ifeq ($(UNAME),Windows)
	TOP_DIR := ..
	# Since this is a Windows compilation, add the Windows define.
	FFLAGS += -DWindows
	ifeq ($(FC),nagfor)
		FFLAGS += -fpp
	endif
	ifeq ($(FC),ifort)
		FFLAGS += /nologo
	endif
endif

SUT: add$(OBJ_EXT) addComplex$(OBJ_EXT) node$(OBJ_EXT)

%$(OBJ_EXT): %.F90
	$(FC) -c $(FFLAGS) $(FPPFLAGS) $<

clean: local-E1-clean

local-E1-clean:
	$(RM) *$(OBJ_EXT) *.mod

