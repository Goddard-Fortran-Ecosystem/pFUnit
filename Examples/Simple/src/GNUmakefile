.PHONY: clean

# Support for Windows/CYGWIN
ifeq ($(UNAME),Windows)
	TOP_DIR := ..
	# Since this is a Windows compilation, add the Windows define.
	FFLAGS += -DWindows
	ifeq ($(F90),nagfor)
		FFLAGS += -fpp
	endif
	ifeq ($(F90),ifort)
		FFLAGS += /nologo
	endif
endif

SUT: add$(OBJ_EXT) addComplex$(OBJ_EXT)

%$(OBJ_EXT): %.F90
	$(F90) -c $(FFLAGS) $<

clean:
	$(RM) *$(OBJ_EXT) *.mod

