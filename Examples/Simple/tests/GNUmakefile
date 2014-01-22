.PHONY: clean tests

SRCS = $(wildcard *.pf)
OBJS = $(SRCS:.pf=$(OBJ_EXT))

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

tests: $(OBJS)

testSuites.inc: $(SRCS)

FFLAGS += -I$(SRC_DIR) -I$(PFUNIT)/mod

ifneq ($(UNAME),Windows)
	ifeq ($(F90),nagfor)
	   FFLAGS += -fpp
	endif
endif

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $<  $@

%$(OBJ_EXT): %.F90
	$(F90) -c $(FFLAGS) $<

clean:
	$(RM) *$(OBJ_EXT) *.mod
