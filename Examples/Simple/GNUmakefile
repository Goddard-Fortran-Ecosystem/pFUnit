.PHONY: clean tests all

# Determine operating system, architecture and compiler
# automatically if possible
UNAME ?=$(shell uname)
ifeq ($(UNAME),)
	UNAME =UNKNOWN
else
# Check for Windows/CYGWIN compilation.
ifneq (,$(findstring CYGWIN,$(UNAME)))
	UNAME =Windows
endif
endif

ifneq ($(UNAME),Windows)
	TOP_DIR := $(shell pwd)
	SRC_DIR=$(TOP_DIR)/src
	TEST_DIR=$(TOP_DIR)/tests
else
	# When using CYGWIN, then relative paths have to be used.
	PFUNIT := ../../../pfunit
	TOP_DIR := $(shell pwd)
	SRC_DIR=src
	TEST_DIR=tests
endif

VPATH = . $(SRC_DIR) $(TEST_DIR)

# Decide the file extensions.
include $(PFUNIT)/include/extensions.mk

ifeq ($(F90),nagfor)
	FFLAGS += -fpp
endif
ifeq ($(UNAME),Windows)
	FFLAGS += -DWindows
	ifeq ($(F90),ifort)
		FFLAGS += /nologo
	endif
endif

EXE = tests$(EXE_EXT)
ifneq ($(UNAME),Windows)
	LIBS = -L$(PFUNIT)/lib -lpfunit 
else
	LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)
endif

all: $(EXE)
	./$(EXE)


$(EXE): testSuites.inc myTests.pf add.F90 addComplex.F90 SUT tests
	$(F90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -Itests $(PFUNIT)/include/driver.F90 $(TEST_DIR)/*$(OBJ_EXT) $(SRC_DIR)/*$(OBJ_EXT) $(LIBS) $(FFLAGS)

clean:
	make -C $(SRC_DIR) clean
	make -C $(TEST_DIR) clean
	rm -f $(EXE) *$(OBJ_EXT)

SUT:
	make -C $(SRC_DIR) SUT
tests: SUT
	make -C $(TEST_DIR) tests

ifeq ($(UNAME),Windows)
	export PFUNIT
endif
export SRC_DIR
export TEST_DIR
export OBJ_EXT
export LIB_EXT
export EXE_EXT
