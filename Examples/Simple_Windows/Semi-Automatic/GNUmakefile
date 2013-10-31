.PHONY: clean tests all

SRC_DIR=src
TEST_DIR=tests

VPATH = . $(SRC_DIR) $(TEST_DIR)

# When using CYGWIN, then relative paths have to be used.
# Set the relative parh to PFUNIT here.
PFUNIT :=  ../../../../pfunit


# Decide the file extensions.
include $(PFUNIT)/include/extensions.mk

# Since this is a Windows compilation, add the Windows define.
FFLAGS += -DWindows

ifeq ($(F90),nagfor)
  FFLAGS += -fpp
endif
ifeq ($(F90),ifort)
  FFLAGS += -fpp /nologo
endif

EXE = tests$(EXE_EXT)

all: $(EXE)
	./$(EXE)

LIBS = $(PFUNIT)/lib/libpfunit$(LIB_EXT)

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

export PFUNIT
export SRC_DIR
export TEST_DIR
export OBJ_EXT
export LIB_EXT
export EXE_EXT
