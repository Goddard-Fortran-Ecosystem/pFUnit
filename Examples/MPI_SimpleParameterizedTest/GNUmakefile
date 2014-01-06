.PHONY: clean distclean tests

TOP_DIR := $(shell pwd)
TEST_DIR=$(TOP_DIR)/tests

VPATH = . $(TEST_DIR)

# Decide the file extensions.
include $(PFUNIT)/include/extensions.mk

ifeq ($(F90),nagfor)
  FFLAGS += -fpp
endif

EXE = tests$(EXE_EXT)

all: $(EXE)
	mpirun -np 4 ./$(EXE)

$(EXE): testSuites.inc parameterizedTests.pf tests
	$(MPIF90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -Itests $(PFUNIT)/include/driver.F90 $(TEST_DIR)/*$(OBJ_EXT) -L$(PFUNIT)/lib -lpfunit -DUSE_MPI $(FFLAGS)

clean:
	make -C $(TEST_DIR) clean

distclean: clean
	rm *$(EXE_EXT)

tests:
	make -C $(TEST_DIR) tests

export TEST_DIR
export OBJ_EXT
export LIB_EXT
export EXE_EXT