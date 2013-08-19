.PHONY: clean distclean tests

TOP_DIR := $(shell pwd)
SRC_DIR=$(TOP_DIR)/src
TEST_DIR=$(TOP_DIR)/tests

VPATH = . $(SRC_DIR) $(TEST_DIR)

ifeq ($(F90),nagfor)
  FFLAGS += -fpp
endif

EXE = tests.x

all: $(EXE)
	mpirun -np 4 ./$(EXE)

$(EXE): testSuites.inc myTests.pf Halo.F90 SUT tests
	$(MPIF90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -Itests $(PFUNIT)/include/driver.F90 $(TEST_DIR)/*.o $(SRC_DIR)/*.o -L$(PFUNIT)/lib -lpfunit -DUSE_MPI $(FFLAGS)

clean:
	make -C $(SRC_DIR) clean
	make -C $(TEST_DIR) clean

distclean: clean
	rm *.x

SUT:
	make -C $(SRC_DIR) SUT
tests:
	make -C $(TEST_DIR) tests

export SRC_DIR
export TEST_DIR
