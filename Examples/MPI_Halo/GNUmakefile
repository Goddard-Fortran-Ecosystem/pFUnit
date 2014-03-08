.PHONY: clean distclean tests

TOP_DIR := $(shell pwd)
SRC_DIR=$(TOP_DIR)/src
TEST_DIR=$(TOP_DIR)/tests

VPATH = . $(SRC_DIR) $(TEST_DIR)

#
include $(PFUNIT)/include/base.mk

ifeq ($(F90),nagfor)
  FFLAGS += -fpp
endif

EXE = tests$(EXE_EXT)

all: $(EXE)
	mpirun -np 4 ./$(EXE)

$(EXE): testSuites.inc myTests.pf Halo.F90 SUT tests
	$(MPIF90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -Itests $(PFUNIT)/include/driver.F90 $(TEST_DIR)/*$(OBJ_EXT) $(SRC_DIR)/*$(OBJ_EXT) -L$(PFUNIT)/lib -lpfunit $(LIBS) -DUSE_MPI $(FFLAGS)

echo:
	@echo PFUNIT: $(PFUNIT)
	@echo BUILDROBUST: \"$(BUILDROBUST)\"
	@echo COMPILER: $(COMPILER)
	@echo FFLAGS: $(FFLAGS)
	@echo FPPFLAGS: $(FPPFLAGS)
	@echo FC: $(FC)
	@echo MPI: $(MPI)
	@echo CPPFLAGS: $(CPPFLAGS)
	@echo LIBS: $(LIBS)

clean:
	make -C $(SRC_DIR) clean
	make -C $(TEST_DIR) clean
	$(RM) *.o *.mod *.i90

distclean: clean
	rm *$(EXE_EXT)

SUT:
	make -C $(SRC_DIR) SUT

.DEFAULT_GOAL = tests
tests:
	make -C $(TEST_DIR) tests

export SRC_DIR
export TEST_DIR
export OBJ_EXT
export LIB_EXT
export EXE_EXT
