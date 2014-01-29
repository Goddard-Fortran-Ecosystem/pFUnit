.PHONY: clean tests all

include $(PFUNIT)/include/base.mk

FFLAGS += -DBUILD_ROBUST
FPPFLAGS += -DBUILD_ROBUST
CPPFLAGS += -DBUILD_ROBUST

TOP_DIR := $(shell pwd)
SRC_DIR=$(TOP_DIR)/src
TEST_DIR=$(TOP_DIR)/tests

VPATH = . $(SRC_DIR) $(TEST_DIR)

# Decide the file extensions.
include $(PFUNIT)/include/extensions.mk

ifeq ($(F90),nagfor)
  FFLAGS += -fpp
endif

EXE = tests$(EXE_EXT)

# NOTE: use the -robust flag
all: $(EXE)
	./$(EXE) -robust


$(EXE): testSuites.inc myTests.pf add.F90 SUT tests
	$(F90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -Itests $(PFUNIT)/include/driver.F90 $(TEST_DIR)/*$(OBJ_EXT) $(SRC_DIR)/*$(OBJ_EXT) -L$(PFUNIT)/lib -lpfunit $(LIBS) $(FFLAGS) #-DUSE_MPI

clean:
	make -C $(SRC_DIR) clean
	make -C $(TEST_DIR) clean
	rm -f tests.x

SUT:
	make -C $(SRC_DIR) SUT
tests: SUT
	make -C $(TEST_DIR) tests

export SRC_DIR
export TEST_DIR
export OBJ_EXT
export LIB_EXT
export EXE_EXT
