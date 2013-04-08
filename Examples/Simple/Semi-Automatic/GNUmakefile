.PHONY: clean tests all

TOP_DIR := $(shell pwd)
SRC_DIR=$(TOP_DIR)/src
TEST_DIR=$(TOP_DIR)/tests

VPATH = . $(SRC_DIR) $(TEST_DIR)

EXE = tests.x

all: $(EXE)
	./$(EXE)


$(EXE): testSuites.inc myTests.pf add.F90 SUT tests
	$(F90) -o $@ -I$(PFUNIT)/mod -I$(PFUNIT)/include -Itests $(PFUNIT)/include/driver.F90 $(TEST_DIR)/*.o $(SRC_DIR)/*.o -L$(PFUNIT)/lib -lpfunit

clean:
	make -C $(SRC_DIR) clean
	make -C $(TEST_DIR) clean

SUT:
	make -C $(SRC_DIR) SUT
tests:
	make -C $(TEST_DIR) tests

export SRC_DIR
export TEST_DIR
