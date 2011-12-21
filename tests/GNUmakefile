TOP = ..
include ../include/base.mk
.PHONY: all

all: tests
VPATH = . ../source
EXE = tests.x
tests: $(EXE)

SRCS = $(wildcard *.F90)
OBJS = $(SRCS:%.F90=%.o)

SRCS_SERIAL=$(filter-out MPI%,$(SRCS))
OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%.o)

F90FLAGS +=$M$(MOD_DIR)

LIBS = $L$(LIB_DIR) -lf2kunit
ifeq ($(MPI),YES)
  LIBS += -lmpi
endif

$(EXE): $(OBJS) libf2kunit.a
	$(MPIF90) -o $@ $(OBJS) $(LIBS)

Test_SimpleTestCase.o: SimpleTestCase.o
Test_FixtureTestCase.o: FixtureTestCase.o
Test_TestResult.o: SimpleTestCase.o
selfTests.o: Test_StringUtilities.o Test_Exception.o Test_Assert.o Test_MpiTestCase.o Test_TestResult.o Test_TestSuite.o Test_AssertionLocation.o Test_AssertReal.o Test_SimpleTestCase.o Test_SimpleTestMethod.o Test_FixtureTestCase.o 
