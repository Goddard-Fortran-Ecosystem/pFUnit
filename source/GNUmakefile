TOP = ..
include ../include/base.mk
.PHONY: all

all: libpfunit.a

SRCS = $(filter-out BasicElements.F90,$(wildcard *.F90))
OBJS = $(SRCS:%.F90=%.o)

EXCLUDE_SRCS = MpiContext.F90 MpiStubs.F90 MpiTestCase.F90
ifneq ($(MPI),YES)
   SRCS_SERIAL=$(filter-out $(EXCLUDE_SRCS),$(SRCS))
   OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%.o)
endif

ifeq ($(MPI),YES)
   libpfunit.a: $(OBJS)
else
   libpfunit.a: $(OBJS_SERIAL)
endif
	ar -r $@ $?
	-@ranlib $@

Exception.o : ParallelContext.o
Assert.o: Exception.o
AssertReal.o: StringUtilities.o


SurrogateTestCase.o:
TestResult.o: SurrogateTestCase.o TestListener.o Exception.o
Test.o: TestResult.o

TestCase.o: Test.o TestResult.o SurrogateTestCase.o

TestCase.o: SerialContext.o
DynamicTestCase.o: TestCase.o

DebugListener.o: TestListener.o

TestSuite.o: Test.o TestMethod.o
TestRunner.o: TestSuite.o
TestRunner.o: ResultPrinter.o

ResultPrinter.o: TestFailure.o

TestFailure.o: Exception.o
TestResult.o: TestFailure.o

MockRepository.o:

pFUnit.o: TestRunner.o TestSuite.o
pFUnit.o: ParallelContext.o

ifeq ($(MPI),YES)
   pFUnit.o: MpiContext.o
endif

pFUnitPackage.o: pFUnit.o
