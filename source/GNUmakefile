TOP = ..
include ../include/base.mk
.PHONY: all

all: libf2kunit.a

SRCS = $(filter-out BasicElements.F90,$(wildcard *.F90))
OBJS = $(SRCS:%.F90=%.o)

EXCLUDE_SRCS = MpiContext.F90 MpiStubs.F90 MpiTestCase.F90
ifneq ($(MPI),YES)
   SRCS_SERIAL=$(filter-out $(EXCLUDE_SRCS),$(SRCS))
   OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%.o)
endif

ifeq ($(MPI),YES)
   libf2kunit.a: $(OBJS)
else
   libf2kunit.a: $(OBJS_SERIAL)
endif
	ar -r $@ $?
	-@ranlib $@

Exception.o : ParallelContext.o
Assert.o: Exception.o
AssertReal.o: StringUtilities.o


SurrogateTestCase.o:
TestResult.o: SurrogateTestCase.o
Test.o: TestResult.o

TestCase.o: Test.o TestResult.o SurrogateTestCase.o

#BasicElements.o: TestFailure.o
#BasicElements.o: Test.inc TestImplementation.inc
#BasicElements.o: GenericTestCase.inc GenericTestCaseImplementation.inc
#BasicElements.o: TestResult.inc TestResultImplementation.inc
#BasicElements.o: TestInterface.inc
#BasicElements.o: GenericTestCaseInterface.inc
#BasicElements.o: ParallelContext.o
#BasicElements.o: TestListener.o

TestCase.o: SerialContext.o
DynamicTestCase.o: TestCase.o

DebugListener.o: TestListener.o

TestSuite.o: Test.o

TestRunner.o: TestSuite.o
TestRunner.o: ResultPrinter.o

ResultPrinter.o: TestFailure.o

TestFailure.o: Exception.o
TestResult.o: TestFailure.o

MockRepository.o:

F2kUnit.o: TestRunner.o TestSuite.o
F2kUnit.o: ParallelContext.o

ifeq ($(MPI),YES)
   F2kUnit.o: MpiContext.o
endif
