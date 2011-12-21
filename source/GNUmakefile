TOP = ..
include ../include/base.mk
.PHONY: all

all: libf2kunit.a

SRCS = $(wildcard *.F90)
OBJS = $(SRCS:%.F90=%.o)

SRCS_SERIAL=$(filter-out MPI%,$(SRCS))
OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%.o)

libf2kunit.a: $(OBJS)
	ar -r $@ $?
	-@ranlib $@

libmpif2kunit.a: $(OBJS)
	ar -r $@ $?
	-@ranlib $@


Exception.o : ParallelContext.o
Assert.o: Exception.o
AssertReal.o: StringUtilities.o

BasicElements.o: TestFailure.o
BasicElements.o: Test.inc TestImplementation.inc
BasicElements.o: GenericTestCase.inc GenericTestCaseImplementation.inc
BasicElements.o: TestResult.inc TestResultImplementation.inc
BasicElements.o: TestInterface.inc
BasicElements.o: GenericTestCaseInterface.inc
BasicElements.o: ParallelContext.o
BasicElements.o: TestListener.o

TestCase.o: SerialContext.o
DynamicTestCase.o: TestCase.o

TestSuite.o: BasicElements.o

TestRunner.o: TestSuite.o
TestRunner.o: ResultPrinter.o

ResultPrinter.o: TestFailure.o

TestFailure.o: Exception.o
TestResult.o: TestFailure.o

F2kUnit.o: TestRunner.o TestSuite.o
F2kUnit.o: ParallelContext.o
F2kUnit.o: MpiContext.o