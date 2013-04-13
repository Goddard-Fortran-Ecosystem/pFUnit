TOP = ..
include ../include/base.mk
.PHONY: all

all: libpfunit.a

SRCS = $(filter-out BasicElements.F90,$(wildcard *.F90))
OBJS = $(SRCS:%.F90=%.o)

EXCLUDE_MPI_SRCS = MpiContext.F90 MpiStubs.F90 MpiTestCase.F90 MpiTestMethod.F90

ifneq ($(MPI),YES)
   SRCS_SERIAL=$(filter-out $(EXCLUDE_MPI_SRCS),$(SRCS))
   OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%.o)
endif

ifeq ($(MPI),YES)
   libpfunit.a: $(OBJS)
else
   libpfunit.a: $(OBJS_SERIAL)
endif
	ar -r $@ $?
	-@ranlib $@

UnixProcess.o : StringUtilities.o UnixPipeInterfaces.o

Exception.o : ParallelContext.o SourceLocation.o
ParallelException.o: ParallelContext.o Exception.o
AssertBasic.o: Exception.o StringUtilities.o
AssertInteger.o: Exception.o
Assert.o: Exception.o AssertBasic.o AssertInteger.o
AssertReal.o: Assert.o StringUtilities.o


SurrogateTestCase.o:
TestResult.o: SurrogateTestCase.o TestListener.o Exception.o
Test.o: TestResult.o

TestCase.o: Test.o TestResult.o SurrogateTestCase.o
TestCase.o: SerialContext.o
ParameterizedTestCase.o: TestCase.o

DynamicTestCase.o: TestCase.o

DebugListener.o: TestListener.o

TestSuite.o: Test.o TestMethod.o

TestCaseDecorator.o :: TestCase.o

BaseTestRunner.o: TestListener.o Test.o ParallelContext.o
TestRunner.o: TestSuite.o BaseTestRunner.o ResultPrinter.o TestResult.o
SubsetRunner.o: BaseTestRunner.o TestResult.o TestSuite.o
RobustRunner.o: BaseTestRunner.o TestResult.o TestSuite.o RemoteProxyTestCase.o UnixProcess.o
RemoteProxyTestCase.o: TestCase.o UnixProcess.o



ResultPrinter.o: TestFailure.o

TestFailure.o: Exception.o
TestResult.o: TestFailure.o

MockRepository.o:

pFUnit.o: TestRunner.o TestSuite.o
pFUnit.o: ParallelContext.o

ifeq ($(MPI),YES)
   pFUnit.o: MpiContext.o

   MpiTestCase.o : TestCase.o MpiContext.o ParallelException.o
   MpiTestCase.o : ParameterizedTestCase.o
   MpiTestMethod.o: MpiTestCase.o
endif

pFUnitPackage.o: pFUnit.o
