# Expand rules defined in parent makefile
$(eval $(F90-Rule))
$(eval $(M4-Rule))
$(eval $(CM4-Rule))

SRCS_C   =$(wildcard *.c)         # compile with C 
SRCS_CM4 =$(wildcard *.cm4)     # preprocess with m4 then compile with C
SRCS_F90 =$(wildcard *.F90)       # compile with F90 (and CPP)
SRCS_M4  =$(wildcard *.m4)       # preprocess with m4 then compile with F90

# Ignore these sources unless using MPI
EXCLUDE_SRCS ?=
ifneq ($(MPI),YES)
  EXCLUDE_SRCS += Test_MPI_WasRun_mod.F90 test_MpiClient_mod.F90 test_MpiServer_mod.F90 test_MpiController_mod.F90
  EXCLUDE_SRCS += test_MpiTestCase_mod.F90 test_newMpiTestCase_mod.F90 MpiWasRun_mod.F90 TestInfo_mod.F90
endif
ifneq ($(ESMF),YES)
  EXCLUDE_SRCS += esmf_stub.F90
endif
ifneq ($(DSO),YES)
  EXCLUDE_SRCS += test_SharedObjLibUtilities_mod.F90 test_pFUnitUseCase_mod.F90 test_pFUnitDriver_mod.F90
endif

ALL_SRCS =$(SRCS_C) $(SRCS_CM4) $(SRCS_F90) $(SRCS_M4)
ALL_OBJS =$(SRCS_C:.c=.o) $(SRCS_CM4:.cm4=.o) $(SRCS_F90:.F90=.o) $(SRCS_M4:.m4=.o)
SRCS =$(filter-out $(EXCLUDE_SRCS),$(ALL_SRCS))

OBJS =$(addsuffix .o,$(basename $(SRCS)))
TestParameterizedTestCase_mod.o : TestParamType_mod.o
TestSuiteTest_mod.o : TestParamType_mod.o
TestSuiteTest_mod.o : TestWasRun_mod.o
ifeq ($(MPI),YES)
  TestSuiteTest_mod.o : Test_MPI_WasRun_mod.o
  newMpiTestCase.o: Test_MPI_WasRun_mod.o
endif


Main.o: TestWasRun_mod.o		TestSuiteTest_mod.o			\
        TestParameterizedTestCase_mod.o	TestInternalFile_mod.o		TestTestResult_mod.o			\
        test_AssertString_mod.o		test_AssertArray_mod.o		test_AssertInteger_mod.o	test_AssertReal_mod.o			\
	test_Exception_mod.o		test_BaseAddress_mod.o		test_ProcedurePointer_mod.o		\
	test_IO_Utilities_mod.o		TestParamType_mod.o		TestCaseTest_mod.o test_FortranNameMangle_mod.o	\
	test_cString_mod.o				\
	test_AssertComplex_mod.o	\
	reflection.h

ifeq ($(DSO),YES)
Main.o:	test_SharedObjLibUtilities_mod.o	test_pFUnitDriver_mod.o		\
	test_pFUnitUseCase_mod.o
endif
ifeq ($(MPI),YES)
test_MpiTestCase_mod.o: MpiWasRun_mod.o
Main.o: test_MpiTestCase_mod.o			\
	test_MpiClient_mod.o		test_MpiServer_mod.o			\
	test_newMpiTestCase_mod.o TestInfo_mod.o
test_newMpiTestCase_mod.o: Test_MPI_WasRun_mod.o
endif


G95LIBS=/Volumes/Data/clune/Desktop/Unsupported\ Apps/g95-install/lib/gcc-lib/powerpc-apple-darwin6.8/4.0.3
DSO_SUFFIX ?=dylib
alltests.$(DSO_SUFFIX): test_Exception_mod.o	test_ProcedurePointer_mod.o	\
			test_IO_Utilities_mod.o	test_AssertString_mod.o		\
			test_AssertArray_mod.o	test_AssertInteger_mod.o	\
			test_AssertReal_mod.o	test_AssertComplex_mod.o	\
			TestTestResult_mod.o	TestCaseTest_mod.o		\
			TestSuiteTest_mod.o	TestWasRun_mod.o TestParamType_mod.o	\
			test_cString_mod.o	test_FortranNameMangle_mod.o

ifeq ($(DSO),YES)
alltests.$(DSO_SUFFIX): test_SharedObjLibUtilities_mod.o			\
			test_pFUnitUseCase_mod.o				\
			test_pFUnitDriver_mod.o
endif

ifeq ($(MPI),YES)
alltests.$(DSO_SUFFIX): Test_MPI_WasRun_mod.o 
endif


alltests.$(DSO_SUFFIX):
	$(DSO_MECHANISM) -L$(SOURCE_DIR) -lpfunit

sharedLibs:
	$(MAKE) -C testData libfoo.$(DSO_SUFFIX) libfoo2.$(DSO_SUFFIX) libbar.$(DSO_SUFFIX) libpFUnitTestsA.$(DSO_SUFFIX) libpFUnitTestsB.$(DSO_SUFFIX)
ifeq ($(MPI),YES)
	$(MAKE) -C testData libmpitests.$(DSO_SUFFIX)
endif

EXE = tests.x

$(EXE): $(OBJS)
	$(F90) $(OBJS) $(FFLAGS) -o $@ $(LDFLAGS)

ifeq ($(MPI),YES)

PFUNIT_MPI_SERVER?=mpi_pFUnit.x
$(PFUNIT_MPI_SERVER): tests.x
	-@rm -rf $@
	-@cd ..; ln -fs tests/tests.x $@
all: $(PFUNIT_MPI_SERVER) testData/square.x
testData/square.x: testData/squareServer.F90
	$(MAKE) -C testData square.x

endif

all: $(EXE)
ifeq ($(DSO),YES)
  all $(EXE): sharedLibs $(SOURCE_DIR)/libpfunit.$(DSO_SUFFIX) alltests.$(DSO_SUFFIX)
else
  all $(EXE): $(SOURCE_DIR)/libpfunit.a
endif

clean:
	$(RM) *.o
	$(MAKE) -C testData clean

distclean: clean
	$(RM) *.mod
	$(RM) *.a
	$(RM) *.x
	$(RM) *_cpp.F90
	$(MAKE) -C testData distclean

