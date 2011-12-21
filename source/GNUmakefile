# Expand rules defined in parent makefile
$(eval $(F90-Rule))
$(eval $(M4-Rule))


SRCS_C   =$(wildcard *.c)         # compile with C 
#SRCS_CM4 =$(wildcard *.cm4)     # preprocess with m4 then compile with C
SRCS_F90 =$(wildcard *.F90)       # compile with F90 (and CPP)
SRCS_M4  =$(wildcard *.m4)       # preprocess with m4 then compile with F90

# Ignore these sources unless using MPI
EXCLUDE_SRCS ?=

ifneq ($(MPI),YES)
  EXCLUDE_SRCS += MpiServer_mod.F90 MpiClient_mod.F90
  EXCLUDE_SRCS += MpiCommand_mod.F90 newMpiTestCase_mod.F90 MpiTestCase_mod.F90
endif

ifneq ($(DSO),YES)
  EXCLUDE_SRCS += SharedObjLibUtilities_mod.F90 pFUnitDriver_mod.F90 pFUnitDriver.F90 pFUnit_DSO_mod.F90
endif

#ALL_SRCS =$(SRCS_C) $(SRCS_CM4) $(SRCS_F90) $(SRCS_M4)
#ALL_OBJS =$(SRCS_C:.c=.o) $(SRCS_CM4:.cm4=.o) $(SRCS_F90:.F90=.o) $(SRCS_M4:.m4=.o)

ALL_SRCS =$(SRCS_C) $(SRCS_F90) $(SRCS_M4)
ALL_OBJS =$(SRCS_C:.c=.o) $(SRCS_F90:.F90=.o) $(SRCS_M4:.m4=.o)
SRCS =$(filter-out $(EXCLUDE_SRCS),$(ALL_SRCS))
OBJS =$(addsuffix .o,$(basename $(SRCS)))

# Remove the intermediate files by using the target 'distclean' only
.SECONDARY: AssertReal_mod.F90 AssertComplex_mod.F90

# Manual list of dependencies - avoid need for dependency analyzer
Params_mod.o:
pFUnitException_mod.o: MpiServices_mod.o
BaseAddress_mod.o:
cString_mod.o:
MpiStubs_mod.o:
MpiServices_mod.o:		MpiStubs_mod.o

Exception_mod.o: 		pFUnitException_mod.o
AssertString_mod.o: 		pFUnitException_mod.o
AssertInteger_mod.o: 		pFUnitException_mod.o
Report_mod.o: 			Params_mod.o			\
				pFUnitException_mod.o           \
				AssertInteger_mod.o             \
                                AssertString_mod.o

AssertReal_mod.o: 		Params_mod.o			\
				pFUnitException_mod.o		\
				AssertString_mod.o

AssertComplex_mod.o:		Params_mod.o			\
				pFUnitException_mod.o		\
				AssertString_mod.o

Assert_mod.o: 			AssertString_mod.o		\
				AssertInteger_mod.o		\
				AssertReal_mod.o		\
				AssertComplex_mod.o		\
				Report_mod.o
ProcedurePointer_mod.o: 	BaseAddress_mod.o		\
				pFUnitException_mod.o

TestCase_mod.o: 		Params_mod.o			\
				BaseAddress_mod.o		\
				ProcedurePointer_mod.o		\
				TestResult_mod.o		\
				TestInfo_mod.o		\

TestCase_mod.o: MpiServices_mod.o

TestResult_mod.o:		Params_mod.o			\
				Assert_mod.o			\
				Report_mod.o			\
				IO_Utilities_mod.o
ParameterizedTestCase_mod.o:	BaseAddress_mod.o		\
				ProcedurePointer_mod.o		\
				TestResult_mod.o

TestSuite_mod.o:		TestCase_mod.o			\
				ParameterizedTestCase_mod.o
ifeq ($(MPI),YES)
TestSuite_mod.o:		MpiTestCase_mod.o
endif

TestInfo_mod.o:			pFUnitException_mod.o
TestInfo_mod.o:			pFUnitException_mod.o MpiServices_mod.o

ifeq ($(MPI),YES)
TestSuite_mod.o:		newMpiTestCase_mod.o
newMpiTestCase_mod.o:           TestCase_mod.o
endif

Semaphore_mod.o:		Params_mod.o			\
				IO_Utilities_mod.o

IO_Utilities_mod.o: 		pFUnitException_mod.o

MpiServer_mod.o:		IO_Utilities_mod.o MpiCommand_mod.o newMpiTestCase_mod.o
MpiClient_mod.o:		IO_Utilities_mod.o
FortranNameMangle_mod.o:	pFUnitException_mod.o
MpiTestCase_mod.o: TestCase_mod.o
SharedObjLibUtilities_mod.o: 	cString_mod.o			\
				ProcedurePointer_mod.o		\
				BaseAddress_mod.o		\
			 	FortranNameMangle_mod.o		\
				pFUnitException_mod.o		\
				IO_Utilities_mod.o

pFUnitDriver_mod.o:		TestSuite_mod.o		\
				SharedObjLibUtilities_mod.o

pFUnit.o:	Assert_mod.o		pFUnitException_mod.o		BaseAddress_mod.o	\
		ProcedurePointer_mod.o	TestResult_mod.o		TestSuite_mod.o		\
		TestCase_mod.o		ParameterizedTestCase_mod.o	IO_Utilities_mod.o	\
		AssertString_mod.o	AssertInteger_mod.o		AssertReal_mod.o	\
		Semaphore_mod.o		Exception_mod.o			BaseAddress_mod.o	\
		ProcedurePointer_mod.o	cString_mod.o TestMethodInfo_mod.o

ifeq ($(MPI),YES)
  pFUnit.o: MpiTestCase_mod.o	MpiClient_mod.o			MpiServer_mod.o
  pFUnit.o: newMpiTestCase_mod.o MpiTestCase_mod.o
  pFUnit.o: TestInfo_mod.o
endif

ifeq ($(DSO),YES)
  pFUnit.o:		pFUnitDriver_mod.o	FortranNameMangle_mod.o	SharedObjLibUtilities_mod.o
  pFUnitDriver.o:	pFUnit.o
  pFUnitDriver.o:	pFUnitDriver_mod.o	FortranNameMangle_mod.o

  pfunit.x: pFUnitDriver.o	libpfunit.$(DSO_SUFFIX)
	$(F90) pFUnitDriver.o $(FFLAGS) -o $@ $(LDFLAGS)
  all: pfunit.x libpfunit.$(DSO_SUFFIX)
endif


libpfunit.a: $(filter-out pFUnitDriver.o,$(OBJS))
	$(AR) cr $@ $?
	ranlib $@
	cp libpfunit.a libpfunitStatic.a

ifeq ($(F90_VENDOR),G95)
  DSO_LDFLAGS = -L/sw/lib/gcc-lib/i386-apple-darwin8/4.0.3/ -lgcc_s -lf95
endif

libpfunit.$(DSO_SUFFIX): $(filter-out pFUnitDriver.o,$(OBJS))
	$(DSO_MECHANISM)

all: libpfunit.a


clean:
	$(RM) *.o

distclean: clean
	$(RM) *.mod
	$(RM) AssertReal_mod.F90
	$(RM) AssertComplex_mod.F90
	$(RM) *.a
	$(RM) *.x
	$(RM) *.$(DSO_SUFFIX)
	$(RM) *_cpp.F90 
	$(RM) *.c # dangerous


