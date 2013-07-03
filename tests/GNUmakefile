TOP = ..
include ../include/base.mk
.PHONY: all tests
EXE = tests.x
REMOTE = remote.x

all: $(EXE) $(REMOTE)

VPATH = . ../source
tests: $(EXE)

SRCS_BASE = $(wildcard *.F90)
PROGRAMS = selfTests.F90 RemoteProgram.F90

SRCS = $(filter-out $(PROGRAMS),$(SRCS_BASE))
OBJS = $(SRCS:%.F90=%.o)

ifneq ($(MPI),YES)
   SRCS_SERIAL=$(filter-out Test_Mpi%,$(SRCS))
   OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%.o)
endif

F90FLAGS +=$M$(MOD_DIR)

LIBS = $L$(LIB_DIR) -lpfunit
ifeq ($(MPI),YES)
   LIBS += -lmpi
endif

$(EXE) $(REMOTE): libpfunit.a

ifeq ($(MPI),YES)

$(EXE): selfTests.o $(OBJS)
	$(FC) -o $@ $^ $(LIBS) -g
$(REMOTE): RemoteProgram.o $(OBJS)
	$(FC) -o $@ $^ $(LIBS) -g

else

$(EXE): selfTests.o $(OBJS_SERIAL)
	$(FC) -o $@ $^ $(LIBS) -g
$(REMOTE): RemoteProgram.o $(OBJS_SERIAL)
	$(FC) -o $@ $^ $(LIBS) -g

endif

Test_SimpleTestCase.o: SimpleTestCase.o
Test_FixtureTestCase.o: FixtureTestCase.o
Test_TestResult.o: SimpleTestCase.o
Test_RobustRunner.o: robustTestSuite.o
Test_TestSuite.o: SimpleTestCase.o

EXCLUDE_OBJS = selfTests.o RemoteProgram.o
ifeq ($(MPI),YES)
   selfTests.o RemoteProgram.o: $(filter-out $(EXCLUDE_OBJS),$(OBJS))
else
   selfTests.o RemoteProgram.o: $(filter-out $(EXCLUDE_OBJS),$(OBJS_SERIAL))
endif
