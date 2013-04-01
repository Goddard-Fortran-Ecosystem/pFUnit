TOP = ..
include ../include/base.mk
.PHONY: all tests
EXE = tests.x

all: $(EXE)

VPATH = . ../source
tests: $(EXE)

SRCS = $(wildcard *.F90)
OBJS = $(SRCS:%.F90=%.o)

$(erro $(OBJS))

ifneq ($(MPI),YES)
   SRCS_SERIAL=$(filter-out Test_Mpi%,$(SRCS))
   OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%.o)
endif

F90FLAGS +=$M$(MOD_DIR)

LIBS = $L$(LIB_DIR) -lpfunit
ifeq ($(MPI),YES)
   LIBS += -lmpi
endif

ifeq ($(MPI),YES)
$(EXE): $(OBJS) libpfunit.a
	$(MPIF90) -o $@ $(OBJS) $(LIBS) -g
else
$(EXE): $(OBJS_SERIAL) libpfunit.a
	$(MPIF90) -o $@ $(OBJS_SERIAL) $(LIBS) -g
endif

Test_SimpleTestCase.o: SimpleTestCase.o
Test_FixtureTestCase.o: FixtureTestCase.o
Test_TestResult.o: SimpleTestCase.o
EXCLUDE_OBJS = SimpleTestCase.o FixtureTestCase.o selfTests.o
ifeq ($(MPI),YES)
   selfTests.o: $(filter-out $(EXCLUDE_OBJS),$(OBJS))
else
   selfTests.o: $(filter-out $(EXCLUDE_OBJS),$(OBJS_SERIAL))
endif
