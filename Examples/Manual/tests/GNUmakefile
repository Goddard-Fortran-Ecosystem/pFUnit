TOP = ../../..
include ../../../include/base.mk
.PHONY: all

all: extests
EX_LIB_DIR=../source
INCLUDE_DIR += $I$(EX_LIB_DIR)

VPATH = . ../source
EXE = extests.x
extests: $(EXE)

SRCS = $(wildcard *.F90)
OBJS = $(SRCS:%.F90=%.o)

SRCS_SERIAL=$(filter-out MPI%,$(SRCS))
OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%.o)

F90FLAGS +=$M$(MOD_DIR)
#F90FLAGS +=$M$(TOP)/tests

#LIB_DIR += $(EX_LIB_DIR)
LIBS = $L$(EX_LIB_DIR) -lf2kex
LIBS += $L$(LIB_DIR) -lf2kunit

ifeq ($(MPI),YES)
  LIBS += -lmpi
endif

$(EXE): $(OBJS) libf2kex.a
	$(MPIF90) -o $@ $(OBJS) $(LIBS)

#Test_Mod.o: SimpleTestCase.o

Test_ModuleTestCase.o: Module1.o
Test_ModuleProceduresTestCase.o: ModuleProcedures.o
Test_ClassExampleTestCase.o: ClassExample.o
exTests.o: Test_ClassExampleTestCase.o Test_ModuleTestCase.o Test_ModuleProceduresTestCase.o

