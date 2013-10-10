
# Gnu makefile for $(TOP)/tests/

TOP = ..
include ../include/base.mk
.PHONY: all tests
EXE = tests$(EXE_EXT)
REMOTE = remote$(EXE_EXT)

all: dependencies.inc $(EXE) $(REMOTE) 

VPATH = . ../source
tests: dependencies.inc $(EXE) 

dependencies.inc: 
	./MakeDependenciesInclude

SRCS_BASE = $(wildcard *.F90)
PROGRAMS = selfTests.F90 RemoteProgram.F90

SRCS = $(filter-out $(PROGRAMS),$(SRCS_BASE))
OBJS = $(SRCS:%.F90=%$(OBJ_EXT))

ifneq ($(MPI),YES)
   SRCS_SERIAL=$(filter-out Test_Mpi%,$(SRCS))
   OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%$(OBJ_EXT))
endif

F90FLAGS +=$M$(MOD_DIR)

LIBS = $L$(LIB_DIR) -lpfunit
ifeq ($(MPI),YES)
   LIBS += -lmpi
endif

$(EXE) $(REMOTE): libpfunit$(LIB_EXT)

ifeq ($(MPI),YES)

$(EXE): selfTests.o $(OBJS)
	$(FC) -o $@ $^ $(LIBS) -g
$(REMOTE): RemoteProgram.o $(OBJS)
	$(FC) -o $@ $^ $(LIBS) -g

else

$(EXE): selfTests$(OBJ_EXT) $(OBJS_SERIAL)
	$(FC) -o $@ $^ $(LIBS) -g
$(REMOTE): RemoteProgram$(OBJ_EXT) $(OBJS_SERIAL)
	$(FC) -o $@ $^ $(LIBS) -g

endif

include ./dependencies.inc

