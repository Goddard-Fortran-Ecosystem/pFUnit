
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
EXCLUDE = $(PROGRAMS)
ifneq ($(BUILDROBUST),YES)
EXCLUDE += RobustRunner.F90 Test_RobustRunner.F90 Test_UnixProcess.F90
endif


SRCS = $(filter-out $(EXCLUDE),$(SRCS_BASE))
OBJS = $(SRCS:%.F90=%$(OBJ_EXT))

ifneq ($(USEMPI),YES)
   SRCS_SERIAL=$(filter-out Test_Mpi%,$(SRCS))
   OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%$(OBJ_EXT))
endif

F90FLAGS +=$M$(MOD_DIR)

ifneq ($(UNAME),Windows)
LIBS = $L$(LIB_DIR) -lpfunit

ifeq ($(USEMPI),YES)
   LIBS += $(LIBMPI)
endif
else
LIBS = $(LIB_DIR)/libpfunit$(LIB_EXT)
ifeq ($(USEMPI),YES)
# TODO: This must still be addressed for MPI
   LIBS += $(LIBMPI)
endif
endif

$(EXE) $(REMOTE): libpfunit$(LIB_EXT)

ifeq ($(USEMPI),YES)


$(EXE): selfTests$(OBJ_EXT) $(OBJS)
	$(FC) $(OUTPUT_FLAG) $@ $^ $(F90FLAGS) $(LIBS) -g
$(REMOTE): RemoteProgram$(OBJ_EXT) $(OBJS)
	$(FC) $(OUTPUT_FLAG) $@ $^ $(F90FLAGS) $(LIBS) -g

else

$(EXE): selfTests$(OBJ_EXT) $(OBJS_SERIAL)
	$(FC) $(OUTPUT_FLAG) $@ $^ $(F90FLAGS) $(LIBS) $(DEBUG_FLAGS)
$(REMOTE): RemoteProgram$(OBJ_EXT) $(OBJS_SERIAL)
	$(FC) $(OUTPUT_FLAG) $@ $^ $(F90FLAGS) $(LIBS) $(DEBUG_FLAGS)

endif

include ./dependencies.inc

