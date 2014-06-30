TOP = ..
include ../include/base.mk
.PHONY: all generate clean_generated_code testCodeMacs

all: dependencies.inc generated.inc libpfunit$(LIB_EXT)

# "GENERATED_CODE" is defined in "generated.inc".
# GENERATED_CODE = AssertReal.F90 AssertComplex.F90 AssertInteger1.F90
# 
generated.inc:
	python ./GenerateAssertsOnArrays.py --maxRank $(MAX_RANK)

# 	@echo "# This file automatically generated. Do not modify." > ./generated.inc

AssertArrays.fh: generated.inc

Assert.F90: AssertArrays.fh

include ./generated.inc

# $(GENERATED_CODE):
#	./GenerateAssertsOnArrays.py

testCodeMacs:
	python $(TOP)/bin/codeMacs.py --inFile testCM.cm --outFile testCM.F90

# %.F90: %.cm
#	python $(TOP)/bin/codeMacs.py $< $@

clean_generated_code:
	rm -f generated.inc AssertArrays.fh
	rm -f $(GENERATED_CODE)

distclean: local-build1-distclean

local-build1-distclean: clean_generated_code

# dependencies.inc: $(GENERATED_CODE)
dependencies.inc: generated.inc
	./MakeDependenciesInclude $(FFLAGS) $(FPPFLAGS)

ifneq ($(BUILDROBUST),YES)
EXCLUDE += UnixProcess.F90 RobustRunner.F90
endif
SRCS = $(filter-out $(EXCLUDE),$(wildcard *.F90))
OBJS = $(SRCS:%.F90=%$(OBJ_EXT))

EXCLUDE_MPI_SRCS = MpiContext.F90 MpiStubs.F90 MpiTestCase.F90 MpiTestMethod.F90

ifneq ($(USEMPI),YES)
   SRCS_SERIAL=$(filter-out $(EXCLUDE_MPI_SRCS),$(SRCS))
   OBJS_SERIAL=$(SRCS_SERIAL:%.F90=%$(OBJ_EXT))
endif

ifeq ($(USEMPI),YES)
   libpfunit$(LIB_EXT): $(OBJS)
else
   libpfunit$(LIB_EXT): $(OBJS_SERIAL)
endif
	$(AR)$@ $?
	-@$(RANLIB) $@

include ./dependencies.inc

