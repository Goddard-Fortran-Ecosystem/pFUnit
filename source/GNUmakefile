TOP = ..
include ../include/base.mk
.PHONY: all generate clean_generated_code testCodeMacs

all: dependencies.inc libpfunit.a

GENERATED_CODE = AssertReal.F90 AssertComplex.F90 AssertInteger1.F90

$(GENERATED_CODE):
	./GenerateRealArrayNewSignature.py

testCodeMacs:
	python $(TOP)/bin/codeMacs.py --inFile testCM.cm --outFile testCM.F90

# %.F90: %.cm
#	python $(TOP)/bin/codeMacs.py $< $@

clean_generated_code: 
	rm -f $(GENERATED_CODE)

distclean: clean_generated_code

dependencies.inc: $(GENERATED_CODE)
	./MakeDependenciesInclude

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

include ./dependencies.inc

