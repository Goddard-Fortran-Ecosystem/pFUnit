F90 ?=ifort

I=-I
M=-I
L=-L

ifneq ($(UNAME),Windows)
# Non-Windows (Linux) command line options for the intel compiler
version = $(shell $(F90) --version | grep -E '\(IFORT\) 13')

F90FLAGS += -assume realloc_lhs
F90FLAGS += -g -O0 -traceback -check uninit -check bounds -check stack -check uninit

ifeq ($(USEOPENMP),YES)
F90FLAGS += -openmp
endif


else
# Windows command line options for the intel compiler
version = $(shell $(F90)  2>&1 | head -1 | grep 'Version 13')

# Suppress version information with each compile.
F90FLAGS += /nologo
F90FLAGS += /assume:realloc_lhs
F90FLAGS += /Z7 /Od /traceback /check:uninit /check:bounds /check:stack /check:uninit
# Enable the Fortran preprocessor
F90FLAGS += /fpp

# Remove the DEBUG_FLAGS -g option.
DEBUG_FLAGS = /Z7
endif


# Common command line options.
F90FLAGS += -DSTRINGIFY_OPERATOR


CPPFLAGS +=-DSTRINGIFY_OPERATOR -DIntel
FPPFLAGS +=-DSTRINGIFY_OPERATOR -DIntel

# Check if the version of the compiler is 13
ifneq ($(version),)
  CPPFLAGS+=-DINTEL_13
  FPPFLAGS+=-DINTEL_13
endif


