F90 ?= gfortran

I=-I
M=-I
L=-L

F90FLAGS += -g -O0 -fbacktrace
F90FLAGS += -fbounds-check -fcheck=mem
F90FLAGS += -DSTRINGIFY_SIMPLE
FPPFLAGS += -DGNU

# The ramifications across all GNUish configurations of eliding CPPFLAGS here are not known. MLR 2013-1104
CPPFLAGS += -DGNU


ifeq ($(USEOPENMP),YES)
F90FLAGS += -fopenmp
endif

