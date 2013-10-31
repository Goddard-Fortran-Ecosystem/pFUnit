F90 ?= gfortran

I=-I
M=-I
L=-L

F90FLAGS += -g -O0 -fbacktrace
F90FLAGS += -fbounds-check -fcheck=mem
F90FLAGS += -DSTRINGIFY_SIMPLE
CPPFLAGS += -DGNU


