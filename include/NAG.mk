F90 ?= nagfor

I=-I
M=-I
L=-L

F90FLAGS += -g -O0 -f2003 -w=uda -gline -fpp -mismatch_all

ifeq ($(USEOPENMP),YES)
F90FLAGS += -openmp
endif

F90_HAS_CPP=NO
CPPFLAGS += -DSTRINGIFY_SIMPLE -DNAG
CPP =cpp -traditional -C
ifeq ($(DSO),YES)
  FFLAGS +=-PIC
endif

LDFLAGS+= -ldl
