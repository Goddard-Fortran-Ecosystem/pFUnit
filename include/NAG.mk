F90 ?= nagfor

I=-I
M=-I
L=-L

FFLAGS += -g -O0 -f2008 -w=uda -fpp -mismatch_all

ifeq ($(USEOPENMP),YES)
FFLAGS += -openmp
else
FFLAGS += -gline
endif

F90_HAS_CPP=NO
CPPFLAGS += -DSTRINGIFY_SIMPLE -DNAG
CPP =cpp -traditional -C
ifeq ($(DSO),YES)
  FFLAGS +=-PIC
endif

LDFLAGS+= -ldl
