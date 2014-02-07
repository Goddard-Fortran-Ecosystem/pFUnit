F90 ?= nagfor

I=-I
M=-I
L=-L


ifeq ($(USEOPENMP),YES)
F90FLAGS += -g -O0 -f2008 -w=uda -openmp -fpp -mismatch_all
else
F90FLAGS += -g -O0 -f2008 -w=uda -gline -fpp -mismatch_all
endif

F90_HAS_CPP=NO
CPPFLAGS += -DSTRINGIFY_SIMPLE -DNAG
CPP =cpp -traditional -C
ifeq ($(DSO),YES)
  FFLAGS +=-PIC
endif

LDFLAGS+= -ldl
