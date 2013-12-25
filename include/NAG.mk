F90 ?= nagfor

I=-I
M=-I
L=-L

F90FLAGS += -g -O0 -f2008 -w=uda -gline -fpp -mismatch_all

F90_HAS_CPP=NO
CPPFLAGS += -DSTRINGIFY_SIMPLE -DNAG
CPP =cpp -traditional -C
ifeq ($(DSO),YES)
  FFLAGS +=-PIC
endif

LDFLAGS+= -ldl
