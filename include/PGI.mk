F90 ?= pgfortran

I=-I
M=-I
L=-L

FFLAGS += -O0 -g -traceback -Mbounds -Mchkfpstk -Mchkptr -Mchkstk -Mallocatable=03
FFLAGS += -DSTRINGIFY_SIMPLE
FPPFLAGS += -DPGI
F90_HAS_CPP=YES
CPPFLAGS += -DSTRINGIFY_SIMPLE -DPGI -Mpreprocess

ifeq ($(DSO),YES)
  FFLAGS +=-PIC
endif

LDFLAGS+= -ldl




