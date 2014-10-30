F90 ?= pgfortran

I=-I
M=-I
L=-L

FFLAGS += -O0 -g -traceback -Mbounds -Mchkfpstk -Mchkptr -Mchkstk -Mallocatable=03
FFLAGS += -DSTRINGIFY_SIMPLE
FPPFLAGS += -DPGI
CPPFLAGS += -DSTRINGIFY_SIMPLE -DPGI -Mpreprocess

F90_PP_ONLY = -E
F90_PP_OUTPUT = >

ifeq ($(DSO),YES)
  FFLAGS +=-PIC
endif

LDFLAGS+= -ldl




