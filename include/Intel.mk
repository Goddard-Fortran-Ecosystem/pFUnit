F90 ?=ifort

I=-I
M=-I
L=-L

F90FLAGS += -assume realloc_lhs
F90FLAGS += -g -O0 -traceback -check uninit -check bounds -check stack -check uninit
F90FLAGS += -DSTRINGIFY_OPERATOR

CPPFLAGS+=-DSTRINGIFY_OPERATOR -DIntel
FPPFLAGS+=-DSTRINGIFY_OPERATOR -DIntel

