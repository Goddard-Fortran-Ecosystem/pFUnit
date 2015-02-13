F90 ?= pgfortran

I=-I
M=-I
L=-L

FFLAGS += -O0 -g -traceback -Mallocatable=03 -Mbounds -Mchkfpstk -Mchkstk

# -O0 -g -traceback -Mchkfpstk -Mchkstk
# -Mallocatable=03
# FFLAGS += 
# Segmentation fault: 11

# FFLAGS += -Mbounds
# Segmentation fault: 11

# FFLAGS += -O0 -g -traceback -Mchkfpstk -Mchkstk -Mallocatable=03
# Segmentation fault: 11

# FFLAGS += -O0 -g -traceback -Mbounds -Mchkfpstk -Mchkstk -Mallocatable=03
#0: Subscript out of range for array this%tests (TestSuite.F90: 287)
#    subscript=8496367, lower bound=1, upper bound=2, dimension=1

# FFLAGS += -Mbounds -Minfo=all -traceback -Mchkfpstk -Mchkstk -Mdalign -Mdclchk -Mdepchk -Miomutex -Mrecursive -Msave -Ktrap=fp -O0 -g -byteswapio -Mallocatable=03 

FPPFLAGS += -DPGI
CPPFLAGS += -DPGI -Mpreprocess

F90_PP_ONLY = -E
F90_PP_OUTPUT = >

ifeq ($(DSO),YES)
  FFLAGS +=-PIC
endif

LDFLAGS+= -ldl




