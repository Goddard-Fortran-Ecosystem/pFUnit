.PHONY: all

TOP_DIR ?=$(shell pwd)

SOURCE_DIR  = $(TOP_DIR)/source
TESTS_DIR   = $(TOP_DIR)/tests
INCLUDE_DIR = $(TOP_DIR)/include
VPATH      += $(SOURCE_DIR) $(INCLUDE_DIR)

# Determine operating system, architecture and compiler
# automatically if possible

UNAME ?=$(shell uname)
ifeq ($(UNAME),)
  UNAME =UNKNOWN
endif

ifneq ($(UNAME),AIX)
  ARCH  ?=$(shell arch)
else
  ARCH ?=AIX
endif
ifeq ($(ARCH),)
  ARCH =UNKNOWN
endif

# Determine default compiler by architecture
ifeq ($(ARCH),AIX)
    F90_VENDOR ?=IBM
else
  ifneq ($(findstring $(ARCH),i386 x86_64 ia64),'')
    F90_VENDOR ?=Intel
  endif
endif
F90_VENDOR ?=UNKNOWN

# 32/64 ABI - almost all architectures are now 64 bit
ifeq ($(ARCH),i386)
  PFUNIT_ABI ?=32
else
  PFUNIT_ABI ?=64
endif

# Other defaults
USE_MPI ?=NO # do not include MPI capabilities
USE_DSO ?=NO # do not use DSO capabilities
USE_ESMF ?=NO # do not use ESMF capabilities

# F90 Vendor common elements (override below)
FFLAGS ?=
D=-D
I=-I
MOD=-I
F90_HAS_CPP=YES
DEBUG_FLAGS =-g
LDFLAGS ?=-L$(SOURCE_DIR) -lpfunitStatic

# F90 Vendor specifics
ifeq ($(F90_VENDOR),Intel)
  F90 ?=ifort
  DEBUG_FLAGS += -traceback -O0
  ifeq ($(F90_VERSION),10)
    CFLAGS+=-m64
  endif
  ifeq ($(DSO),YES)
    FPPFLAGS +=-fPIC
    CFLAGS +=-fPIC
  endif
  FPPFLAGS+=-DSTRINGIFY_OPERATOR
endif

ifeq ($(F90_VENDOR),G95)
  F90 ?=g95
  FFLAGS += -Wno=155
  FPPFLAGS += -DSTRINGIFY_SIMPLE
endif

ifeq ($(F90_VENDOR),GFortran)
  F90 ?=gfortran
  FPPFLAGS += -DSTRINGIFY_SIMPLE
endif

ifeq ($(F90_VENDOR),NAG)
  F90 ?=nagfor
  FFLAGS +=-f2003
  FFLAGS += -mismatch_all
  F90_HAS_CPP=NO
  FPPFLAGS += -DSTRINGIFY_SIMPLE
  CPP =cpp -traditional -C
  ifeq ($(DSO),YES)
    FFLAGS +=-PIC
  endif
  LDFLAGS+= -ldl
endif

ifeq ($(F90_VENDOR),PGI)
  F90 ?=pgf90
  FFLAGS +=-Mallocatable=03
  FPPFLAGS += -DSTRINGIFY_SIMPLE -Minform=severe -O0 -g
  LDFLAGS+= -ldl
endif

ifeq ($(F90_VENDOR),IBM)
  F90 ?=xlf
  CC ?=xlc
  FFLAGS += -qsuffix=cpp=F90 -qfree=f90 -qzerosize -qnosave
  D=-WF,-D
  FPPFLAGS += $DSTRINGIFY_SIMPLE
endif

ifeq ($(ESMF),YES)
MPI=YES
endif

ifeq ($(MPI),YES)
  MPIF90 ?= mpif90
  F90     = $(MPIF90)
  FPPFLAGS += $DUSE_MPI
  CPPFLAGS += -DUSE_MPI
  ifeq ($(MPICH),YES)
     LIBMPI ?=-lmpich
  else 
     LIBMPI ?=-lmpi
  endif
  LDFLAGS += $(LIBMPI)
ifeq ($(ESMF),YES)
   FPPFLAGS += $DUSE_ESMF
endif
endif

DSO_SUFFIX ?=so
ifeq ($(DSO),YES)
  FPPFLAGS += $DUSE_DSO
  CPPFLAGS += -DUSE_DSO
endif

FPPFLAGS += $D$(F90_VENDOR) $D$(UNAME)
CPPFLAGS += -D$(F90_VENDOR) -D$(UNAME) -I$(INCLUDE_DIR)

ifeq ($(PFUNIT_ABI),64)
  FPPFLAGS += $DLONG_PTR
  CPPFLAGS += -DLONG_PTR
endif

FFLAGS +=$I$(INCLUDE_DIR) $(MOD)$(SOURCE_DIR)
CFLAGS +=-I$(INCLUDE_DIR)

ifeq ($(DEBUG),YES)
	FFLAGS += $(DEBUG_FLAGS)
endif

all:
	$(MAKE) -j -C $(SOURCE_DIR) all
	$(MAKE) -j -C $(TESTS_DIR) all

clean:
	$(MAKE) -C $(SOURCE_DIR) clean
	$(MAKE) -C $(TESTS_DIR) clean
	$(RM) .pFUnitLog

distclean: clean
	$(MAKE) -C $(SOURCE_DIR) distclean
	$(MAKE) -C $(TESTS_DIR) distclean
	$(RM) *.x

tests: all
ifeq ($(MPI),YES)
	mpirun -np 5 ./mpi_pFUnit.x
else
	tests/tests.x
endif

install: libpfunit.a
ifeq ($(DSO),YES)
  install: libpfunit.$(DSO_SUFFIX)
endif

INSTALL_DIR ?= $(CURDIR)
install:
	echo Installing pFUnit in $(INSTALL_DIR)
	mkdir -p $(INSTALL_DIR)/lib
	mkdir -p $(INSTALL_DIR)/mod
	mkdir -p $(INSTALL_DIR)/bin
	mkdir -p $(INSTALL_DIR)/include
	cp -p source/lib*     $(INSTALL_DIR)/lib/.
	cp -p source/*.mod    $(INSTALL_DIR)/mod/.
	cp -p bin/wrapTest    $(INSTALL_DIR)/bin/.
	cp -p bin/extract     $(INSTALL_DIR)/bin/.
	cp include/*.h        $(INSTALL_DIR)/include/.
	cp include/*.F90      $(INSTALL_DIR)/include/.
	cp include/*.makefile $(INSTALL_DIR)/include/.
ifeq ($(DSO),YES)
	cp -p source/pfunit.x $(INSTALL_DIR)/bin/.
endif


ifeq ($(F90_HAS_CPP),YES)

define F90-Rule
%.o:%.F90
	$(F90) -c $(FFLAGS) $(FPPFLAGS) $$$$<
endef

else

define F90-Rule
%.o:%.F90
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) $$$$< > $$$$*_cpp.F90
	$(F90) -c $(FFLAGS)  $$$$*_cpp.F90 -o $$$$@
	$(RM) $$$$*_cpp.F90
endef

endif

define M4-Rule
%.F90 : %.m4
	m4 < $$$$< > $$$$*.F90
endef

define CM4-Rule
%.c : %.cm4
	m4 -I$(INCLUDE_DIR) -DfortranCompiler="lowerCase($(F90_VENDOR))" < $$$$< > $$$$*.c
endef


ifeq ($(F90_VENDOR),Intel)
#  DSO_EXTERNAL_LIBRARIES += -L/opt/intel/fc/9.1.038/lib -limf -lifcore
  DSO_EXTERNAL_LIBRARIES += #-L/opt/intel/fc/10.1.014/lib -limf -lifcore -lifport
endif

ifeq ($(F90_VENDOR),NAG)
  DSO_EXTERNAL_LIBRARIES +=
endif

ifeq ($(F90_VENDOR),G95)
  DSO_EXTERNAL_LIBRARIES += #-L/sw/lib/gcc-lib/i386-apple-darwin8/4.0.3/ -lgcc_s -lf95
endif

DSO_EXTERNAL_LIBRARIES +=-L$(SOURCE_DIR) -lpfunit

ifeq ($(UNAME),Linux)
  FFLAGS +=$DLINUX
  ifeq ($(F90_VENDOR),Intel)
    DSO_MECHANISM ?=$(F90) -shared -o $$@ $$^ $(DSO_EXTERNAL_LIBRARIES)
  endif
  ifeq ($(F90_VENDOR),NAG)
    DSO_MECHANISM ?=ld -shared -o $$@ -lc $$^ $(DSO_EXTERNAL_LIBRARIES)
  endif
endif
ifeq ($(UNAME),Darwin)
  DSO_MECHANISM ?=libtool -v -o $$@ -dynamic -undefined dynamic_lookup -single_module -dead_strip $$^ $(DSO_EXTERNAL_LIBRARIES) 
endif
DSO_MECHANISM ?=UNKNOWN

export UNAME
export F90
export F90_VENDOR
export FFLAGS
export FPPFLAGS
export F90_HAS_CPP

export CPP
export CFLAGS
export CPPFLAGS

export LDFLAGS

export F90-Rule
export M4-Rule
export CM4-Rule

export SOURCE_DIR
export INCLUDE_DIR
export VPATH

export MPI
export ESMF
export DSO
export DSO_SUFFIX
export DSO_MECHANISM
export CC
export LIBMPI

ifeq ($(DEBUG),YES)
  $(warning Compilation configuration is as follows:)
  $(warning 	UNAME: 	$(UNAME))
  $(warning 	ARCH:  	$(ARCH))
  $(warning 	F90 vendor:	$(F90_VENDOR))
  $(warning 	F90 command:	$(F90))
  $(warning 	F90 has cpp:	$(F90_HAS_CPP))
  $(warning	USE MPI:	$(MPI))
  $(warning	USE DSO:	$(DSO))
  $(warning	ABI:		$(PFUNIT_ABI))
endif
#FFLAGS+=-prof-gen=srcpos -prof-dir=..

