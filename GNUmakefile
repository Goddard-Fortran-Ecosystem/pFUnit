.PHONY: tests all

# Add -j below for parallel make in subdirectories.
MAKEFLAGS = 

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

ARCH  ?=$(shell arch)
ifeq ($(ARCH),)
  ARCH =UNKNOWN
endif

# Default compiler by architecture - always gfortran for now:
F90 ?=gfortran
F90_VENDOR ?=GNU

# 32/64 ABI - almost all architectures are now 64 bit
ifeq ($(ARCH),i386)
  PFUNIT_ABI ?=32
else
  PFUNIT_ABI ?=64
endif

# Other defaults
MPI ?=NO # do not include MPI capabilities

# F90 Vendor common elements (override below)
FFLAGS ?=
D=-D
I=-I
MOD=-I
F90_HAS_CPP=YES
DEBUG_FLAGS =-g

# F90 Vendor specifics
# Possibly F90 defined - makes things simple:

ifneq (,$(findstring $(F90), ifort gfortran nag nagfor pgfortran))
  ifeq ($(F90),ifort)
     COMPILER=Intel
  else ifeq ($(F90),gfortran)
     COMPILER=GNU
  else ifeq ($(F90),nagfor)
     COMPILER=NAG
  else ifeq ($(F90),pgfortran)
     COMPILER=PGI
  endif
else # use F90_VENDOR to specify
  ifneq (,$(findstring $(F90_VENDOR),INTEL Intel intel ifort))
    COMPILER=Intel
  else ifneq (,$(findstring $(F90_VENDOR),GNU gnu gfortran GFortran GFORTRAN))
    COMPILER=GNU
  else ifneq (,$(findstring $(F90_VENDOR),nag NAG nagfor))
    COMPILER=NAG
  else ifneq (,$(findstring $(F90_VENDOR),pgi PGI pgfortran))
    COMPILER=PGI
  endif
endif

ifeq ($(MPI),YES)
  MPIF90 ?= mpif90
	MPIRUN ?= mpirun
  FPPFLAGS += $DUSE_MPI
  CPPFLAGS += -DUSE_MPI
  ifeq ($(MPICH),YES)
     LIBMPI ?=-lmpich
  else
     LIBMPI ?=-lmpi
  endif
  LDFLAGS += $(LIBMPI)
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
	$(MAKE) $(MAKEFLAGS) -C $(SOURCE_DIR) all
	$(MAKE) $(MAKEFLAGS) -C $(TESTS_DIR) all

clean:
	$(MAKE) -C $(SOURCE_DIR) clean
	$(MAKE) -C $(TESTS_DIR) clean

distclean:
	$(MAKE) -C $(SOURCE_DIR) distclean
	$(MAKE) -C $(TESTS_DIR) distclean

tests: all
ifeq ($(MPI),YES)
	$(MPIRUN) -np 4 ./tests/tests.x
else
	./tests/tests.x
endif

develop:
	mv -f $(TOP_DIR)/include/base-develop.mk $(TOP_DIR)/include/base.mk 

install: libpfunit.a
INSTALL_DIR ?= $(CURDIR)
install: 
	@echo Installing pFUnit in $(INSTALL_DIR)
	mkdir -p $(INSTALL_DIR)/lib
	mkdir -p $(INSTALL_DIR)/mod
	mkdir -p $(INSTALL_DIR)/include
	mkdir -p $(INSTALL_DIR)/bin
	cp -p source/lib*     $(INSTALL_DIR)/lib/.
	cp -p source/*.mod    $(INSTALL_DIR)/mod/.
	cp include/*        $(INSTALL_DIR)/include/.
	mv -f $(INSTALL_DIR)/include/base-install.mk $(INSTALL_DIR)/include/base.mk 
	cp -r bin/* $(INSTALL_DIR)/bin/.
	@echo For normal usage please set PFUNIT to $(INSTALL_DIR).
	@echo For example:  export PFUNIT=$(INSTALL_DIR)

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
export SOURCE_DIR
export INCLUDE_DIR
export VPATH
export MPI
export MPIF90
export LIBMPI
export COMPILER
export MAKEFLAGS

ifeq ($(DEBUG),YES)
  $(warning Compilation configuration is as follows:)
  $(warning     UNAME:  $(UNAME))
  $(warning     ARCH:   $(ARCH))
  $(warning     F90 vendor:     $(COMPILER))
  $(warning     F90 command:    $(F90))
  $(warning     F90 has cpp:    $(F90_HAS_CPP))
  $(warning     USE MPI:        $(MPI))
  $(warning     ABI:            $(PFUNIT_ABI))
endif

