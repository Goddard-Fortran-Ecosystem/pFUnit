# Check for the PFUNIT environment variable
ifndef PFUNIT
$(error The environment variable "PFUNIT" must be given a value which is the \
path for the installation of pFUnit.)
endif

# What is the name of the F90 compiler
F90 ?= ifort
MPIF90 ?= mpif90

PFUNIT_LIBDIR ?=$(PFUNIT)/lib
PFUNIT_INCDIR ?=$(PFUNIT)/include
PFUNIT_MODDIR ?=$(PFUNIT)/mod
PFUNIT_BINDIR ?=$(PFUNIT)/bin

F90FLAGS +=-I$(PFUNIT_MODDIR)
LDFLAGS  +=-L$(PFUNIT_LIBDIR) -lpfunit

F90_SRC = $(wildcard *.F90)
F90_OBJ = $(F90_SRC:.F90=.o)

EXECUTABLE = halo.x
COMMAND = mpirun -np 5 ./$(EXECUTABLE)

# Targets
.PHONY:	all

all: $(EXECUTABLE)
	$(COMMAND)

$(EXECUTABLE): $(F90_OBJ)
	$(MPIF90) -o $@ $(F90_OBJ) $(LDFLAGS)

%.o:%.F90
	$(MPIF90) -c $(F90FLAGS) $<

clean:
	$(RM) $(F90_OBJ)
	$(RM) *.mod

distclean: clean
	$(RM) $(EXECUTABLE)


testHalo_mod.o : Halo_mod.o
driver.o : testHalo_mod.o
