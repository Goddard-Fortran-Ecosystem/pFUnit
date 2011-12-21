# Check for the PFUNIT environment variable
ifndef PFUNIT
$(error The environment variable "PFUNIT" must be given a value which is the \
path for the installation of pFUnit.)
endif

# What is the name of the F90 compiler
F90 ?= ifort

PFUNIT_LIBDIR ?=$(PFUNIT)/lib
PFUNIT_INCDIR ?=$(PFUNIT)/include
PFUNIT_MODDIR ?=$(PFUNIT)/mod
PFUNIT_BINDIR ?=$(PFUNIT)/bin

F90FLAGS +=-I$(PFUNIT_MODDIR)
LDFLAGS  +=-L$(PFUNIT_LIBDIR) -lpfunit

F90_SRC = $(wildcard *.F90)
F90_OBJ = $(F90_SRC:.F90=.o)

EXECUTABLE = fixture.x
COMMAND = ./$(EXECUTABLE)

# Targets
.PHONY:	all

all: $(EXECUTABLE)
	$(COMMAND)

$(EXECUTABLE): $(F90_OBJ)
	$(F90) -o $@ $(F90_OBJ) $(LDFLAGS)

%.o:%.F90
	$(F90) -c $(F90FLAGS) $<

clean:
	$(RM) -f $(F90_OBJ)
	$(RM) -f *.mod

distclean: clean
	$(RM) -f $(EXECUTABLE)

driver.o : testUseFixture_mod.o
