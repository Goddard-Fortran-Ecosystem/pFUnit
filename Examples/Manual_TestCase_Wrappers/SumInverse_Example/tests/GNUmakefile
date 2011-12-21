# Check for the PFUNIT environment variable
ifndef PFUNIT
$(error The environment variable "PFUNIT" must be given a value which is the \
path for the installation of pFUnit.)
endif

ROOT_DIR = ..
LIB_DIR = $(ROOT_DIR)/lib
MOD_DIR = $(ROOT_DIR)/mod

F90 ?= ifort

PFUNIT_LIBDIR ?=$(PFUNIT)/lib
PFUNIT_INCDIR ?=$(PFUNIT)/include
PFUNIT_MODDIR ?=$(PFUNIT)/mod
PFUNIT_BINDIR ?=$(PFUNIT)/bin

F90FLAGS += -I. -I$(MOD_DIR)
F90FLAGS += -I$(PFUNIT_MODDIR)
LDFLAGS  += -L$(LIB_DIR) -lsuminverse
LDFLAGS  += -L$(PFUNIT_LIBDIR) -lpfunit

SRC = $(wildcard *.F90)
OBJS = $(SRC:.F90=.o)

EXECUTABLE = test.x
COMMAND = ./$(EXECUTABLE) 

.PHONY=	all

all:	$(EXECUTABLE) tests

tests:
	@$(COMMAND)

$(EXECUTABLE):	$(OBJS)
	$(F90) $(OBJS) -o $@ $(LDFLAGS)
	
%.o:%.F90
	$(F90) -c $(F90FLAGS) $< 
	
clean:	
	$(RM) $(OBJS)
	$(RM) *.mod

distclean:	clean
	$(RM) $(EXECUTABLE) 

TestDriver.o:	TestSumInverse_mod.o
