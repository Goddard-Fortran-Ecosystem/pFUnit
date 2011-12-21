ROOT_DIR = ..
LIB_DIR = $(ROOT_DIR)/lib
MOD_DIR = $(ROOT_DIR)/mod
LIB = libsuminverse.a
MOD = suminverse_mod.mod

F90 ?= ifort

F90FLAGS += -I. -I$(MOD_DIR)

SRC = $(wildcard *.F90)
OBJS = $(SRC:.F90=.o)

.PHONY=	all

all:	$(LIB) tests

tests:
	mkdir -p $(LIB_DIR)
	mkdir -p $(MOD_DIR)
	cp $(LIB) $(LIB_DIR)/.
	cp $(MOD) $(MOD_DIR)/.

$(LIB):	$(OBJS)
	ar -cr $@ $?
	
%.o:%.F90
	$(F90) -c $(F90FLAGS) $< 
	
clean:	
	$(RM) $(OBJS) $(LIB)
	$(RM) *.mod

distclean:	clean
	$(RM) $(LIB_DIR)/$(LIB) $(MOD_DIR)/$(MOD) 
