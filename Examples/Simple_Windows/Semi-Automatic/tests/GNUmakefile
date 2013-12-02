.PHONY: clean tests

TOP_DIR := ..

SRCS = $(wildcard *.pf)
OBJS = $(SRCS:.pf=$(OBJ_EXT))

# Since PFUNIT is defined relative to the TOP_DIRECTORY, adjust it here
PFUNIT := $(TOP_DIR)/$(PFUNIT)

tests: $(OBJS)

testSuites.inc: $(SRCS)


FFLAGS += -I$(TOP_DIR)/$(SRC_DIR) -I$(PFUNIT)/mod
# Since this is a Windows compilation, add the Windows define.
FFLAGS += -DWindows

ifeq ($(F90),nagfor)
  FFLAGS += -fpp
endif
ifeq ($(F90),ifort)
  FFLAGS += -fpp /nologo
endif

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $<  $@

%$(OBJ_EXT): %.F90
	$(F90) -c $(FFLAGS) $<

clean:
	$(RM) *$(OBJ_EXT) *.mod
