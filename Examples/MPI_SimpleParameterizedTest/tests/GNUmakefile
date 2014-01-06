.PHONY: clean tests
# .PRECIOUS: %.F90

# Note: unused references to SRC_DIR removed.

SRCS = $(wildcard *.pf)
OBJS = $(SRCS:.pf=$(OBJ_EXT))

tests: $(OBJS)

testSuites.inc: $(SRCS)

FFLAGS += -I$(PFUNIT)/mod
ifeq ($(F90),nagfor)
   FFLAGS += -fpp
endif

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $<  $@

%$(OBJ_EXT): %.F90
	$(MPIF90) -c $(FFLAGS) $<

clean:
	$(RM) *$(OBJ_EXT) *.mod
