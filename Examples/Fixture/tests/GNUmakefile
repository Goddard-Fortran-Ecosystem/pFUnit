.PHONY: clean tests

SRCS = $(wildcard *.pf)
OBJS = $(SRCS:.pf=$(OBJ_EXT))
#SRCS = Foo.F90
#OBJS = $(SRCS:.F90=$(OBJ_EXT))

tests: $(OBJS)

testSuites.inc: $(SRCS)

FFLAGS += -I$(SRC_DIR) -I$(PFUNIT)/mod

ifndef SKIP_INTENTIONALLY_BROKEN
  FFLAGS += -DINCLUDE_INTENTIONALLY_BROKEN
endif

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $<  $@

%$(OBJ_EXT): %.F90
	$(FC) -c $(FFLAGS) $(FPPFLAGS) $<

clean: local-E1-clean

local-E1-clean:
	$(RM) *$(OBJ_EXT) *.mod
