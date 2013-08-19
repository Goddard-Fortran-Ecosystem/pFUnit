.PHONY: clean tests
# .PRECIOUS: %.F90


SRCS = $(wildcard *.pf)
OBJS = $(SRCS:.pf=.o)

tests: $(OBJS)

testSuites.inc: $(SRCS)


FFLAGS += -I$(SRC_DIR) -I$(PFUNIT)/mod
ifeq ($(F90),nagfor)
   FFLAGS += -fpp
endif

%.F90: %.pf
	python $(PFUNIT)/bin/pFUnitParser.py $<  $@

%.o: %.F90
	$(MPIF90) -c $(FFLAGS) $<

clean:
	$(RM) *.o *.mod
