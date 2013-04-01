.PHONY: clean tests


SRCS = $(wildcard *.pf)
OBJS = $(SRCS:.pf=.o)

tests: $(OBJS)

testSuites.inc: $(SRCS)


FFLAGS += -I$(SRC_DIR) -I$(PFUNIT)/mod

%.F90: %.pf
	$(PFUNIT)/bin/preprocess.pl $<  > $@

%.o: %.F90
	$(MPIF90) -c $(FFLAGS) $<

clean:
	$(RM) *.o *.mod
