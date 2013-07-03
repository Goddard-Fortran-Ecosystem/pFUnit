.PHONY: clean

SUT: add.o

%.o: %.F90
	$(F90) -c $(FFLAGS) $<

clean:
	$(RM) *.o *.mod

