.PHONY: clean

SUT: LinearInterpolator.o

%.o: %.F90
	$(F90) -c $(FFLAGS) $<

clean:
	$(RM) *.o *.mod

