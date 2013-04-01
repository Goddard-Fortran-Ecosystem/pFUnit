.PHONY: clean

SUT: Halo.o

%.o: %.F90
	$(MPIF90) -c $(FFLAGS) $<

clean:
	$(RM) *.o *.mod

