.PHONY: clean

SUT: Halo$(OBJ_EXT)

%$(OBJ_EXT): %.F90
	$(MPIF90) -c $(FFLAGS) $<

clean:
	$(RM) *$(OBJ_EXT) *.mod

