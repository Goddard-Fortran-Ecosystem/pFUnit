.PHONY: clean

SUT: Halo$(OBJ_EXT)

%$(OBJ_EXT): %.F90
	$(MPIF90) -c $(FFLAGS) $<

clean: local-E1-clean

local-E1-clean:
	$(RM) *$(OBJ_EXT) *.mod

