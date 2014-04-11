.PHONY: clean

SUT: LinearInterpolator$(OBJ_EXT)

%$(OBJ_EXT): %.F90
	$(FC) -c $(FFLAGS) $<

clean: local-E1-clean

local-E1-clean:
	$(RM) *$(OBJ_EXT) *.mod

