.PHONY: clean

SUT: LinearInterpolator$(OBJ_EXT)

%$(OBJ_EXT): %.F90
	$(FC) -c $(FFLAGS) $<

clean:
	$(RM) *$(OBJ_EXT) *.mod

