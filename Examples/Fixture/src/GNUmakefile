.PHONY: clean

SUT: LinearInterpolator$(OBJ_EXT)

%$(OBJ_EXT): %.F90
	$(F90) -c $(FFLAGS) $<

clean:
	$(RM) *$(OBJ_EXT) *.mod

