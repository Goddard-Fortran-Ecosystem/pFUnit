.PHONY: clean

SUT: add$(OBJ_EXT) addComplex$(OBJ_EXT)

%$(OBJ_EXT): %.F90
	$(F90) -c $(FFLAGS) $<

clean:
	$(RM) *$(OBJ_EXT) *.mod

