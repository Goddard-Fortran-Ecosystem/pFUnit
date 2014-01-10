.PHONY: clean

SUT: SphericalCoordinates$(OBJ_EXT)

%$(OBJ_EXT): %.F90
	$(F90) -c $(FFLAGS) $<

clean:
	$(RM) *$(OBJ_EXT) *.mod

