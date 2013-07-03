TOP = ../..


.PHONY: extests

ifeq ($(MPI),YES)
  NPES = 4
  LAUNCH=mpirun -np $(NPES)
else
  LAUNCH=
endif

extests: 
	$(MAKE) -C source all
	$(MAKE) -C tests all
	$(LAUNCH) ./tests/extests.x

clean:
	$(MAKE) -C source clean
	$(MAKE) -C tests clean

distclean:
	$(MAKE) -C source distclean
	$(MAKE) -C tests distclean


