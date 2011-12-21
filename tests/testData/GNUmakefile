# Expand rules defined in parent makefile
$(eval $(F90-Rule))
$(eval $(M4-Rule))
$(eval $(CM4-Rule))

foo.o : foo.F90
foo2.o : foo2.F90
foo_3.o : foo_3.F90
bar_mod.o : bar_mod.F90
MPI_methods_mod.o : MPI_methods_mod.F90
test_serial.o: test_serial.F90


ifeq ($(strip $(FC)),xlf)
  FC_USE_CPP2=$(strip $(FC_USE_CPP))F90
else
  FC_USE_CPP2=$(FC_USE_CPP)
endif

ifeq ($(F90_VENDOR),INTEL)
	FFLAGS += -fPIC
endif

%.$(DSO_SUFFIX):
	$(DSO_MECHANISM)

ifneq ($(DSO_SUFFIX),so)
	ln -s $@ $(addsuffix $(basename $@),.so)
endif

libfoo.$(DSO_SUFFIX): foo.o
libfoo2.$(DSO_SUFFIX): foo.o foo2.o foo_3.o
libbar.$(DSO_SUFFIX): bar_mod.o
libpFUnitTestsA.$(DSO_SUFFIX): test_succeed.o test_fail.o
libpFUnitTestsB.$(DSO_SUFFIX): test_serial.o
ifeq ($(MPI),YES)

libmpitests.$(DSO_SUFFIX): MPI_methods_mod.o
squareServer.o: squareServer.F90
square.x: squareServer.o
	$(F90) squareServer.o -o $@ $(LIBMPI)
endif


clean:
	$(RM) *.o

distclean: clean
	$(RM) *.mod lib*.*
	$(RM) *.x