# Flags for Fortran compiler, C preprocesor and loader
$(PFUNIT_FFLAGS_VARIABLE) += -$I$(PFUNIT)/include -$(MOD)$(PFUNIT)/mod -$I.

PFUNIT_CPPFLAGS+=-I$(PFUNIT)/include    # may need additional flags for some CPP variants
PFUNIT_XLFLAGS+= $(PFUNIT_WRAP_OBJECTS) $(PFUNIT_TEST_OBJECTS)
PFUNIT_XLFLAGS+= $(PFUNIT_APP_OBJECTS)
PFUNIT_LFLAGS+= $(PFUNIT_LFLAGS)
PFUNIT_XLFLAGS+= -L$(PFUNIT)/lib -lpfunit

# Project tests and object files
PFUNIT_TEST_f90_SOURCES ?=$(filter-out %$(PFUNIT_WRAP_SUFFIX).f90,\
                                       $(filter $(PFUNIT_TEST_PATTERNS),$(notdir $(wildcard $(addsuffix /*.f90,$(PFUNIT_TEST_DIRECTORIES))))))
PFUNIT_TEST_F90_SOURCES ?=$(filter-out %$(PFUNIT_WRAP_SUFFIX).F90,\
                                       $(filter $(PFUNIT_TEST_PATTERNS),$(notdir $(wildcard $(addsuffix /*.F90,$(PFUNIT_TEST_DIRECTORIES))))))

PFUNIT_TEST_SOURCES =$(PFUNIT_TEST_f90_SOURCES) $(PFUNIT_TEST_F90_SOURCES)
PFUNIT_TEST_OBJECTS =$(PFUNIT_TEST_f90_SOURCES:%.f90=%.o) $(PFUNIT_TEST_F90_SOURCES:%.F90=%.o)

# Wrappers and object files for project tests
PFUNIT_WRAP_f90_SOURCES =$(PFUNIT_TEST_f90_SOURCES:%.f90=%$(PFUNIT_WRAP_SUFFIX).f90)
PFUNIT_WRAP_F90_SOURCES = $(PFUNIT_TEST_F90_SOURCES:%.F90=%$(PFUNIT_WRAP_SUFFIX).F90)
PFUNIT_WRAP_SOURCES =$(PFUNIT_WRAP_f90_SOURCES) $(PFUNIT_WRAP_F90_SOURCES)
PFUNIT_WRAP_OBJECTS =$(PFUNIT_WRAP_f90_SOURCES:%.f90=%.o) $(PFUNIT_WRAP_F90_SOURCES:%.F90=%.o)

# Targets and rules
.SECONDARY: $(PFUNIT_WRAP_SOURCES)
.PHONY: tests pfunitdistclean

VPATH +=$(PFUNIT_TEST_DIRECTORIES) $(PFUNIT)/include
PFUNIT_LAUNCH_SERIAL=./$(PFUNIT_TEST_EXECUTABLE)
PFUNIT_LAUNCH_PARALLEL=$(MPIRUN) -np $(PFUNIT_NPES) ./$(PFUNIT_TEST_EXECUTABLE)

tests: $(PFUNIT_TEST_EXECUTABLE)
  ifeq ($(PFUNIT_USE_MPI),YES) # parallel executable
	$(PFUNIT_LAUNCH_PARALLEL)
  else # serial executable
	$(PFUNIT_LAUNCH_SERIAL)
  endif

# dependcies for test wrappers
# Poor efficiency - lots of unnecessary compilation.
$(PFUNIT_TEST_OBJECTS): $(PFUNIT_APP_OBJECTS)
define MAKE_WRAP_DEPENDENCY
      $(1)_wrap.o: $(1).o
endef

$(PFUNIT_SUITE_LIST): $(PFUNIT_TEST_SOURCES)
	@$(PFUNIT)/bin/extract $^ > $(PFUNIT_SUITE_LIST)

$(foreach file,$(basename $(PFUNIT_TEST_OBJECTS)),$(eval $(call MAKE_WRAP_DEPENDENCY,$(file))))

%_wrap.F90 : %.F90
	@$(PFUNIT)/bin/wrapTest $< $@

ifeq ($(PFUNIT_USE_MPI),YES)
  PFUNIT_COMPILER=$(MPI_FC)
else
  PFUNIT_COMPILER=$(FC)
endif

driver.F90: $(PFUNIT_SUITE_LIST)
$(PFUNIT_TEST_EXECUTABLE): $(PFUNIT_TESTS_OBJECTS) $(PFUNIT_WRAP_OBJECTS) $(PFUNIT_SUITE_LIST)
	$(PFUNIT_COMPILER) -o $@ $($(PFUNIT_FFLAGS_VARIABLE)) $(PFUNIT)/include/driver.F90 $(PFUNIT_XLFLAGS)

pfunitdistclean:
	$(RM) $(PFUNIT_WRAP_SOURCES)
	$(RM) $(PFUNIT_TEST_EXECUTABLE)
	$(RM) $(PFUNIT_SUITE_LIST)
	$(RM) $(PFUNIT_TEST_EXECUTABLE)
