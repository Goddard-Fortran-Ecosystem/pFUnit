# ANNOUNCEMENT:  Release of 4.0_beta

The beta release of 4.0 is now available (on branch release/4.0_beta).  Please give it a try.

Documentation (esp. diffs) is forthcoming, but there is now set of [demos](https://github.com/Goddard-Fortran-Ecosystem/pFUnit_demos).

**Note** the GitHub release tarballs don't work due to a technical issue with git archive and git submodules.   Please use git to directly clone the repository.    A helpful user has already provided a solution for the tar balls but it will probably not be available until this weekend.





# pFUnit

pFUnit is a unit testing framework enabling JUnit-like testing of
serial and MPI-parallel software written in Fortran. Initial support
for OPENMP has been implemented. pFUnit makes use of modern Fortran
programming techniques, including object oriented programming,
offering a convenient, lightweight mechanism for Fortran developers to
create and run software tests that specify the desired behavior for a
given piece of code. The framework was originally created by
developers from NASA and NGC TASC.

# NOTE: The documentation below is for pFUnit 3.x

pFunit 4.0.0_beta is expected to be released in early April.

## Installation and basic usage guide

### Table of contents

1. [Prerequisites](#prerequisites)
2. [Obtaining pFUnit](#obtaining-pfunit)
3. [What's in the directory?](#whats-in-the-directory)
4. [Configuration](#configuration)
5. [Building pFUnit](#building-pfunit)
6. [Installation](#installation)
7. [Usage](#usage)
8. [Development](#development)
9. [Feedback and support](#feedback-and-support)
10. [Acknowledgments](#acknowledgments)
11. [Known installations/versions](#known-installationsversions)
12. [Notes](#notes)
13. [TODO](#todo)

## Prerequisites

The development work for pFUnit has mostly been carried out on a
mixture of systems, including high-end computers, Apple Mac OSX, and
linux-based systems. A preliminary Windows/CYGWIN port has been
contributed by a user. Full use of the system depends on the following
being available.

- Fortran 2003+
  - Tested with:
    - Intel 14+,
    - NAG 6.0,
    - GFortran 4.8.3, 4.9.+, 5.0+
    - IBM's XLF
    - PGI 15.7
- The Message Passing Interface (MPI)
- OpenMP
- GNU Make
- Python 2.7+

A CMake build process is also available.

Doxygen is used to generate documentation (see http://www.doxygen.org).

The system is routinely undergoes regression testing, including with GNU,
Intel, and NAG fortran compilers and OpenMPI.

pFUnit makes extensive use of leading edge Fortran language features,
which are generally best supported with by the latest compiler
versions. The capacity to support older compilers is limited.

## Obtaining pFUnit

The best way to obtain pFUnit is to clone the git repository from
GitHub as follows:

    git clone https://github.com/Goddard-Fortran-Ecosystem/pFUnit.git

This will create the directory pFUnit in the current working
directory.

You can download the latest version as a tarball from the [release
page](https://github.com/Goddard-Fortran-Ecosystem/pFUnit/releases/latest),
and extract it like:

    $ tar zxf ./pFUnit-*.tar.gz

which will place the pFUnit files into a directory called
`pFUnit-<version>`, where `<version>` is the version number.

For other ways to acquire the code visit

    https://github.com/Goddard-Fortran-Ecosystem/pFUnit/

or contact the pFUnit team.

## What's in the directory?

In the top level of the pFUnit distribution you will see the following
files.

- `CMakeLists.txt` - Initial support for cmake-based builds.

- `COPYRIGHT` - Contains information pertaining to the use and
  distribution of pFUnit.
  
- `Examples` - Contains examples of how to use pFUnit once it is
  installed.

- `bin` - Executables used to construct and perform unit tests.

- `documentation` - Provides information about the pFUnit.  (Very out of date.)

- `extern` - external dependencies installed as git submodules

- `include` - Files to be included into makefiles or source, including use
  code.

- `LICENSE` - The NASA Open Source Agreement for GSC-15,137-1 F-UNIT,
  also known as pFUnit.

- `README.md` - This file.

- `src` - Source code and scripts of the pFUnit library and framework.

- `tests` - Source code for unit testing pFUnit itself.

- `tools` - Tools used to help develop, build, and install pFUnit.

- `VERSION` - Contains a string describing the current version of the framework.

## Configuration

Little needs to be done to configure pFUnit for the build, however
there are several environment variables on which the package depends.

- `FC` - possibly used by CMake to identify your Fortran compiler.

- If using MPI, it must be in a location that CMake's FindMPI can find.

As a convenience for working with multiple MPI configurations, you may
also set the following.

    MPIRUN
    $ # e.g.
    $ export MPIRUN=/some.path/mpirun

- `-DDMAX_ASSERT_RANK` - controls the maximum number of (Fortran)
  dimensions of the arrays asserts are defined over. If not set, the default is 5 and pFUnit's
  assertions will be able to handle arrays up to rank 5.

## Building pFUnit with CMake

*** Note that vanilla Make is no longer supported.***

   The process for building pFUnit using cmake is as follows. In the
   top directory of the distribution make a new directory to support the
   build, then change to that directory and run cmake (pointing back to
   the source) to generate the required makefiles.

        $ mkdir build
        $ cd build
        $ cmake ..
        $ make tests

   Don't forget you can use the standard `-DCMAKE_INSTALL_PREFIX` to
   define where the resulting tool will be installed.    
   Otherwise the install will be inside your build directory in a directory called `installed'

 If the build is successful, then at this point `make install` should work.

## INSTALLATION

Installations 6.1-6.5 are based on GNU make and the project
makefiles.   Because many pFUnit users lack permissions to install 
in the cmake default installation path, pFUnit instead by default
installs in the "installed" subdirectory in the main build directory.  
This can be easily overridden via  `CMAKE_INSTALL_PREFIX` on the cmake
command line.  Note that cmake allows a few variants on the internal structure
of the install, and future versions of pFUnit are likely to switch to a different 
variant.  (`find_package(pFUnit)` should continue to work, but there may be 
issues with the search order for users with conflicting installs.)

## Default directory

If `INSTALL_DIR` is not set, `make install` will attempt to install
pFUnit into a directory called `installed` at the top build directory. 


## Usage
### Configuration

For regular use, after installation, the same compiler/MPI development
configuration that was used to build pFUnit should be used. Once the
environment variables and paths associated with the environment are
set, to configure pFUnit, please set the following.

- `pFUnit` - set to the directory into which pFUnit was installed.
- `F90_VENDOR` - set to Intel, GNU, NAG, or PGI accordingly.

### Preprocessor - Hello World

An example of how to use the preprocessor can be found in
Examples/Simple. The GNU makefile shows how to construct an F90 file
from a preprocessor input file. For example, the GNU make rule can be:

```make
# GNU makefile rule
%.F90: %.pf
    $(pFUnit)/bin/pFUnitParser.py $< $@
```

The file testSuites.inc is included in the include/driver.F90 file
during the build process. To include tests, one must add the test
suite module to testSuites.inc, as follows.

    ! Add a test suite to the build.
    ADD_TEST_SUITE(helloWorld_suite)

A preprocessor input file contains tests and is a 
Fortran free-format file with directives, for example:

```fortran
! helloWorld.pf - with a successful test...
@test
subroutine testHelloWorld()
   use pfunit_mod
   implicit none
   @assertEqual("Hello World!","Hello World!")
end subroutine testHelloWorld
```

### Compiling and Executing the Tests (SERIAL)

An example of a GNU make rule for for the final step of compiling a test follows.

```make
# This step presumes "include $(pFUnit)/include/PFUNIT.mk" earlier in the makefile.
tests.x: testSuites.inc myTests.pf
         $(F90) -o $@ -I$(pFUnit)/include \
                $(pFUnit)/include/driver.F90 \
                ./*$(OBJ_EXT) $(LIBS) $(FFLAGS)
```

To execute the tests, one invokes `./tests.x` with the appropriate command line options (see below).

In some cases, since include/driver.F90 is `implicit none` it may be
necessary to insert a `use` clause to identify external suite-wide
fixture code to the compiler. As a convenience, the CPP macro
`pFUnit_EXTRA_USAGE` can be set to a module of fixture code via a
compiler command line argument turning on a `use pFUnit_EXTRA_USAGE`
line at the beginning of include/driver.F90.

### Compiling and Executing the Tests (MPI PARALLEL)

One invokes MPI-based parallel tests according to the MPI framework
being used. For example:

    $ mpirun -np 4 tests.x


### Command Line Options

The executable test program provides several command line options,
when "include/driver.F90" is used, as it is automatically when using
the pFUnit preprocessor.

    -v or -verbose                  Verbose execution.
    -d or -debug                    Provide debugging information.
    -h                              Print help message.
    -o <outputfile>                 Direct pFUnit messages to a file.
    -robust                         Use the robust runner. Runs tests as processes so failures do not halt testing.
    -max-timeout-duration <duration> Limit detection time for robust runner.
    -max-launch-duration  <duration> Limit detection time for robust runner.
    -skip <number of tests to skip> Use the subset runner, which runs a subset of the tests in a suite.

An example from Examples/Robust:

    $ ./tests.x -robust

## Development

Generally pFUnit development is performed in the build directory
structure. Care should be taken to make clean or distclean in between
configuration changes. As stated above, it is best to set `INSTALL_DIR`
and `make install` pFUnit to another directory that can be placed in a
user's paths. 

## Feedback and bugs

### Support

### Tips

1. Environment Modules - Though not strictly required, the Environment
   Modules package can be a convenient way to package, maintain, and
   switch between environments. This can be particularly important for
   pFUnit, which must be built using the same tool suite being used for
   development, e.g. compilers, linkers, etc. [To do:  A sample pFUnit
   modulefile is provided in the OTHER directory.] Environment Modules

2. Compile time errors like `"include [...]include/.mk" not found`
   likely signify that you not executing make in the top level
   directory during a build. Alternatively, during regular usage after
   installation, pFUnit has not been set.

   During building, if you wish to compile in a subdirectory of within the
   pFUnit heriarchy, please try setting the COMPILER environment variable
   on the make command line. For example:

        $ make all COMPILER=Intel

3. If you wish to see the intermediate files, use the target .PRECIOUS
   in the makefile to keep them from being deleted. For example:

```make
# In GNUmakefile
.PRECIOUS: %_cpp.F90
```

### Platform specific notes

#### Mac OSX

The MacPorts package management system is a convenient way to install
and maintain many packages, including gcc which includes gfortran.

#### Windows/CYGWIN

User contributed code for Windows/CYGWIN has been added, but is
currently not tested and supported by the pFUnit team. At this
writing, 2013-1031, serial Examples and MPI are not known to be
supported. Please contact us if you wish to either contribute or
otherwise discuss this port.

#### Intel Fortran Version 13: `-DINTEL_13`

Using version 13 is deprecated. We have encountered problems using
version 13, which we believe may be due to subtle compiler bugs. We
strongly recommend upgrading to the latest version possible.

To make pFUnit work with Intel Fortran Version 13, please ensure that
`-DINTEL_13` is passed to the compiler when building or using
pFUnit. In the build process for pFUnit, this is added to the make
variables CPPFLAGS and FPPFLAGS. 

## Acknowledgments

Thanks to the follwing for their review and comments: B. Van Aartsen, T. Clune.

- Windows/CYGWIN contributions from E. Lezar.

- PGI port contributions from M. Leair (PG Group).

- Other acknowledgments: S.P. Santos (NCAR), M. Hambley (UK Met Office),
  J. Krishna (ANL), J. Ebo David.

## Known installations/versions

(git cognizant from "sourceforge.net/projects/pfunit")

- `master` - The current release.
- `development` - The cutting edge of pFUnit development.
- `mock_services` - Experimental support for mocking.
- `pfunit_2.1.0` - A feature freeze prior to a major upgrade of the preprocessor.
- `cray` - An intermediate port to Cray CCE.

## Notes

* For modifications and feature requests please see https://github.com/Goddard-Fortran-Ecosystem/pFUnit

## TODO

- Make other directory.
- Make Environment Modules example in other directory.
- Other build systems, e.g. CMake.

## REVISIONS TO THIS DOCUMENT

- 2015-1210 Minor changes to documentation. MLR
- 2015-0608 Added note about PFUNIT_EXTRA_USAGE (from MH). MLR
- 2015-0508 Some PGI workarounds removed for PGI 15.4. MLR
- 2015-0420 Clarified PFUNIT_MAX_ARRAY_RANK note. MLR
- 2015-0320 PGI port workarounds, including examples. 3.1. MLR
- 2014-1211 Minor updates for 3.0.2. MLR
- 2014-1110, 2014-1031 Minor edits. MLR
- 2014-0915 Minor updates for 3.0.1. MLR
- 2014-0404 Updated for release of 3.0. TLC
- 2014-0131, 2014-0205. Updated. MLR
- 2013-1107. Minor edits. MLR
- 2013-1031. Added user contributed code for Windows/CYGWIN & IBM's XLF. MLR
- 2013-0830-1359. Minor corrections and added MPIF90 to 6.2. MLR
- 2013-0806-1345. Corrected git reference. Was using old URL. MLR
- 2013-0805. Initial draft. MLR
