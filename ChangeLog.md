# ChangeLog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [4.16.0] - 2026-02-23

### Added

- Advanced test filtering with regex and glob patterns (issue #523)
  - `-f`/`--filter` flag supports POSIX regex on Unix/Linux/macOS, glob patterns on Windows
  - `-e`/`--exclude` flag for anti-filtering using glob patterns on all platforms
  - Multiple space-separated patterns supported (OR logic)
  - New modules: `RegexFilter`, `GlobFilter`, and C wrapper for POSIX regex
  - Backward compatible with existing simple substring filtering

### Changed

- Update submodule (fArgParse v1.11.0)
- Minor cleanup to CI
- Turned off NVHPC CI test as it seems the image is too large for Github Actions.  Will investigate further and re-enable when possible.
- Removed some unneeded debugging prints in `pFUnitParser.py`

### Fixed

- Fix `add_pfunit_ctest` to properly track test file dependencies (issue #380)
  - Adding new `.pf` files to `TEST_SOURCES` now triggers automatic rebuild without `make clean`
  - Test suite registry (`.inc` file) generation moved from configure-time to build-time
  - Added build-time dependency tracking between `.pf` files and generated registry
  - Driver recompilation now triggered automatically when registry changes
  - New helper script: `include/generate_test_suite_inc.cmake`

## [4.15.0] - 2025-11-24

### Changed

- Workaround for complex flang use case.
  - Modified an internal interface so that `load_tests` is now a subroutine. 
  - Also added a subroutine version of `TestSuite::filter()` (called `filter_sub()`
- Remove `gfortran-12` from macos CI tests

### Fixed

- Undo accidental case change in `add_pfunit_test` (introduced in #509) which led to empty `_TEST_SUITES`
- Enable `build-tests` and `tests` targets only if `ENABLE_TESTS` is `ON`.

## [4.14.0] - 2025-10-14

### Changed

- Add an enclosing `<testsuites>` element an xml version/encoding element in the funit xml output. Makes the output readable by more CI systems (Jenkins, GitLab-CI).
- Change CMake from using `PARENT_SCOPE` to `CACHE INTERNAL` as it seems in some setups using
  pFUnit via `FetchContent` the former does not work as expected.
- Update some `COMMENT` lines in functions with double-quotes to avoid warnings

### Fixed

- Fix parser on Windows for paths with different drive letters
- Updates for CMake versions newer than 3.30.
- Update NVHPC CI (build only)
- Fix CMake for LLVM Flang
- Update object library dependency handling (see #495)

## [4.13.0] - 2025-09-30

### Fixed

- Alter CMake in `add_pfunit_test.cmake` and `add_pfunit_ctest.cmake` to workaround ifx 2025.2 preprocessor bug

### Changed

- Update fArgParse submodule to v1.10.0
- Update CMake minimum version to 3.24 to match other GFE repos
- Remove `macos-13` from CI, add `macos-15`
- Add `gfortran-15` for macOS CI
- Fix handling of forward/backward slashes in the parser on Windows when using the usual Python Windows release (worked only with Python through MinGW before)
- Ensure testing returns non-zero value when tests fail or are in error.

## [4.12.0] - 2025-04-07

### Changed

- Updated minimum required CMake version to 3.12 in tests/ to match main CMakeLists.txt and documentation. Previously set to 3.0 which is now deprecated and was causing build issues.

## [4.11.1] - 2025-02-04

### Fixed

- Workaround for gfortran 13/14 on Ubuntu.   Failure does not show on other flavors of Linux no macos.

## [4.11.0] - 2025-02-03

### Changed

- Updated submodule for fArgParse
- Update CI to have `gfortran-10` and `gfortran-11` only on `ubuntu-22.04`
- Update CI NVIDIA to NVHPC 24.7
- Add Flang to CI

## [4.10.0] - 2024-07-10

### Changed

- Updated pFUnit to use v2 template interfaces
- Added `-quiet` flag for NAG Fortran
- Remove `macos-11` from GitHub Actions, add `macos-12` and `gfortran-14` to Ubuntu 24.04
- Updated fArgParse to v1.8.0

### Added

- LLVMFlang compiler support

### Fixed

- Fixes some ctest failures

## [4.9.0] - 2024-02-06

### Added

- Fujitsu compiler support

### Fixed

- Removes numerous SyntaxWarning messages during compilation and parsing when working with python >= 3.12 by using raw strings to hold all regular expressions.
- This fixes a small CMake bug which can lead to posix_predefined.x being built in the wrong build subdirectory when CMAKE_RUNTIME_OUTPUT_DIRECTORY is set*.
- Missing implementation of `assertIsFinite_real80()`.  Apparently undetected until recent attempt to port to flang.
- Made support for REAL128 optional.  (Port to nvfortran)

### Changed

- Updated the CI to use Intel LLVM compilers
- Removed obsolete documentation

## [4.8.0] - 2023-11-29

### Changed

- Updated submodule for fArgparse [v1.6.0]

## [4.7.4] - 2023-11-01

### Fixed

- Several workarounds added to enable building with gfortran 13.2.   Polymorphic assignment is broken, and must be replaced by `ALLOCATE(obj,source=...)`.  But apparently not everywhere?
- Add `-check nouninit` for Intel LLVM to work around [`ifx` bug](https://github.com/HPC-Bugs/reproducers/tree/main/compiler/Fortran/ifx/allocatable).

### Changed

- Updated CI to remove gcc-9 from macOS11 and add gcc-12

## [4.7.3] - 2023-07-21

### Fixed
- Fixed cmake issue where target "pfunit-mpi-defines" is defined more than once
- Missing variable declaration in parameterized test case boiler plate code.
  
## [4.7.2] - 2023-06-26

### Fixed
- Restored consistent behaviour for file paths between `add_pfunit_ctest` and `add_pfunit_sources`.
  Now the `add_pfunit_ctest` handles relative filepaths (e.g. `./path/to/source.pf`) as described
  in the script documentation.

## [4.7.1] - 2023-06-26

### Fixed
- Increased size of buffer for reporting real values in asserts.   Previous length was not quite enough for some 128 bit values, which resulted in EOR failures during execution.


## [4.7.0] - 2023-04-17

### Changed

- Update fArgParse submodule to v1.5.0

### Added

- Added IntelLLVM.cmake to support ifx
- Added interface for `@asertEquals` for arrays of strings.   Previously only string scalars could be compared.
- Added check in pFUnit preprocessor that raises an exception if the module name and filename do not agree unless `@suite` is used to override default assumptions.
- Added option to set labels to ctests
- Added changelog enforcer GitHub Action

### Fixed

- `--verbose option` is now passed through by ctest runner
- Converted GitHub CI to use cmake abstract build commands

## [4.6.3] - 2023-02-07

### Fixed

- Fix for compilers that do not support 128 bit reals

## [4.6.2] - 2023-01-23

### Fixed

- Fixed build_submodule for old git versions
- Fixed for use with FetchContent
- Fix CMake logic in `add_pfunit_ctest.cmake` for `MPIEXEC_EXECUTABLE`.  Problem not exposed by common MPI flavors which use `mpirun`
- Fix GitHub CI workflow by pinning to CMake 3.24.3
- Fixes for GNU Make builds
- Update fArgParse submodule to v1.4.2
- Fix `pFUnitParser.py` in cases where there is no module name

## [4.6.1] - 2022-11-15

### Fixed

- Restore Python2 compatibility in `pFUnitParser.py` script

## [4.6.0] - 2022-11-07

### Added

- Added `PFUNIT::pfunit-mpi-defines` ALIAS target
- Added option to `add_pfunit_ctest()` macro to specify `WORKING_DIRECTORY`.  This is
  the directory in which the specified test suite will _execute_. (Not where it is _built_).

### Changed

- Updated GitHub Actions
  - Moved to latest versions of "base" actions
  - Removed stale code
  - Added weekly run to try to keep MPI caches live

## [4.5.0] - 2022-11-07

### Changed

- Updated fArgParse submodule


## [4.4.2] - 2022-08-06

### Fixed

- Corrected CMake logic related to activating F08 support. (#366)  Previously only
  `-DENABLE_MPI_F08=1` would work, but other settings such as `-DENABLE_MPI_F08=YES` would
  not activate F08.

## [4.4.1] - 2022-06-01

### Fixed
- Corrected CMake logic in previous release.  Passed self CI tests but broke CI for other packages that use pFUnit.

## [4.4.0] - 2022-06-01

### Added

- CMake option to use `mpi_f08` interfaces:
```
    option (ENABLE_MPI_F08 "Use the 'mpi_f08' module." NO)
```

### Changed

- Updated GitHub Actions
  - OSs
    - Remove macos-10.15
    - Add ubuntu-22.04 and macos-12
  - Compilers
   - Removed gfortran-8
   - Added gfortran-11
   - Added gfortran-12 (for ubuntu-22.04)
- Updated how `collections` from Python is imported due to change in package

### Fixed

- Corrected (undetected?) failing test that incorrectly spelled its expected exception.
- Implemented small workaround for gfortran-11 for test to check if an array is empty. (Related to previous item.)


## [4.3.0] - 2022-04-20

### Fixed

- Bug in AssertNotEqual() for integers.  A return clause was missing.  This bug is
  ancient - apparentyl this routine is not used often.

### Changed

- Automated archive of full tar ball

## [4.2.7] - 2022-04-19

### Fixed

- Fixed unintended dependency on MPI in serial tests using `add_pfunit_ctest` macro

## [4.2.6] - 2022-04-19

### Fixed

- Fix erroneous library order in PFUNITconfig.cmake.in

## [4.2.5] - 2022-04-06

### Fixed

- Fix erroneous CMake MPI found logic from 4.2.4

## [4.2.4] - 2022-03-18

### Fixed

- Fixed MPI find logic issue exposed by CI testing. Was including PFUNIT::pfunit even if MPI was not found

## [4.2.3] - 2022-03-09

### Fixed

 - Incorrect treatment of 128 bit real support for compilers that do not support REAL128.
 - Incorrect compile flags for PGI

### Changed


- When any tests fail, the driver now invokes Fortran `STOP` instead of
  Fortran `ERROR STOP`.  This suppresses annoying back traces from
  GFortran.

- Changed `OTHER_SRCS` to `OTHER_SOURCES` in PFUNIT.mk.  The previous spelling
  is deprecated, but preserved to keep backwards compatibility.

- Add GitHub Action to automatically generate release tarball

## [4.2.2] - 2021-11-15

### Fixed
 - Fix for OpenMP handling in `PFUNITCmake.cmake`. If you skip OpenMP, it is no
   longer a dependency
 - Updated external modules that contain bugfixes.
 - Fixed cmake logic that fails on enabling tests if using submodules.
 - Allow GFortran to use longer lines.  (Impacts some upstream use cases.)

### Changed

 - Changed `OTHER_SRCS` to `OTHER_SOURCES` in PFUNIT.mk.  The previous spelling
   is deprecated, but preserved to keep backwards compatibility.

## [4.2.1] - 2021-03-24

This release fixes a CMake race condition in cmake when multiple ctests are
build in parallel in the same direcory.

### Fixed

 - race condition in CMake build of multiple ctests in same directory


## [4.2.0] - 2021-02-06

This release fixes some instability in the build that is related to the switch
to using namespaces and exporting targets.

### Added

 - Improved ability to embed pFUnit in the source tree of other projects.

### Changed

 - pFUnit now uses CMake namespaces.  Upstream projects should now link
   against `PFUNIT::funit` (or `PFUNIT::pfunit`) rather than just `funit`.  Users that
   build test suites using the `add_pfunit_ctest()` macro should not see an impact.

### Fixed

 - The `add_pfunit_ctest()` macro could fail under several not-so-rare
   circumstances.  One way is for CMake to fail to build
   `OTHER_SOURCES` before the driver as it cannot correctly analyze
   the indirect Fortran `USE PFUNIT_EXTRA_INITIALIZE` statement.  The
   other is when using paralle builds with multiple test suites using
   Intel and the `-save-temps` flag.  Here the compiler would overwrite the
   `driver.i90` in the build directory and produce confusing results.

   The solution is to use Cmake `configure_file()` to preprocess the driver
   directly on a per-suite basis.   This will allow CMake+FPP to corretly
   analyze dependencies and avoid reuse of `driver.i90`.

## [4.1.15] - 2021-01-06

### Added

- Enabled use off `add_subdirectory` and build directory directly.
  This improves the ability to build pFUnit when embedded within
  another project instead of building it as a separate project.

### Fixed

- Bug in assert for relatively equal.  Incorrect index for location of of first
  failing element.
- Workaround for WSL issue in driver.


## [4.1.14]

### Added
- Flag for position independent code.

## [4.1.13]

There is a ticket opened against Intel Fortran 19.2 which breaks some
fHamcrest functionality.  A failing test has been added to the test
suite.  19.1.3 still has this bug.  (I have not checked if earlier
compilers also had this problem, but quite possibly they do as
fHamcrest is still under development.)

## Changed

- Separated tests related to RobustRunner into separate test suite.  This is to
  facilitate CI, as the tests do intermittently fail under CI, though rarely
  in any development environment used by the main developers.

- Corrected so that default flags are with aggressive debugging.  This had been
  the default in the past, but was apparently lost for NAG to workaround a compiler
  bug in an intermediate release.  Also corrected the way the flags are managed
  for Debug vs Release.

## Fixed

- Added workaround for NAG+cmake+OpenMP.  An issue has been filed with kitware
  https://gitlab.kitware.com/cmake/cmake/-/issues/21280

- Missing PRESENT checks for arguments in TestMethod.  Not sure how
  these were not being caught - probably related to change mentioned
  above about skipping debug flags with NAG Fortran.

- Missing RECURSIVE attribute on function in Every.F90.  Not necessary
  in F2008, but not all compilers have implemented this yet.

## [4.1.12] - 2020-08-21

### Fixed
- Problem with FHamcrest `equal_to` where when the expected value is a numeric
  array and the actual value is any type except a numeric array the test still
  passes, such as: ` @assert_that(1, is(equal_to([2, 3, 4])))` passes.

- Problem with FHamcrest tests causing a segment fault when a test fails that
  involves a complex number.

### Added
- Tests for FHamcrest `equal_to` functionality.

### Changed
- Changed `CMakeLists.txt` for FHamcrest tests to use the new `pfunit_add_ctest`
  cmake macro.


## [4.1.11] - 2020-08-04

### Fixed
- Added flag that allows GFortran 10.x to compile with argument mismatches
  in MPI layer.  This apparently is only an issue for some MPI flavors, with
  others importing the correct interface variability via ```use mpi```.


## [4.1.10] - 2020-07-29

### Fixed
- Fixed problem under WSL+Gfortran-9 in which -O0 crashed pFUnit self tests.

## [4.1.9] - 2020-05-29

### Fixed
- Changed internal cmake dependencies so that "make test" will now build
  tests.   The tests and test executables are intentionally EXCLUDE_FROM_OLL
  which interferes with some canonical ways of driving cmake projects.
  "make tests" will continue to work as before.


## [4.1.8] - 2020-05-12

- Compiler workarounds
  - Changes in gFTL maps to allow IBM XLF to compile
  - Workaround for GFortran regression in 10.1 release
- Submodule updates

## [4.1.7] - 2020-03-06

### Added
- Allow @before without requiring @after

### Changed
- Fixed support for building shared libraries
- Updated to latest submodules


## [4.1.6] - 2020-03-05

### Fixed
  - Corrected problem with cmake -DSKIP_FHAMCREST
  - Command line arguments not working with MPI


## [4.1.5] - 2019-12-12

### Changed
- Fixed XML
- Updated to latest external dependencies (bugfix in fArgParse)
- Start using new format for ChangeLog.md

## [4.1.4] - 2019-12-19

- Fix bug in exported pFUnit.mk file.    Was not correctly expanding openmp libraries for projects built with Make.
- Improved pFunit driver to avoid name conflicts with user defined initialize()

## [4.1.3] - 2019-12-10

- Fix bug in add_pfunit_ctest() macro involving path
  Only affects using with Intel MPI

## [4.1.2] - 2019-12-07

- Fix minor bug related to OpenMP propagation

## [4.1.1] - 2019-11-10

- Fix for #122 (allow add_pfunit_test() with abs path)


## [4.1.0] - November 08, 2019

- Major correction to README to reflect changes from v3.0
- Expansion of hamcrest capabilities
- Completed TAP listener
- Fixes for self-tests that were failing due to Python-3 support


## [4.0.1] - September 01, 2019

- various (still incomplete) improvements to the README instructions.
- Added preprocessor support for hamcrest @assert_that
- Corrected issue with absolute paths for add_pfunit_sources()

## [4.0.0] - May 2019

- Major cleanup of source code and CMake logic.
- Single build now builds both serial and mpi support layers
- Introduced initial hamcrest support
- Improved mechanism for cmake building of test suites.
- Simplified overload process for assertions.

## [3.2.7] - May 24, 2016

- Fix 3.2.7 broke the NAG 6.0 compiler (internal compiler error).
  Workaround is to simply not use -C=all during the build for now.

## [3.2.6] - May 24, 2016

- Bug fix for several issues identified by the latest NAG compiler
  (6.1).  These are mostly related to nostandard usage of TARGET
  attributes that are unsafe for copyin/copyout.  Many thanks to NAG
  for helping to identify the problems.  Note: these bugs are
  innocuous under most circumstances/compilers.

## [3.2.5] - April 27, 2016

- Another bug that prevented the compiler version workaround from
          being handled correctly.

## [3.2.4] - April 27, 2016

- Bug fix - earlier merge broke unix test for Intel compiler
          version.  This prevented fix in 3.2.2 from being used.

## [3.2.3] - April 25, 2016

- Fixed mistake in OpenMP introduced during previous bug fix.

## [3.2.2] - April 24, 2016

- Workaround for ifort 16.0.2 bug with openmp
- Various minor improvements to code:
  - Fixed inconsistent names in self tests.
  - Introduced "-qopenmp" in find_package for OpenMP
  - RemoteProxy now ignores output starting with "DEBUG:" - useful
   for debugging self tests.


## [3.2.1] - April 21, 2016

- Trivial bug fix in include/driver.F90.   Missed in rush to do release 3.2.1.

## [3.2.0] - April 21, 2016

- Extension: support test to run on "all available" pes.
  - This is primarily aimed to enable testing coarray Fortran procedures
    CAF does not yet have "teams", so tests must use all images.  But
    MPI users may find it useful as well.   Just use "*" instead of a number
    when specifying NPES.   True CAF support should be expected in release 4.0.0.
- Extension: support test case with custom constructor.   With this extension was
  able to create external extensions to the framework that allow testing ESMF grideded
  components.  Contact Tom Clune if you want to have those extensions - did not want to
  induce an ESMF requirement for pFUnit.
- Improved support for CMake (contributed by Pal Levold)
  - packaging
  - add_pfunit_test() macro
  - Ctests (CMake testing package)
  - NOTE: now requires a more recent CMake version.
- Increased max filename length to avoid truncation
- Disabled --verbose command line option (breaks under gcc)
- Cleanup to reduce/eliminate compiler warnings when building.
- There is a regression in NAG 6.1, so NAG users should continue to use 6.0 until a
  fix or a workaround are found

## [3.1.1]

- PGI 15.7 appears to be working robustly
  - some previously necessary workarounds have been removed
- Additional documentation for XML printer
- Improved compliance with JUnit for XML printer
- Bug fix for RemoteProxyTestCase
- CMake workaround for OpenMPI 1.8.8 which otherwise complains about nested
  MPI programs in self tests.
- Fixed KEEP_ALL option in AssertEqual for zero-length strings.
  Because "" == " " in Fortran, AssertEqual(""," ",whitespace=KEEP_ALL),
  which should throw an exception, failed silently for zero-length arrays.
- Fixed the make clean bug in Example code.
- Added time out command line arguments.
- Added PFUNIT_EXTRA_USAGE in include/driver.F90 for suite-wide fixture use.
- Cray workarounds on a separate branch (hope for a release in November).

## [3.1] - March 20, 2015

- PGI 15.1 now supported.
- Asserts over integer arrays now supported. INT32 and INT64 support added.
- Consolidated assertAssociated directives to:
    ```
    @assertAssociated
    @assertNotAssociated```
- Added ifndef option to preprocessor directives.
- Fixes: Name length checking, unbalanced allocate, python 3 basestring.

## [3.0.2] - December 12, 2014

- Corrected lack of PRESENT check on some optional arguments.
- Brought integer array version of assertEqual up to level of other numbers.
- Directives added:
  ```
	    @assertEquivalent(...)
	    @assertEqual(a,b)
	    @assertAssociated(...), @assertUnAssociated(...)
	    @assertAssociatedWith(...), @assertUnAssociatedWith(...)
   ```
- Added code to parse brackets in directive arguments,
    allowing @assertEquivalent([...],[...]).
    Needed for directives that must parse arguments to construct other calls.
- Extended assertTrue and assertFalse to cover arrays of logical.
- Removed dependency on CPP stringification in the REFLECT macro simplifying build.
- Improved portability of build, fixing problem with OUTPUT_FLAG, i.e. "-o".
- Fixed build problem on NAG, cmake/gmake, and OS X.
- Replaced explicit invocations of python with $(PYTHON), set in GNUmakefile,
    to aid specification.
- Removed an extraneous allocate (Patch 5).

## [3.0.1] - September 15, 2014

- Fixed parser bug that was not recognizing user-provided procedures
	  annotated with @before/@after for MPI tests.
- Corrected end-of-run logic in include/driver.F90.
- Minor corrections & simplifications to build process.
- Improved compilation time by refactoring automatically generated code.
- Added compile-time configuration parameter to control maximum rank
  supported by assertions over arrays, e.g. AssertReal.
- Added "whitespace=IGNORE_DIFFERENCES" and similar options to AssertEqual.

## [3.0.0] - April 04, 2014

- Design improvement that unfortunately breaks GFortran prior to
  4.8.3 and 4.9.0 (main reason for major release)
- Default driver now produces useful return code in serial (and some MPI)
- New assertions for floating point:  <, <=, >, >=
- Various improvements to parser


