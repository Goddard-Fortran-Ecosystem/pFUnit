!---------------------------------------------------------------------------
! This file is external to the pfunit library and is used to represent
! information that cannot be determined at run-time.   In particular,
! Fortran lacks reflection/introspection and therefore cannot automatically
! collect user-defined test suites.
!
!
! Typical usage is for the user to compile this file while leaving it in the
! pFUnit installation directory.    Use "-D<_TEST_SUITES>" to specify a file
! that contains the list of test_suites to be builtlinked and executed.
!
!
! For serial runs, the user links with the FUnit library, while for parallel
! runs the user links with FUnit _and_ pFUnit.
!---------------------------------------------------------------------------


program main
   use FUnit
#ifdef PFUNIT_EXTRA_USE
      ! Use external code for whatever suite-wide fixture is in use.
      use PFUNIT_EXTRA_USE
#endif
   implicit none

   procedure(), pointer :: extra_initialize
   procedure(), pointer :: extra_finalize
   
#ifdef PFUNIT_EXTRA_INITIALIZE
   extra_initialize => PFUNIT_EXTRA_INITIALIZE
#else
   extra_initialize => stub
#endif

#ifdef PFUNIT_EXTRA_FINALIZE
   extra_finalize => PFUNIT_EXTRA_FINALIZE
#else
   extra_finalize => stub
#endif

   call funit_main(load_tests, extra_initialize, extra_finalize)

contains

   function load_tests() result(suite)

#define ADDULE_TEST_SUITE(m,s) use m, only: s
#define ADD_TEST_SUITE(s) ! do nothing
#include _TEST_SUITES
#undef ADDULE_TEST_SUITE
#undef ADD_TEST_SUITE

      type (TestSuite) :: suite

#define ADDULE_TEST_SUITE(m,s) ! do nothing
#define ADD_TEST_SUITE(s) type (TestSuite), external :: s
#  include _TEST_SUITES
#undef ADD_TEST_SUITE
#undef ADDULE_TEST_SUITE

      suite = TestSuite()

#define ADD_TEST_SUITE(s) call suite%addTest(s())
#define ADDULE_TEST_SUITE(m,s) call suite%addTest(s())
#  include _TEST_SUITES
#undef ADD_TEST_SUITE
#undef ADDULE_TEST_SUITE

   end function load_tests

end program main






