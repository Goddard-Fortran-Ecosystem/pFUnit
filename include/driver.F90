program main
   use sfunit
   use pf_main_mod
   implicit none

   procedure(), pointer :: extra_initialize
   procedure(), pointer :: extra_finalize
   
#ifdef PFUNIT_EXTRA_INITIALIZE
   extra_initialize => pfunit_extra_initialize
#else
   extra_initialize => null()
#endif

#ifdef PFUNIT_EXTRA_FINALIZE
   extra_finalize => pfunit_extra_initialize
#else
   extra_finalize => null()
#endif
   
   call pfunit_main(load_all_possible_tests, &
        extra_initialize, extra_finalize)

contains

   subroutine load_all_possible_tests(suite)

#define ADD_MODULE_TEST_SUITE(m,s) use m, only: s
#define ADD_TEST_SUITE(s) ! do nothing
#include _TEST_SUITES
#undef ADD_MODULE_TEST_SUITE
#undef ADD_TEST_SUITE

      type (TestSuite), intent(out) :: suite

#define ADD_MODULE_TEST_SUITE(m,s) ! do nothing
#define ADD_TEST_SUITE(s) type (TestSuite), external :: s
#  include _TEST_SUITES
#undef ADD_TEST_SUITE
#undef ADD_MODULE_TEST_SUITE

      suite = newTestSuite()

#define ADD_TEST_SUITE(s) call suite%addTest(s())
#define ADD_MODULE_TEST_SUITE(m,s) call suite%addTest(s())
#  include _TEST_SUITES
#undef ADD_TEST_SUITE
#undef ADD_MODULE_TEST_SUITE

   end subroutine load_all_possible_tests

end program main






