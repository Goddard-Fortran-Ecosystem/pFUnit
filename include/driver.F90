program main
   use sfunit
   implicit none

   call pfunit_main(load_all_possible_tests)

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






