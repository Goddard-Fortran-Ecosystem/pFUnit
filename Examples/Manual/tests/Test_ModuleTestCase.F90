#include 'reflection.h'
module Test_ModuleTestCase_mod
   use ModuleExample, only: printout
   use Assert_mod
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use SimpleTestMethod_mod, only: newSimpleTestMethod, SimpleTestMethod
      type (TestSuite), pointer :: suite

      allocate(suite)
      suite = newTestSuite('ModuleExample')
#define ADD(method) call suite%addTest(newSimpleTestMethod(REFLECT(method)))

      ADD(testprintout)

   end function suite


   subroutine testprintout()
!      call assertEqual('+0.000000', toString(0.))
   end subroutine testprintout


end module Test_ModuleTestCase_mod
