#include "reflection.h"
module Test_AssertInteger_mod
   use AssertBasic_mod
   use AssertInteger_mod
   use TestSuite_mod, only: TestSuite, newTestSuite
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use TestMethod_mod, only: newTestMethod
      use Test_mod

      type (TestSuite) :: suite

      suite = newTestSuite('AssertIntegerTests')
#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))

      ADD(testAssertEqual_equal)
      ADD(testAssertEqual_unequal)

   end function suite

   subroutine testAssertEqual_equal()
      call assertEqual(2,2)
   end subroutine testAssertEqual_equal

   subroutine testAssertEqual_unequal()
      call assertEqual(2,3)
      call assertExceptionRaised('Integer scalar assertion failed:' // new_line('a') // &
           & '    expected: <2>' // new_line('a') // &
           & '   but found: <3>')
   end subroutine testAssertEqual_unequal

end module Test_AssertInteger_mod
