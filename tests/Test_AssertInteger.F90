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
      ADD(testAssertEqual_unequalWithMessage)
      ADD(testAssertEqual1D1D_equal)
      ADD(testAssertEqual1D1D_nonconforming)
      ADD(testAssertEqual1D1D_conforming)
      ADD(testAssertEqual1D1D_unequalA)
      ADD(testAssertEqual1D1D_unequalB)
      ADD(testAssertEqual2D2D_equal)
      ADD(testAssertEqual2D2D_nonconforming)
      ADD(testAssertEqual2D2D_unequal)
      ADD(testAssertLestThan_falseA)
!!$      ADD(testAssertLestThan_falseB)
      ADD(testAssertLestThan_true)

   end function suite

   subroutine testAssertEqual_equal()
      call assertEqual(2,2)
   end subroutine testAssertEqual_equal

   subroutine testAssertEqual_unequal()
      call assertEqual(2,3)
      call assertExceptionRaised('expected: <2> but found: <3>')
   end subroutine testAssertEqual_unequal

   subroutine testAssertEqual_unequalWithMessage()
      call assertEqual(2,3,'what?')
      call assertExceptionRaised('what? expected: <2> but found: <3>')
   end subroutine testAssertEqual_unequalWithMessage

   subroutine testAssertEqual1D1D_equal()
      call assertEqual([1,2],[1,2])
   end subroutine testAssertEqual1D1D_equal

   subroutine testAssertEqual1D1D_nonconforming()
      call assertEqual([1,2],[1,2,3])
      call assertExceptionRaised('nonconforming arrays - expected shape: [2] but found shape: [3]')
   end subroutine testAssertEqual1D1D_nonconforming

   subroutine testAssertEqual1D1D_conforming()
      call assertEqual(1,[1,1,1])
   end subroutine testAssertEqual1D1D_conforming

   subroutine testAssertEqual1D1D_unequalA()
      call assertEqual([1,2,3],[1,3,3])
      call assertExceptionRaised('expected: <2> but found: <3> at position: [2]')
   end subroutine testAssertEqual1D1D_unequalA

   subroutine testAssertEqual1D1D_unequalB()
      call assertEqual(1, [1,2,1])
      call assertExceptionRaised('expected: <1> but found: <2> at position: [2]')
   end subroutine testAssertEqual1D1D_unequalB

   subroutine testAssertEqual2D2D_equal()
      integer :: array(2,3)
      array = reshape([1,2,3,4,5,6],[2,3])
      call assertEqual(array, array)
   end subroutine testAssertEqual2D2D_equal

   subroutine testAssertEqual2D2D_nonconforming()
      integer :: expected(2,3)
      integer :: found(3,5)

      expected = 1
      found = 1
      call assertEqual(expected, found)
      call assertExceptionRaised('nonconforming arrays - expected shape: [2,3] but found shape: [3,5]')

   end subroutine testAssertEqual2D2D_nonconforming

   subroutine testAssertEqual2D2D_unequal()
      integer :: expected(2,3)
      integer :: found(2,3)

      expected = 1
      found = 1
      found(1,2) = -1

      call assertEqual(expected, found)
      call assertExceptionRaised('expected: <1> but found: <-1> at position: [1,2]')

      found(1,2) = 1
      found(2,3) = -1

      call assertEqual(expected, found)
      call assertExceptionRaised('expected: <1> but found: <-1> at position: [2,3]')

   end subroutine testAssertEqual2D2D_unequal

   subroutine testAssertLestThan_falseA()
      call assertLessThan(1, 1)
      call assertExceptionRaised('expected: <1> to be less than: <1>')
   end subroutine testAssertLestThan_falseA

   subroutine testAssertLestThan_falseB()
      call assertLessThan(1, 2)
      call assertExceptionRaised('expected: <1> to be less than: <2>')
   end subroutine testAssertLestThan_falseB

   subroutine testAssertLestThan_true()
      call assertLessThan(1, 2)
   end subroutine testAssertLestThan_true

end module Test_AssertInteger_mod
