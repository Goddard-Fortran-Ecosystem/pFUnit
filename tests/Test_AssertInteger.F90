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

      ADD(testAssertLessThan_falseA)
      ADD(testAssertLessThan_falseB)
      ADD(testAssertLessThan_true)

      ADD(testAssertLessThanOrEqual_false)
      ADD(testAssertLessThanOrEqual_trueA)
      ADD(testAssertLessThanOrEqual_trueB)

      ADD(testAssertGreaterThan_falseA)
      ADD(testAssertGreaterThan_falseB)
      ADD(testAssertGreaterThan_true)

      ADD(testAssertGreaterThanOrEqual_false)
      ADD(testAssertGreaterThanOrEqual_trueA)
      ADD(testAssertGreaterThanOrEqual_trueB)

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

   subroutine testAssertLessThan_falseA()
      call assertLessThan(1, 1)
      call assertExceptionRaised('expected: <1> to be less than: <1>')
   end subroutine testAssertLessThan_falseA

   subroutine testAssertLessThan_falseB()
      call assertLessThan(2, 1)
      call assertExceptionRaised('expected: <2> to be less than: <1>')
   end subroutine testAssertLessThan_falseB

   subroutine testAssertLessThan_true()
      call assertLessThan(1, 2)
   end subroutine testAssertLessThan_true
   
   subroutine testAssertLessThanOrEqual_false()
      call assertLessThanOrEqual(2, 1)
      call assertExceptionRaised('expected: <2> to be less than or equal to: <1>')
   end subroutine testAssertLessThanOrEqual_false

   subroutine testAssertLessThanOrEqual_trueA()
      call assertLessThanOrEqual(2, 2)
   end subroutine testAssertLessThanOrEqual_trueA

   subroutine testAssertLessThanOrEqual_trueB()
      call assertLessThanOrEqual(1, 2)
   end subroutine testAssertLessThanOrEqual_trueB
   
   subroutine testAssertGreaterThan_falseA()
      call assertGreaterThan(1, 1)
      call assertExceptionRaised('expected: <1> to be greater than: <1>')
   end subroutine testAssertGreaterThan_falseA

   subroutine testAssertGreaterThan_falseB()
      call assertGreaterThan(1, 2)
      call assertExceptionRaised('expected: <1> to be greater than: <2>')
   end subroutine testAssertGreaterThan_falseB

   subroutine testAssertGreaterThan_true()
      call assertGreaterThan(2, 1)
   end subroutine testAssertGreaterThan_true
   
   subroutine testAssertGreaterThanOrEqual_false()
      call assertGreaterThanOrEqual(1, 2)
      call assertExceptionRaised('expected: <1> to be greater than or equal to: <2>')
   end subroutine testAssertGreaterThanOrEqual_false

   subroutine testAssertGreaterThanOrEqual_trueA()
      call assertGreaterThanOrEqual(2, 2)
   end subroutine testAssertGreaterThanOrEqual_trueA

   subroutine testAssertGreaterThanOrEqual_trueB()
      call assertGreaterThanOrEqual(2, 1)
   end subroutine testAssertGreaterThanOrEqual_trueB
   
end module Test_AssertInteger_mod
