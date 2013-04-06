#include "reflection.h"
!TODO: Tests for 1D arrays with tolerance
!TODO: Tests for 1D array with expected scalar

module Test_AssertReal_mod
   use TestSuite_mod
   use StringUtilities_mod, only: toString
   use StringUtilities_mod, only: MAXLEN_STRING
   use AssertBasic_mod
   use AssertReal_mod

   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: newTestSuite, TestSuite
      use TestMethod_mod, only: newTestMethod
      type (TestSuite) :: suite

      suite = newTestSuite('assertRealSuite')

#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))

      ADD(testValuesReport)
      ADD(testDifferenceReport)

      ADD(testExactUnequalA)
      ADD(testExactUnequalB)
      ADD(testExactEqual)

      ADD(testEqualsWithinTolerance)
      ADD(testEqualsNotWithinTolerance)

      ADD(testEquals_1D1D_nonConformableA)
      ADD(testEquals_1D1D_nonConformableB)
      ADD(testEquals_1D1D_nonConformableC)
      ADD(testEquals_1D1D_diffA)
      ADD(testEquals_1D1D_diffB)

   end function suite

   subroutine testValuesReport()
      use Assert_mod, only: assertEqual
      character(len=MAXLEN_STRING) :: one
      one = toString(1.)

      call assertEqual('expected: <' // trim(one) // '> but found: <' // trim(one) // '>', &
           & valuesReport(1.,1.))
   end subroutine testValuesReport

   subroutine testDifferenceReport()
      use Assert_mod, only: assertEqual
      character(len=MAXLEN_STRING) :: one
      one = toString(1.)

      call assertEqual('    difference: |' // trim(one) // '| > tolerance:' // trim(one), &
           & differenceReport(1.,1.))
   end subroutine testDifferenceReport

   subroutine testExactUnequalA()
      call checkUnequal(1., 1.0001)
   end subroutine testExactUnequalA

   subroutine testExactUnequalB()
      call checkUnequal(2., 3.)
   end subroutine testExactUnequalB

   subroutine checkUnequal(expected, found)
      real, intent(in) :: expected
      real, intent(in) :: found

      real :: difference

      difference = found - expected

      call assertEqual(expected, found)
      call assertCatch( &
           & trim(valuesReport(expected, found)) // new_line('$') // &
           & trim(differenceReport(difference, tolerance = 0.)) &
           & )
   end subroutine checkUnequal

   subroutine checkNotWithinTolerance(expected, found, tolerance)
      real, intent(in) :: expected
      real, intent(in) :: found
      real, intent(in) :: tolerance

      real :: difference

      difference = found - expected

      call assertEqual(expected, found, tolerance)
      call assertCatch( &
           & trim(valuesReport(expected, found)) // new_line('$') // &
           & trim(differenceReport(difference, tolerance)) &
           & )
   end subroutine checkNotWithinTolerance

   subroutine testExactEqual()
      use Exception_mod, only: getNumExceptions
      use Assert_mod, only: assertEqual
      call assertEqual(1., 1.)
      call assertEqual(0, getNumExceptions())
   end subroutine testExactEqual

   subroutine testEqualsWithinTolerance()
      use Exception_mod, only: getNumExceptions
      use Assert_mod, only: assertEqual
      call assertEqual(1., 1.1, tolerance = 0.1001)
      call assertEqual(0, getNumExceptions())
      call assertEqual(1., 0.9, tolerance = 0.1001)
      call assertEqual(0, getNumExceptions())
   end subroutine testEqualsWithinTolerance

   subroutine testEqualsNotWithinTolerance()
      call checkNotWithinTolerance(1., 1.11, 0.1)
      call checkNotWithinTolerance(1., 0.89, 0.1)
   end subroutine testEqualsNotWithinTolerance

   subroutine testEquals_1D1D_nonConformableA()
      call assertEqual([1.], [1.,2.])
      call assertExceptionRaised('nonconforming arrays - expected shape: [1] but found shape: [2]')
   end subroutine testEquals_1D1D_nonConformableA

   subroutine testEquals_1D1D_nonConformableB()
      call assertEqual([1.,2.], [1.])
      call assertExceptionRaised('nonconforming arrays - expected shape: [2] but found shape: [1]')
   end subroutine testEquals_1D1D_nonConformableB

   subroutine testEquals_1D1D_nonConformableC()
      use Exception_mod, only: getNumExceptions
      use Assert_mod, only: assertEqual
      call assertEqual([1.,2.], [1.,2.])
      call assertEqual(0, getNumExceptions())
   end subroutine testEquals_1D1D_nonConformableC

   subroutine testEquals_1D1D_diffA()
      call assertEqual([1.,2.], [0.,2.])
      call assertCatch( &
           & 'Assertion failed: unequal real 1D arrays.' // new_line('$') // & 
           & '  First difference at element <[1]>' // &
           &  trim(valuesReport(expected=1., found=0.)) // &
           &  trim(differenceReport(0.-1., tolerance = 0.)) &
           & )
   end subroutine testEquals_1D1D_diffA

   subroutine testEquals_1D1D_diffB()
      call assertEqual([1.,0.], [1.,2.])
      call assertCatch( &
           & 'Assertion failed: unequal real 1D arrays.' // new_line('$') // & 
           & '  First difference at element <[2]>'      //  &
           &  trim(valuesReport(expected=0., found=2.)) // &
           &  trim(differenceReport(2.-0., tolerance = 0.)) &
           & )
   end subroutine testEquals_1D1D_diffB

   subroutine assertCatch(string)
      use Exception_mod, only: getNumExceptions, Exception, catchAny
      use Assert_mod, only: assertEqual
      character(len=*), intent(in) :: string
      type (Exception) :: anException

      if (getNumExceptions() > 0) then
         anException = catchAny()
         call assertEqual(string, anException%getMessage())!, 'exceptions do not match')
      else
         call assertEqual(string, ' ')!, 'missing exception')
      end if
   end subroutine assertCatch

end module Test_AssertReal_mod
