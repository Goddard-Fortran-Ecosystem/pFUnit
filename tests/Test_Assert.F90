#include "reflection.h"
module Test_Assert_mod
   use TestSuite_mod
   use Assert_mod
   use Exception_mod, only: catch
   use Exception_mod, only: getNumExceptions
   implicit none
   private

   public :: suite 

contains

   function suite() result(aSuite)
      use Test_mod
      use TestMethod_mod
      use TestSuite_mod
      type (TestSuite) :: aSuite

      aSuite = newTestSuite('Assert')

#define ADD(method) call aSuite%addTest(newTestMethod(REFLECT(method)))

      ADD(testAssertTrueF)
      ADD(testAssertTrueT)
      ADD(testAssertFalseT)
      ADD(testAssertFalseF)
      ADD(testAssertEqualStringSame)
      ADD(testAssertEqualStringDifferent)
      ADD(testAssertWithLocation)
   end function suite

   subroutine testAssertTrueF()
      call assertTrue(.false.)
      call assertTrue(catch('<>'))
   end subroutine testAssertTrueF

   subroutine testAssertTrueT()
      call assertTrue(.true.)
      call assertEqual(0, getNumExceptions())
   end subroutine testAssertTrueT

   subroutine testAssertFalseT()
      call assertFalse(.true.)
      call assertTrue(catch('<>'))
   end subroutine testAssertFalseT

   subroutine testAssertFalseF()
      call assertFalse(.false.)
      call assertEqual(0, getNumExceptions())
   end subroutine testAssertFalseF

   subroutine testAssertEqualStringSame()
      call assertEqual(expected="string A", found="string A")
      call assertEqual(0, getNumExceptions())
   end subroutine testAssertEqualStringSame

   subroutine testAssertEqualStringDifferent()
      call assertEqual(expected="string A", found="string B")
      call assertTrue(catch('String assertion failed:' // new_line('A') // &
           & '    expected: <"string A">' // new_line('A') // &
           & '   but found: <"string B">' // new_line('A') // &
           & '  first diff:   -------^'))
   end subroutine testAssertEqualStringDifferent

   subroutine testAssertEqualStringDiffer1st()
      call assertEqual(expected="a string A", found="string B")
      call assertTrue(catch('String assertion failed:' // new_line('A') // &
           & '    expected: <"a string A">' // new_line('A') // &
           & '   but found: <"string B">' // new_line('A') // &
           & '  first diff:   ^'))
   end subroutine testAssertEqualStringDiffer1st

   subroutine testAssertWithLocation
      use SourceLocation_mod
      call assertTrue(.false., 'intentional fail', SourceLocation(lineNumber=5))
      call assertTrue(catch('intentional fail'))
   end subroutine testAssertWithLocation

end module Test_Assert_mod
