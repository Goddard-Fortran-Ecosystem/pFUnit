#include "reflection.h"
module Test_Assert_mod
   use Assert_mod
   use Exception_mod, only: catch
   use Exception_mod, only: getNumExceptions
   implicit none
   private


   public :: suite 

contains

   function suite()
      use TestSuite_mod, only: newTestSuite, TestSuite
      use SimpleTestMethod_mod, only: newSimpleTestMethod, SimpleTestMethod
      type (TestSuite), pointer :: suite

      allocate(suite)
      suite = newTestSuite('Assert')

#define ADD(method) call suite%addTest(newSimpleTestMethod(REFLECT(method)))

      ADD(testAssertTrueF)
      ADD(testAssertTrueT)
      ADD(testAssertFalseT)
      ADD(testAssertFalseF)
      ADD(testAssertEqualStringSame)
      ADD(testAssertEqualStringDifferent)
      ADD(testAssertEqualIntegerScalarEqual)
      ADD(testAssertEqualIntegerScalarUnequal)
      ADD(testAssertWithLineNumber)
   end function suite

   subroutine testAssertTrueF()
      call assertTrue(.false.)
      call assertTrue(catch('Logical assertion failed.'))
   end subroutine testAssertTrueF

   subroutine testAssertTrueT()
      call assertTrue(.true.)
      call assertEqual(0, getNumExceptions())
   end subroutine testAssertTrueT

   subroutine testAssertFalseT()
      call assertFalse(.true.)
      call assertTrue(catch('Logical assertion failed.'))
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

   subroutine testAssertEqualIntegerScalarEqual()
      call assertEqual(expected=1, found=1)
      call assertTrue(0 == getNumExceptions())
   end subroutine testAssertEqualIntegerScalarEqual

   subroutine testAssertEqualIntegerScalarUnequal()
      call assertEqual(expected=0, found=1)
      call assertTrue(catch('Integer scalar assertion failed:' // new_line('A') // &
           &   '    expected: <0>' // new_line('A') // & 
           &   '   but found: <1>'))
   end subroutine testAssertEqualIntegerScalarUnequal

   subroutine testAssertWithLineNumber
      call assertTrue(.false., lineNumber = 5)
      call assertTrue(catch('Logical assertion failed at line <5>'))
   end subroutine testAssertWithLineNumber

end module Test_Assert_mod
