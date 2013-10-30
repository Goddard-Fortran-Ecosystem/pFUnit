#include "reflection.h"
module Test_AssertBasic_mod
   use Exception_mod, only: NULL_MESSAGE
   use AssertBasic_mod
   use TestSuite_mod, only: TestSuite, newTestSuite
   use Exception_mod, only: catch
   use Exception_mod, only: getNumExceptions
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

      ADD(testAssertTrueF)
      ADD(testAssertTrueT)
      ADD(testAssertFalseT)
      ADD(testAssertFalseF)
      ADD(testAssertFalseF)
      ADD(testAssertEqualStringSame)
      ADD(testAssertEqualStringDifferent)
      ADD(testAssertAny)
      ADD(testAssertAnyFail)
      ADD(testAssertAll)
      ADD(testAssertAllFail)
      ADD(testAssertNone)
      ADD(testAssertNoneFail)
      ADD(testAssertNotAll)
      ADD(testAssertNotAllFail)

      ADD(testAssertIsNaN)
      ADD(testAssertIsFinite)
   end function suite

   subroutine testAssertTrueF()
      call assertTrue(.false.)
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertTrueF

   subroutine testAssertTrueT()
      call assertTrue(.true.)
   end subroutine testAssertTrueT

   subroutine testAssertFalseF()
      call assertFalse(.false.)
   end subroutine testAssertFalseF

   subroutine testAssertFalseT()
      call assertFalse(.true.)
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertFalseT

   subroutine testAssertEqualStringSame()
      call assertEqual(expected="string A", found="string A")
   end subroutine testAssertEqualStringSame

   subroutine testAssertEqualStringDifferent()
      call assertEqual(expected="string A", found="string B")
      call assertTrue(catch('String assertion failed:' // new_line('A') // &
           & '    expected: <"string A">' // new_line('A') // &
           & '   but found: <"string B">' // new_line('A') // &
           & '  first diff:   -------^'))
   end subroutine testAssertEqualStringDifferent

   ! Fail only if all .false.
   subroutine testAssertAny()
      call assertAny([.true.])
      call assertAny([.true., .true.])
      call assertAny([.true.,.false.])
      call assertAny([.false.,.true.])
   end subroutine testAssertAny

   subroutine testAssertAnyFail()
      call assertAny([.false.])
      call assertTrue(catch(NULL_MESSAGE))
      call assertAny([.false.,.false.])
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertAnyFail

   ! Fail if any .false.
   subroutine testAssertAll()
      call assertAll([.true.])
      call assertAll([.true., .true.])
   end subroutine testAssertAll

   subroutine testAssertAllFail()
      call assertAll([.false.])
      call assertTrue(catch(NULL_MESSAGE))
      call assertAll([.false.,.false.])
      call assertTrue(catch(NULL_MESSAGE))
      call assertAll([.true.,.false.])
      call assertTrue(catch(NULL_MESSAGE))
      call assertAll([.false.,.true.])
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertAllFail

   ! Fail if any .true.
   subroutine testAssertNone()
      call assertNone([.false.])
      call assertNone([.false., .false.])
   end subroutine testAssertNone

   subroutine testAssertNoneFail()
      call assertNone([.true.])
      call assertTrue(catch(NULL_MESSAGE))
      call assertNone([.false.,.true.])
      call assertTrue(catch(NULL_MESSAGE))
      call assertNone([.true.,.false.])
      call assertTrue(catch(NULL_MESSAGE))
      call assertNone([.true.,.true.])
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertNoneFail


   ! Fail if any .true.
   subroutine testAssertNotAll()
      call assertNotAll([.false.])
      call assertNotAll([.false., .true.])
      call assertNotAll([.true., .false.])
      call assertNotAll([.false., .false.])
   end subroutine testAssertNotAll

   subroutine testAssertNotAllFail()
      call assertNotAll([.true.])
      call assertTrue(catch(NULL_MESSAGE))
      call assertNotAll([.true.,.true.])
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertNotAllFail

   subroutine testAssertIsNaN()
      use MakeNaN_mod, only: makeNaN_32, makeNaN_64

      call assertIsNaN(1.e0, 'not NaN')
      call assertExceptionRaised('not NaN')
      call assertIsNaN(1.d0, 'not NaN')
      call assertExceptionRaised('not NaN')

      call assertIsNaN(makeNaN_32())
      call assertIsNaN(makeNaN_64())
 
   end subroutine testAssertIsNaN

   subroutine testAssertIsFinite()
      use MakeInfinity_mod, only: makeInf_32, makeInf_64

      call assertIsFinite(1.e0, 'finite')
      call assertIsFinite(1.d0, 'finite')

      call assertIsFinite(makeInf_32(), 'not finite')
      call assertExceptionRaised('not finite')
      call assertIsFinite(makeInf_64(), 'not finite')
      call assertExceptionRaised('not finite')

   end subroutine testAssertIsFinite

end module Test_AssertBasic_mod
