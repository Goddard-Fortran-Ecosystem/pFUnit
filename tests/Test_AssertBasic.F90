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
      use Params_mod, only: r32, r64

      call assertIsNaN(1.e0, 'not NaN')
      call assertExceptionRaised('not NaN')
      call assertIsNaN(1.d0, 'not NaN')
      call assertExceptionRaised('not NaN')

      call assertIsNaN(makeNaN_32())
      call assertIsNaN(makeNaN_64())

   contains

      function makeNaN_32() result(NaN_32)
        real(r32) :: NaN_32
        integer, parameter :: i32 = selected_int_kind(8)
        integer(i32), parameter :: nan_bits_32 = int(Z'7FA00000',i32)

        NaN_32 = transfer(nan_bits_32, NaN_32)

      end function makeNaN_32

      function makeNaN_64() result(NaN_64)
        real(r64) :: NaN_64
        integer, parameter :: i64 = selected_int_kind(18)
        integer(i64), parameter :: nan_bits_64 = int(Z'7FF4000000000000',i64)

        NaN_64 = transfer(nan_bits_64, NaN_64)

      end function makeNaN_64


   end subroutine testAssertIsNaN

   subroutine testAssertIsFinite()
      use Params_mod, only: r32, r64

      call assertIsFinite(1.e0, 'finite')
      call assertIsFinite(1.d0, 'finite')

      call assertIsFinite(makeInf_32(), 'not finite')
      call assertExceptionRaised('not finite')
      call assertIsFinite(makeInf_64(), 'not finite')
      call assertExceptionRaised('not finite')

   contains

      function makeInf_32() result(Inf_32)
        real(r32) :: Inf_32
        integer, parameter :: i32 = selected_int_kind(8)
        integer(i32), parameter :: inf_bits_32 = int(Z'7F800000',i32)

        Inf_32 = transfer(inf_bits_32, Inf_32)

      end function makeInf_32

      function makeInf_64() result(Inf_64)
        real(r64) :: Inf_64
        integer, parameter :: i64 = selected_int_kind(18)
        integer(i64), parameter :: inf_bits_64 = int(Z'7FF0000000000000',i64)

        Inf_64 = transfer(inf_bits_64, Inf_64)

      end function makeInf_64

   end subroutine testAssertIsFinite

end module Test_AssertBasic_mod
