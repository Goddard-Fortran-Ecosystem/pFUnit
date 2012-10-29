#include "reflection.h"
module Test_StringUtilities_mod
   use StringUtilities, only: toString
   use Assert_mod
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use SimpleTestMethod_mod, only: newSimpleTestMethod
      type (TestSuite), pointer :: suite

      allocate(suite)
      suite => newTestSuite('StringUtilities')
#define ADD(method) call suite%addTest(newSimpleTestMethod(REFLECT(method)))

      ADD(testToStringInteger1D)
      ADD(testToString_realZero)
      ADD(testToString_realPositive)
      ADD(testToString_realNegative)

   end function suite

   subroutine testToStringInteger1D()
      call assertEqual('2',toString([2]))
      call assertEqual('2,3',toString([2,3]))
      call assertEqual('2,3,4',toString([2,3,4]))
   end subroutine testToStringInteger1D


   subroutine testToString_realZero()
      call assertEqual('+0.000000', toString(0.))
   end subroutine testToString_realZero

   subroutine testToString_realPositive()
      call assertEqual('+0.1000000E-01',toString(0.0100000))
      call assertEqual('+0.1000001',toString(0.1000001))
      call assertEqual('+1.0000000',toString(1.))
      call assertEqual('+1.000001',toString(1.000001))
      call assertEqual('+12.12345',toString(12.12345))
      call assertEqual('+123.1234',toString(123.1234))
      call assertEqual('+1234.123',toString(1234.123))
      call assertEqual('+12345.12',toString(12345.12))
      call assertEqual('+123456.1',toString(123456.1))
      call assertEqual('+1234567.',toString(1234567.))
   end subroutine testToString_realPositive

   subroutine testToString_realNegative()
      call assertEqual('-1.0000000',toString(-1.))
      call assertEqual('-1.000001',toString(-1.000001))
      call assertEqual('-12.12345',toString(-12.12345))
      call assertEqual('-123.1234',toString(-123.1234))
      call assertEqual('-1234.123',toString(-1234.123))
      call assertEqual('-12345.12',toString(-12345.12))
      call assertEqual('-123456.1',toString(-123456.1))
      call assertEqual('-1234567.',toString(-1234567.))
   end subroutine testToString_realNegative

end module Test_StringUtilities_mod
