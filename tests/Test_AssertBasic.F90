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

      ADD(testAssertEqualStrIgnAllWhite1)
      ADD(testAssertEqualStrIgnAllWhite2)
      ADD(testAssertEqualStrIgnAllWhite3)
      ADD(testAssertEqualStrIgnAllWhite4)
      ADD(testAssertEqualStrIgnAllWhite5)
      ADD(testAssertEqualStrIgnAllWhite6)
      ADD(testAssertEqualStrIgnAllWhite7)

      ADD(testAssertEqualStrIgnWhiDif1)
      ADD(testAssertEqualStrIgnWhiDif2)
      ADD(testAssertEqualStrIgnWhiDif3)
      ADD(testAssertEqualStrIgnWhiDif4)
      ADD(testAssertEqualStrIgnWhiDif5)
      ADD(testAssertEqualStrIgnWhiDif6)
      ADD(testAssertEqualStrIgnWhiDif7)
      ADD(testAssertEqualStrIgnWhiDif8)

      ADD(testAssertEqualStringTrimWhitespace1)
      ADD(testAssertEqualStringTrimWhitespace2)
      ADD(testAssertEqualStringKeepWhitespace1)
      ADD(testAssertEqualStringKeepWhitespace2)

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

      ADD(testAssertFail)
      ADD(testAssertExceptionRaised)
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

   subroutine testAssertEqualStrIgnAllWhite1()
     call assertEqual(expected="stringA", found="string A", &
          & whitespace=IGNORE_ALL )
   end subroutine testAssertEqualStrIgnAllWhite1

   subroutine testAssertEqualStrIgnAllWhite2()
     call assertEqual(expected="string A", found="stringA", &
          & whitespace=IGNORE_ALL )
   end subroutine testAssertEqualStrIgnAllWhite2

   subroutine testAssertEqualStrIgnAllWhite3()
     call assertEqual(expected="stringA ", found="string A", &
          & whitespace=IGNORE_ALL )
   end subroutine testAssertEqualStrIgnAllWhite3

   subroutine testAssertEqualStrIgnAllWhite4()
     call assertEqual(expected=" string A", found="string A", &
          & whitespace=IGNORE_ALL )
   end subroutine testAssertEqualStrIgnAllWhite4

   subroutine testAssertEqualStrIgnAllWhite5()
     call assertEqual(expected=" string A ", found="stringA", &
          & whitespace=IGNORE_ALL )
   end subroutine testAssertEqualStrIgnAllWhite5

   subroutine testAssertEqualStrIgnAllWhite6()
     character tab; tab = char(9)
     call assertEqual(expected=tab//"string A ", found="stringA", &
          & whitespace=IGNORE_ALL )
   end subroutine testAssertEqualStrIgnAllWhite6

   subroutine testAssertEqualStrIgnAllWhite7()
     character tab; tab = char(9)
     call assertEqual(expected=tab//"string A ", found=" stringA"//tab, &
          & whitespace=IGNORE_ALL )
   end subroutine testAssertEqualStrIgnAllWhite7

   subroutine testAssertEqualStrIgnWhiDif1()
     character, parameter :: tab = char(9), spc = char(32)
     call assertEqual(expected=tab, found=spc, &
          & whitespace=IGNORE_DIFFERENCES)
   end subroutine

   subroutine testAssertEqualStrIgnWhiDif2()
     character, parameter :: tab = char(9), spc = char(32)
     call assertEqual(expected=spc, found=tab, &
          & whitespace=IGNORE_DIFFERENCES)
   end subroutine

   subroutine testAssertEqualStrIgnWhiDif3()
     character, parameter :: tab = char(9), spc = char(32)
     call assertEqual(expected=tab//"string A", found="string A", &
          & whitespace=IGNORE_DIFFERENCES)
   end subroutine

   subroutine testAssertEqualStrIgnWhiDif4()
     character, parameter :: tab = char(9), spc = char(32)
     call assertEqual(expected=tab//"string A", found=spc//"string A", &
          & whitespace=IGNORE_DIFFERENCES)
   end subroutine

   subroutine testAssertEqualStrIgnWhiDif5()
     character, parameter :: tab = char(9), spc = char(32)
     call assertEqual(expected=tab//"string A", found=spc//"string A"//spc, &
          & whitespace=IGNORE_DIFFERENCES)
   end subroutine

   subroutine testAssertEqualStrIgnWhiDif6()
     character, parameter :: tab = char(9), spc = char(32)
     call assertEqual( &
          & expected =spc//"string"//tab//spc//"6", &
          & found    ="string"//spc//spc//spc//spc//"6"//spc, &
          & whitespace=IGNORE_DIFFERENCES)
   end subroutine

   subroutine testAssertEqualStrIgnWhiDif7()
     character, parameter :: tab = char(9), spc = char(32)
     call assertEqual(expected=tab//"string 7 ", found="string7", &
          & whitespace=IGNORE_DIFFERENCES)

     call assertTrue(catch( &
          & 'String assertion failed:' // new_line('A') // &
          & '    expected: <"'//tab//'string 7">' // new_line('A') // &
          & '   but found: <"string7">'  // new_line('A') // &
          & '  first diff:   ------^'))

   end subroutine 

   subroutine testAssertEqualStrIgnWhiDif8()
     character, parameter :: tab = char(9), spc = char(32)
     call assertEqual(expected="string8A ", found="string8B", &
          & whitespace=IGNORE_DIFFERENCES)

     call assertTrue(catch( &
          & 'String assertion failed:' // new_line('A') // &
          & '    expected: <"string8A">' // new_line('A') // &
          & '   but found: <"string8B">'  // new_line('A') // &
          & '  first diff:   -------^'))

   end subroutine 

   subroutine testAssertEqualStringTrimWhitespace1()
     character tab; tab = char(9)
     call assertEqual(expected=tab//"string A ", found="string A", &
          & whitespace=TRIM_ALL )
   end subroutine testAssertEqualStringTrimWhitespace1

   subroutine testAssertEqualStringTrimWhitespace2()
     character tab; tab = char(9)
     ! Should fail !
     call assertEqual(expected=tab//"string A ", found="stringA", &
          & whitespace=TRIM_ALL )
     call assertTrue(catch( &
          & 'String assertion failed:' // new_line('A') // &
          & '    expected: <"'//tab//'string A">' // new_line('A') // &
          & '   but found: <"stringA">'  // new_line('A') // &
          & '  first diff:   ------^'))
   end subroutine testAssertEqualStringTrimWhitespace2

   subroutine testAssertEqualStringKeepWhitespace1()
     character tab; tab = char(9)
     call assertEqual(expected=tab//"string A ", found=tab//"string A ", &
          & whitespace=KEEP_ALL )
   end subroutine testAssertEqualStringKeepWhitespace1

   subroutine testAssertEqualStringKeepWhitespace2()
     character tab; tab = char(9)
     ! A strict interpretation of keep:  TAB != SPC !
     call assertEqual(expected=tab//"string A ", found=" string A ", &
          & whitespace=KEEP_ALL )
     call assertTrue(catch( &
          & 'String assertion failed:' // new_line('A') // &
          & '    expected: <"	string A ">' // new_line('A') // &
          & '   but found: <" string A ">' // new_line('A') // &
          & '  first diff:   ^'))
   end subroutine testAssertEqualStringKeepWhitespace2

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

   subroutine testAssertExceptionRaised()
      use Exception_mod, only: throw
      use SourceLocation_mod

      character(len=*), parameter :: message = 'a message'

      call throw(message)
      call assertExceptionRaised(message)

      call throw(message)
      call assertExceptionRaised(message,SourceLocation('here',5))

   end subroutine testAssertExceptionRaised

   subroutine testAssertFail()
      use SourceLocation_mod

      character(len=*), parameter :: message = 'a message'

      call assertFail(message)
      call assertExceptionRaised(message)

      call assertFail(message)
      call assertExceptionRaised(message,SourceLocation('here',5))
   end subroutine testAssertFail

end module Test_AssertBasic_mod
