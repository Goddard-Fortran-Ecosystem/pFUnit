!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_AssertBasic
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 20 Mar 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 20 Mar 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module Test_AssertBasic
   use PF_Exception, only: NULL_MESSAGE
   use PF_AssertBasic
   use PF_ExceptionList, only: catch
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite, only: TestSuite
      use PF_TestMethod, only: TestMethod
      use PF_Test

      type (TestSuite) :: suite

      suite = TestSuite('AssertIntegerTests')
!#define ADD(method) call suite%addTest(TestMethod(REFLECT(method)))

      call suite%addTest( &
           &   TestMethod('testAssertTrueF', &
           &                  testAssertTrueF))
      call suite%addTest( &
           &   TestMethod('testAssertTrueF1', &
           &                  testAssertTrueF1))
      call suite%addTest( &
           &   TestMethod('testAssertTrueF2', &
           &                  testAssertTrueF2))
      call suite%addTest( &
           &   TestMethod('testAssertTrueT', &
           &                  testAssertTrueT))
      call suite%addTest( &
           &   TestMethod('testAssertTrueT1', &
           &                  testAssertTrueT1))
      call suite%addTest( &
           &   TestMethod('testAssertFalseT', &
           &                  testAssertFalseT))
      call suite%addTest( &
           &   TestMethod('testAssertFalseT1', &
           &                  testAssertFalseT1))
      call suite%addTest( &
           &   TestMethod('testAssertFalseF', &
           &                  testAssertFalseF))
      call suite%addTest( &
           &   TestMethod('testAssertFalseF1', &
           &                  testAssertFalseF1))
      call suite%addTest( &
           &   TestMethod('testAssertAny', &
           &                  testAssertAny))
      call suite%addTest( &
           &   TestMethod('testAssertAnyFail', &
           &                  testAssertAnyFail))
      call suite%addTest( &
           &   TestMethod('testAssertAll', &
           &                  testAssertAll))
      call suite%addTest( &
           &   TestMethod('testAssertAllFail', &
           &                  testAssertAllFail))
      call suite%addTest( &
           &   TestMethod('testAssertNone', &
           &                  testAssertNone))
      call suite%addTest( &
           &   TestMethod('testAssertNoneFail', &
           &                  testAssertNoneFail))
      call suite%addTest( &
           &   TestMethod('testAssertNotAll', &
           &                  testAssertNotAll))
      call suite%addTest( &
           &   TestMethod('testAssertNotAllFail', &
           &                  testAssertNotAllFail))

      call suite%addTest( &
           &   TestMethod('testAssertIsNaN', &
           &                  testAssertIsNaN))
      call suite%addTest( &
           &   TestMethod('testAssertIsFinite', &
           &                  testAssertIsFinite))

      call suite%addTest( &
           &   TestMethod('testAssertIsNotNaN', &
           &                  testAssertIsNotNaN))
      call suite%addTest( &
           &   TestMethod('testAssertIsInfinite', &
           &                  testAssertIsInfinite))
      call suite%addTest( &
           &   TestMethod('testAssertFail', &
           &                  testAssertFail))
      call suite%addTest( &
           &   TestMethod('testAssertExceptionRaised', &
           &                  testAssertExceptionRaised))
   end function suite

   subroutine testAssertTrueF()
      call assertTrue(.false.)
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertTrueF

   subroutine testAssertTrueF1()
      call assertTrue([.false.].eqv.[.true.])
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertTrueF1

   subroutine testAssertTrueF2()
      call assertTrue([.true.,.false.].eqv.[.true.,.true.])
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertTrueF2

   subroutine testAssertTrueT()
      call assertTrue(.true.)
   end subroutine testAssertTrueT

   subroutine testAssertTrueT1()
      call assertTrue([.true.,.true.].eqv.[.true.,.true.])
   end subroutine testAssertTrueT1

   subroutine testAssertFalseF()
      call assertFalse(.false.)
   end subroutine testAssertFalseF

   subroutine testAssertFalseF1()
      call assertFalse([.false.,.false.].eqv.[.true.,.true.])
   end subroutine testAssertFalseF1

   subroutine testAssertFalseT()
      call assertFalse(.true.)
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertFalseT

   subroutine testAssertFalseT1()
      call assertFalse([.true.,.true.,.true.])
      call assertTrue(catch(NULL_MESSAGE))
   end subroutine testAssertFalseT1

   
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
      use MakeNaN, only: makeNaN_32, makeNaN_64

      call assertIsNaN(1.e0, 'not NaN')
      call assertExceptionRaised('not NaN')
      call assertIsNaN(1.d0, 'not NaN')
      call assertExceptionRaised('not NaN')

      call assertIsNaN(makeNaN_32())
      call assertIsNaN(makeNaN_64())
 
   end subroutine testAssertIsNaN


   subroutine testAssertIsNotNaN()
      use MakeNaN, only: makeNaN_32, makeNaN_64

      call assertIsNotNaN(1.e0,'not Nan')
      call assertIsNotNaN(makeNan_32(),'is NaN')
      call assertExceptionRaised('is NaN')

      call assertIsNotNaN(1.d0,'not Nan')
      call assertIsNotNaN(makeNan_64(),'is NaN')
      call assertExceptionRaised('is NaN')

 
   end subroutine testAssertIsNotNaN


   subroutine testAssertIsFinite()
      use MakeInf, only: makeInf_32, makeInf_64

      call assertIsFinite(1.e0, 'finite')
      call assertIsFinite(1.d0, 'finite')

      call assertIsFinite(makeInf_32(), 'not finite')
      call assertExceptionRaised('not finite')
      call assertIsFinite(makeInf_64(), 'not finite')
      call assertExceptionRaised('not finite')

   end subroutine testAssertIsFinite

   subroutine testAssertIsInfinite()
      use MakeInf, only: makeInf_32, makeInf_64

      call assertIsInfinite(1.e0, 'finite')
      call assertExceptionRaised('finite')
      call assertIsInfinite(1.d0, 'finite')
      call assertExceptionRaised('finite')

      call assertIsInfinite(makeInf_32())
      call assertIsInfinite(makeInf_64())

   end subroutine testAssertIsInfinite

   subroutine testAssertExceptionRaised()
      use PF_ExceptionList, only: throw
      use PF_SourceLocation

      character(len=*), parameter :: message = 'a message'

      call throw(message)
      call assertExceptionRaised(message)

      call throw(message)
      call assertExceptionRaised(message,SourceLocation('here',5))

   end subroutine testAssertExceptionRaised

   subroutine testAssertFail()
      use PF_SourceLocation

      character(len=*), parameter :: message = 'a message'

      call assertFail(message)
      call assertExceptionRaised(message)

      call assertFail(message)
      call assertExceptionRaised(message,SourceLocation('here',5))
   end subroutine testAssertFail

end module Test_AssertBasic
