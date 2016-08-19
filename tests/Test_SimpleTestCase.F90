!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_SimpleTestCase_mod
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 21 Mar 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 21 Mar 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module Test_SimpleTestCase_mod
   use PF_TestSuite_mod, only: TestSuite, newTestSuite
   implicit none
   private

   public :: suite

contains

!#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))
   function suite()
      use PF_TestMethod_mod, only: newTestMethod
      type (TestSuite) :: suite

      suite = newTestSuite('Test_SimpleTestCase')

      call suite%addTest( &
           &   newTestMethod('testRunSuite', &
           &                  testRunSuite))
      call suite%addTest( &
           &   newTestMethod('testRunMethodShouldFail', &
           &                  testRunMethodShouldFail))

   end function suite

   function internalSuite()
      use PF_TestMethod_mod, only: newTestMethod
      type (TestSuite) :: internalSuite

      internalSuite = newTestSuite('Test_TestCase')

      call internalSuite%addTest(newTestMethod('testWorks',testWorks))
      call internalSuite%addTest(newTestMethod('testFails',testFails))

   end function internalSuite

   subroutine testWorks()
      use PF_TestCase_mod
      use PF_TestSuite_mod
      use PF_TestResult_mod, only: TestResult, newTestResult
      use SimpleTestCase_mod, only: newSimpleTestCase, SimpleTestCase
      use SimpleTestCase_mod, only: method1, method2
      use PF_Assert_mod, only: assertEqual
      use PF_SerialContext_mod

      type (TestResult) :: aTestResult
      type (SimpleTestCase) :: aTest

      aTestResult = newTestResult()
      aTest = newSimpleTestCase('method1', method1)
      call aTest%run(aTestResult, newSerialContext())
      call assertEqual('run method1', aTest%runLog)

      aTest = newSimpleTestCase('method2', method2)
      call aTest%run(aTestResult, newSerialContext())
      call assertEqual('run method2', aTest%runLog)

   end subroutine testWorks

   subroutine testFails()
      use PF_TestCase_mod
      use PF_TestSuite_mod
      use PF_TestResult_mod, only: TestResult, newTestResult
      use SimpleTestCase_mod, only: newSimpleTestCase, SimpleTestCase
      use SimpleTestCase_mod, only: method1
      use PF_Assert_mod, only: assertEqual
      use PF_SerialContext_mod

      type (TestResult) :: aTestResult
      type (SimpleTestCase) :: aTest

      aTestResult = newTestResult()
      aTest = newSimpleTestCase('method1', method1)
      call aTest%run(aTestResult, newSerialContext())
      call assertEqual('run method2', aTest%runLog)

   end subroutine testFails

   subroutine testRunSuite()
      use PF_TestSuite_mod, only: TestSuite
      use PF_TestResult_mod, only: TestResult, newTestResult
      use PF_Assert_mod, only: assertEqual
      use PF_SerialContext_mod
      type (TestResult) :: aTestResult
      type (TestSuite) :: aSuite

      aSuite = internalSuite()
      aTestResult = newTestResult()
      call aSuite%run(aTestResult, newSerialContext())
      call assertEqual(2, aTestResult%runCount())
      call assertEqual(1, aTestResult%failureCount())

    end subroutine testRunSuite

    ! Previously TestCase deferred implementation of runMethod()
    ! New changes though require there to be a default implementation
    ! (to avoid user types being ABSTRACT), but it should never be used.
    ! This test ensures that the default throws an exception.
    subroutine testRunMethodShouldFail()
       use PF_TestCase_mod
       use PF_Assert_mod

       type, extends(TestCase) :: TempTestCase
       end type TempTestCase

       type (TempTestCase) :: testObject

       call testObject%runMethod()
       call assertExceptionRaised('TestCase::runMethod() must be overridden.')
    end subroutine testRunMethodShouldFail

end module Test_SimpleTestCase_mod

