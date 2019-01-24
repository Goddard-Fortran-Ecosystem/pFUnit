!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_SimpleTestCase
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
module Test_SimpleTestCase
   use PF_TestSuite, only: TestSuite
   implicit none
   private

   public :: suite

contains

!#define ADD(method) call suite%addTest(TestMethod(REFLECT(method)))
   function suite()
      use PF_TestMethod, only: TestMethod
      type (TestSuite) :: suite

      suite = TestSuite('Test_SimpleTestCase')

      call suite%addTest( &
           &   TestMethod('testRunSuite', &
           &                  testRunSuite))
      call suite%addTest( &
           &   TestMethod('testRunMethodShouldFail', &
           &                  testRunMethodShouldFail))

   end function suite

   function internalSuite()
      use PF_TestMethod, only: TestMethod
      type (TestSuite) :: internalSuite

      internalSuite = TestSuite('Test_TestCase')

      call internalSuite%addTest(TestMethod('testWorks',testWorks))
      call internalSuite%addTest(TestMethod('testFails',testFails))

   end function internalSuite

   subroutine testWorks()
      use PF_TestCase
      use PF_TestSuite
      use PF_TestResult, only: TestResult, TestResult
      use pf_SimpleTestCase, only: newSimpleTestCase, SimpleTestCase
      use pf_SimpleTestCase, only: method1, method2
      use PF_Assert, only: assertEqual
      use PF_SerialContext

      type (TestResult) :: aTestResult
      type (SimpleTestCase) :: aTest

      aTestResult = TestResult()
      aTest = newSimpleTestCase('method1', method1)
      call aTest%run(aTestResult, SerialContext())
      call assertEqual('run method1', aTest%runLog)

      aTest = newSimpleTestCase('method2', method2)
      call aTest%run(aTestResult, SerialContext())
      call assertEqual('run method2', aTest%runLog)

   end subroutine testWorks

   subroutine testFails()
      use PF_TestCase
      use PF_TestSuite
      use PF_TestResult, only: TestResult, TestResult
      use pf_SimpleTestCase, only: newSimpleTestCase, SimpleTestCase
      use pf_SimpleTestCase, only: method1
      use PF_Assert, only: assertEqual
      use PF_SerialContext

      type (TestResult) :: aTestResult
      type (SimpleTestCase) :: aTest

      aTestResult = TestResult()
      aTest = newSimpleTestCase('method1', method1)
      call aTest%run(aTestResult, SerialContext())
      call assertEqual('run method2', aTest%runLog)

   end subroutine testFails

   subroutine testRunSuite()
      use PF_TestSuite, only: TestSuite
      use PF_TestResult, only: TestResult, TestResult
      use PF_Assert, only: assertEqual
      use PF_SerialContext
      type (TestResult) :: aTestResult
      type (TestSuite) :: aSuite

      aSuite = internalSuite()
      aTestResult = TestResult()
      call aSuite%run(aTestResult, SerialContext())
      call assertEqual(2, aTestResult%runCount())
      call assertEqual(1, aTestResult%failureCount())

    end subroutine testRunSuite

    ! Previously TestCase deferred implementation of runMethod()
    ! New changes though require there to be a default implementation
    ! (to avoid user types being ABSTRACT), but it should never be used.
    ! This test ensures that the default throws an exception.
    subroutine testRunMethodShouldFail()
       use PF_TestCase
       use PF_Assert

       type, extends(TestCase) :: TempTestCase
       end type TempTestCase

       type (TempTestCase) :: testObject

       call testObject%runMethod()
       call assertExceptionRaised('TestCase::runMethod() must be overridden.')
    end subroutine testRunMethodShouldFail

end module Test_SimpleTestCase

