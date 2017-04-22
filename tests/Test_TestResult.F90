!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_TestResult_mod
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
module Test_TestResult_mod
   use PF_TestSuite_mod, only: TestSuite, newTestSuite
   use PF_TestResult_mod, only: TestResult, newTestResult
   use PF_TestResult_mod, only: newTestResult, TestResult
   use PF_TestCase_mod
   use SimpleTestCase_mod, only: newSimpleTestCase, SimpleTestCase
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite_mod, only: TestSuite, newTestSuite
      use PF_TestResult_mod, only: TestResult, newTestResult
      use PF_TestCase_mod
      use PF_TestMethod_mod, only: newTestMethod
      type (TestSuite) :: suite

      suite = newTestSuite('TestResultSuite')

!#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))


      call suite%addTest( &
           &   newTestMethod('testGetNumRun', &
           &                  testGetNumRun))
      call suite%addTest( &
           &   newTestMethod('testGetNumFailed', &
           &                  testGetNumFailed))

      call suite%addTest( &
           &   newTestMethod('testAddListenerStart', &
           &                  testAddListenerStart))
      call suite%addTest( &
           &   newTestMethod('testAddListenerEnd', &
           &                  testAddListenerEnd))
      call suite%addTest( &
           &   newTestMethod('testAddListenerFailure', &
           &                  testAddListenerFailure))

   end function suite

   subroutine testGetNumRun()
      use PF_Assert_mod, only: assertEqual
      use PF_TestResult_mod, only: newTestResult, TestResult
      use PF_TestCase_mod
      use SimpleTestCase_mod
      type (TestResult) :: aResult
      type (SimpleTestCase) :: tstCase

      aResult = newTestResult()
      call assertEqual(0, aResult%runCount())

      tstCase = newSimpleTestCase('method1', method1)

      call aResult%startTest(tstCase%getSurrogate())
      call aResult%endTest(tstCase%getSurrogate())
      call assertEqual(1, aResult%runCount())

      call aResult%startTest(tstCase%getSurrogate())
      call aResult%endTest(tstCase%getSurrogate())
      call assertEqual(2, aResult%runCount())

   end subroutine testGetNumRun

   subroutine testGetNumFailed()
      use PF_Assert_mod, only: assertEqual
      use PF_Exception_mod, only: Exception
      use SimpleTestCase_mod, only: SimpleTestCase
      use PF_SurrogateTestCase_mod
      use PF_TestCase_mod

      type (TestResult) :: aResult
      
      type (SimpleTestCase) :: aTest
      call aTest%setSurrogate()
      aResult = newTestResult()
      call assertEqual(0, aResult%failureCount())

      call aResult%addFailure(aTest%getSurrogate(), [Exception('fail')])
      call assertEqual(1, aResult%failureCount())

      call aResult%addFailure(aTest%getSurrogate(), [Exception('fail again')])
      call assertEqual(2, aResult%failureCount())

   end subroutine testGetNumFailed

   subroutine testAddListenerEnd()
      use PF_TestListener_mod
      use MockListener_mod
      use PF_Assert_mod
      use SimpleTestCase_mod
      use PF_SurrogateTestCase_mod
      use PF_TestCase_mod

      type (TestResult) :: result
      type (MockListener), target :: listener
      type (SimpleTestCase) :: tstCase
      
      result = newTestResult()
      call result%addListener(listener)
      tstCase = newSimpleTestCase('method1', method1)
      call result%endTest(tstCase%getSurrogate())
      call assertEqual('endTest() was called', listener%log)

   end subroutine testAddListenerEnd

   subroutine testAddListenerStart()
      use PF_TestListener_mod
      use PF_SurrogateTestCase_mod
      use MockListener_mod
      use PF_Assert_mod
      use SimpleTestCase_mod
      type (TestResult) :: result
      type (MockListener), target :: listener
      
      type (SimpleTestCase) :: tstCase

      result = newTestResult()
      call result%addListener(listener)
      tstCase = newSimpleTestCase('method1', method1)
      call result%startTest(tstCase%getSurrogate())
      call assertEqual('startTest() was called', trim(listener%log))

   end subroutine testAddListenerStart

   subroutine testAddListenerFailure()
      use PF_TestListener_mod
      use MockListener_mod
      use PF_Assert_mod
      use PF_Exception_mod
      use SimpleTestCase_mod
      use PF_SurrogateTestCase_mod
      use PF_TestCase_mod
      
      type (TestResult) :: result
      type (MockListener), target :: listener
      type (Exception) :: anException
      
      class(TestCase), allocatable :: tstCase
      
      result = newTestResult()
      call result%addListener(listener)
      allocate(tstCase, source = newSimpleTestCase('method1', method1))
      call result%addFailure(tstCase%getSurrogate(), [anException])
      call assertEqual('addFailure() was called', listener%log)

   end subroutine testAddListenerFailure

end module Test_TestResult_mod
