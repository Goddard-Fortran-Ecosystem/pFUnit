#include "reflection.h"
module Test_TestResult_mod
   use TestResult_mod, only: TestResult, newTestResult
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use SimpleTestMethod_mod, only: newSimpleTestMethod, SimpleTestMethod
      type (TestSuite), pointer :: suite

      allocate(suite)
      suite = newTestSuite('Exception')

#define ADD(method) call suite%addTest(newSimpleTestMethod(REFLECT(method)))


      ADD(testGetNumRun)
      ADD(testGetNumFailed)

      ADD(testAddListenerStart)
      ADD(testAddListenerEnd)
      ADD(testAddListenerFailure)

   end function suite

   subroutine testGetNumRun()
      use Assert_mod, only: assertEqual
      use SimpleTestCase_mod
      type (TestResult) :: aResult

      aResult = newTestResult()
      call assertEqual(0, aResult%runCount())

      call aResult%startTest(newSimpleTestCase(method1,'method1'))
      call aResult%endTest(newSimpleTestCase(method1,'method1'))
      call assertEqual(1, aResult%runCount())

      call aResult%startTest(newSimpleTestCase(method1,'method1'))
      call aResult%endTest(newSimpleTestCase(method1,'method1'))
      call assertEqual(2, aResult%runCount())

   end subroutine testGetNumRun

   subroutine testGetNumFailed()
      use Assert_mod, only: assertEqual
      use Exception_mod, only: Exception, newException
      use SimpleTestCase_mod, only: SimpleTestCase
      type (TestResult) :: aResult
      
      type (SimpleTestCase) :: aTest

      aResult = newTestResult()
      call assertEqual(0, aResult%failureCount())

      call aResult%addFailure(aTest, newException('fail'))
      call assertEqual(1, aResult%failureCount())

      call aResult%addFailure(aTest, newException('fail again'))
      call assertEqual(2, aResult%failureCount())

   end subroutine testGetNumFailed

   subroutine testAddListenerEnd()
      use TestListener_mod
      use MockListener_mod
      use Assert_mod
      use SimpleTestCase_mod
      type (TestResult) :: result
      type (MockListener) :: listener
      
      result = newTestResult()
      call result%addListener(listener)
      call result%endTest(newSimpleTestCase(method1,'method1'))
      call assertEqual('endTest() was called', listener%log)
   end subroutine testAddListenerEnd

   subroutine testAddListenerStart()
      use TestListener_mod
      use MockListener_mod
      use Assert_mod
      use SimpleTestCase_mod
      type (TestResult) :: result
      type (MockListener) :: listener
      
      result = newTestResult()
      call result%addListener(listener)
      call result%startTest(newSimpleTestCase(method1,'method1'))
      call assertEqual('startTest() was called', trim(listener%log))
   end subroutine testAddListenerStart

   subroutine testAddListenerFailure()
      use TestListener_mod
      use MockListener_mod
      use Assert_mod
      use Exception_mod
      use SimpleTestCase_mod
      
      type (TestResult) :: result
      type (MockListener) :: listener
      type (Exception) :: anException
      
      result = newTestResult()
      call result%addListener(listener)
      call result%addFailure(newSimpleTestCase(method1,'method1'), anException)
      call assertEqual('addFailure() was called', listener%log)
   end subroutine testAddListenerFailure

end module Test_TestResult_mod
