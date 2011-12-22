#include 'reflection.h'
module Test_ExampleTestCase_mod
   use TestSuite_mod, only: TestSuite, newTestSuite
   use SimpleTestMethod_mod, only: newSimpleTestMethod, SimpleTestMethod
   implicit none
   private

   public :: suite

contains

#define ADD(method) call suite%addTest(newSimpleTestMethod(REFLECT(method)))
   function suite()
      type (TestSuite), pointer :: suite

      allocate(suite)
      suite = newTestSuite('Test_TestCase')

      ADD(testRunSuite)

   end function suite

   function internalSuite()
      type (TestSuite), pointer :: internalSuite

      allocate(internalSuite)
      internalSuite = newTestSuite('Test_TestCase')

      call internalSuite%addTest(newSimpleTestMethod(REFLECT(testWorks)))
      call internalSuite%addTest(newSimpleTestMethod(REFLECT(testFails)))

   end function internalSuite

   subroutine testWorks()
      use TestCase_mod
      use TestSuite_mod
      use TestResult_mod, only: TestResult
      use ExampleTestCase_mod, only: newExampleTestCase, ExampleTestCase
      use ExampleTestCase_mod, only: method1, method2
      use Assert_mod, only: assertEqual
      use SerialContext_mod

      type (TestResult) :: aTestResult
      type (TestSuite), pointer :: aSuite
      type (ExampleTestCase), pointer :: aTest

      aTest => newExampleTestCase(method1, 'method1')
      call aTest%run(aTestResult, newSerialContext())
      call assertEqual('run method1', aTest%runLog)
      deallocate(aTest)

      aTest => newExampleTestCase(method2, 'method2')
      call aTest%run(aTestResult, newSerialContext())
      call assertEqual('run method2', aTest%runLog)
      deallocate(aTest)

   end subroutine testWorks

   subroutine testFails()
      use TestCase_mod
      use TestSuite_mod
      use TestResult_mod, only: TestResult
      use ExampleTestCase_mod, only: newExampleTestCase, ExampleTestCase
      use ExampleTestCase_mod, only: method1, method2
      use Assert_mod, only: assertEqual
      use SerialContext_mod

      type (TestResult) :: aTestResult
      type (TestSuite), pointer :: aSuite
      type (ExampleTestCase), pointer :: aTest

      aTest => newExampleTestCase(method1, 'method1')
      call aTest%run(aTestResult, newSerialContext())
      call assertEqual('run method2', aTest%runLog)
      deallocate(aTest)

   end subroutine testFails

   subroutine testRunSuite()
      use Test_mod, only: Test
      use TestCase_mod, only: TestCase
      use TestSuite_mod, only: TestSuite
      use TestResult_mod, only: TestResult, newTestResult
      use ExampleTestCase_mod, only: suite
      use Assert_mod, only: assertEqual
      use SerialContext_mod
      type (TestResult) :: aTestResult
      type (TestSuite), pointer :: aSuite

      aSuite => internalSuite()
      aTestResult = newTestResult()
      call aSuite%run(aTestResult, newSerialContext())
      call assertEqual(2, aTestResult%runCount())
      call assertEqual(1, aTestResult%failureCount())
      deallocate(aSuite)

    end subroutine testRunSuite

end module Test_ExampleTestCase_mod

