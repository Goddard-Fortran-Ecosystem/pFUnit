#include "reflection.h"
module Test_SimpleTestCase_mod
   use TestSuite_mod, only: TestSuite, newTestSuite
   implicit none
   private

   public :: suite

contains

#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))
   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use TestMethod_mod, only: newTestMethod
      type (TestSuite), pointer :: suite

      suite => newTestSuite('Test_TestCase')

      ADD(testRunSuite)

   end function suite

   function internalSuite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use TestMethod_mod, only: newTestMethod
      type (TestSuite), pointer :: internalSuite

      internalSuite => newTestSuite('Test_TestCase')

      call internalSuite%addTest(newTestMethod(REFLECT(testWorks)))
      call internalSuite%addTest(newTestMethod(REFLECT(testFails)))

   end function internalSuite

   subroutine testWorks()
      use TestCase_mod
      use TestSuite_mod
      use TestResult_mod, only: TestResult, newTestResult
      use SimpleTestCase_mod, only: newSimpleTestCase, SimpleTestCase
      use SimpleTestCase_mod, only: method1, method2
      use Assert_mod, only: assertEqual
      use SerialContext_mod

      type (TestResult), pointer :: aTestResult
      type (SimpleTestCase), pointer :: aTest

      aTestResult => newTestResult()
      aTest => newSimpleTestCase('method1', method1)
      call aTest%run(aTestResult, newSerialContext())
      call assertEqual('run method1', aTest%runLog)
      deallocate(aTest)

      aTest => newSimpleTestCase('method2', method2)
      call aTest%run(aTestResult, newSerialContext())
      call assertEqual('run method2', aTest%runLog)
      deallocate(aTest)

   end subroutine testWorks

   subroutine testFails()
      use TestCase_mod
      use TestSuite_mod
      use TestResult_mod, only: TestResult, newTestResult
      use SimpleTestCase_mod, only: newSimpleTestCase, SimpleTestCase
      use SimpleTestCase_mod, only: method1
      use Assert_mod, only: assertEqual
      use SerialContext_mod

      type (TestResult), pointer :: aTestResult
      type (SimpleTestCase), pointer :: aTest

      aTestResult => newTestResult()
      aTest => newSimpleTestCase('method1', method1)
      call aTest%run(aTestResult, newSerialContext())
      call assertEqual('run method2', aTest%runLog)
      deallocate(aTest)

   end subroutine testFails

   subroutine testRunSuite()
      use TestSuite_mod, only: TestSuite
      use TestResult_mod, only: TestResult, newTestResult
      use Assert_mod, only: assertEqual
      use SerialContext_mod
      type (TestResult), pointer :: aTestResult
      type (TestSuite), pointer :: aSuite

      aSuite => internalSuite()
      aTestResult => newTestResult()
      call aSuite%run(aTestResult, newSerialContext())
      call assertEqual(2, aTestResult%runCount())
      call assertEqual(1, aTestResult%failureCount())
      deallocate(aSuite)

    end subroutine testRunSuite

end module Test_SimpleTestCase_mod

