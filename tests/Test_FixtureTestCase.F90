#include 'reflection.h'
module Test_FixtureTestCase_mod
   use TestSuite_mod, only: TestSuite, newTestSuite
   use SimpleTestMethod_mod, only: newSimpleTestMethod, SimpleTestMethod
   use TestResult_mod, only: TestResult, newTestResult
   implicit none
   private

   public :: suite

contains

   function suite()
      type (TestSuite), pointer :: suite

      allocate(suite)
      suite = newTestSuite('Test_TestCase')

#define ADD(method) call suite%addTest(newSimpleTestMethod(REFLECT(method)))

      ADD(testRunWithFixture)
      ADD(testBrokenTestCase)
      ADD(testBrokenSetUpCase)

   end function suite

   subroutine testRunWithFixture()
      use FixtureTestCase_mod, only: FixtureTestCase, newFixtureTestCase
      use FixtureTestCase_mod, only: SimpleTestMethod, delete
      use Assert_mod, only: assertEqual
      use SerialContext_mod
      type (FixtureTestCase) :: test
      type (TestResult) :: aTestResult

      aTestResult = newTestResult()
      test = newFixtureTestCase()
      call test%run(aTestResult, newSerialContext())
      call assertEqual('setUp run tearDown', test%runLog)
      call delete(test)

   end subroutine testRunWithFixture

   subroutine testBrokenTestCase()
      use BrokenTestCase_mod, only: BrokenTestCase
      use Assert_mod, only: assertEqual
      use SerialContext_mod
      type (BrokenTestCase) :: test
      type (TestResult) :: aTestResult

      aTestResult = newTestResult()
      call test%run(aTestResult, newSerialContext())
      call assertEqual('setUp broken run tearDown', test%runLog)
      call assertEqual(1, aTestResult%failureCount())

   end subroutine testBrokenTestCase

   subroutine testBrokenSetUpCase()
      use BrokenSetUpCase_mod, only: BrokenSetUpCase
      use Assert_mod, only: assertEqual
      use SerialContext_mod
      type (BrokenSetUpCase) :: test
      type (TestResult) :: aTestResult

      aTestResult = newTestResult()
      call test%run(aTestResult, newSerialContext())
      call assertEqual('broken setUp', test%runLog)
      call assertEqual(1, aTestResult%failureCount())

   end subroutine testBrokenSetUpCase

end module Test_FixtureTestCase_mod

