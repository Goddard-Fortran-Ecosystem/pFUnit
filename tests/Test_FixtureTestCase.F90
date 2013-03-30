#include "reflection.h"
module Test_FixtureTestCase_mod
   use TestSuite_mod
   use TestResult_mod, only: TestResult, newTestResult
   implicit none
   private

   public :: suite

contains

   function suite() result(aSuite)
      use TestSuite_mod, only: newTestSuite, TestSuite
      use TestMethod_mod, only: newTestMethod
      type (TestSuite) :: aSuite

      aSuite = newTestSuite('Test_TestCase')

#define ADD(method) call aSuite%addTest(newTestMethod(REFLECT(method)))

      ADD(testRunWithFixture)
      ADD(testBrokenTestCase)
      ADD(testBrokenSetUpCase)

   end function suite

   subroutine testRunWithFixture()
      use TestCase_mod
      use FixtureTestCase_mod, only: FixtureTestCase, newFixtureTestCase
      use FixtureTestCase_mod, only: delete
      use SerialContext_mod
      use Assert_mod, only: assertEqual
      type (FixtureTestCase) :: aTest
      type (TestResult), pointer :: aTestResult

      aTestResult => newTestResult()
      aTest = newFixtureTestCase()
      call aTest%setSurrogate()
      call aTest%run(aTestResult, newSerialContext())
      call assertEqual('setUp run tearDown', aTest%runLog)
      call delete(aTest)

   end subroutine testRunWithFixture

   subroutine testBrokenTestCase()
      use TestCase_mod
      use BrokenTestCase_mod, only: BrokenTestCase
      use Assert_mod, only: assertEqual
      use SerialContext_mod
      type (BrokenTestCase) :: test
      type (TestResult) :: aTestResult

      call test%setSurrogate()
      call test%setName('foo')
      aTestResult = newTestResult()

      call test%run(aTestResult, newSerialContext())
      call assertEqual('setUp broken run tearDown', test%runLog)
      call assertEqual(1, aTestResult%failureCount())

   end subroutine testBrokenTestCase

   subroutine testBrokenSetUpCase()
      use TestCase_mod
      use BrokenSetUpCase_mod, only: BrokenSetUpCase
      use Assert_mod, only: assertEqual
      use SerialContext_mod
      type (BrokenSetUpCase) :: test
      type (TestResult) :: aTestResult

      call test%setSurrogate()
      call test%setName('foo')
      aTestResult = newTestResult()
      call test%run(aTestResult, newSerialContext())
      call assertEqual('broken setUp', test%runLog)
      call assertEqual(1, aTestResult%failureCount())

   end subroutine testBrokenSetUpCase

end module Test_FixtureTestCase_mod

