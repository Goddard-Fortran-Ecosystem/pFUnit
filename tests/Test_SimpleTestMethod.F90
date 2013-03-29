module Test_SimpleTestMethod_mod
   use TestSuite_mod, only: TestSuite, newTestSuite
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use SimpleTestMethod_mod, only: newSimpleTestMethod
      type (TestSuite), pointer :: suite

      suite => newTestSuite('Test_SimpleTestMethod')
      call suite%addTest(newSimpleTestMethod(testMethodWasRun, 'testMethodWasRun'))

   end function suite

   subroutine testMethodWasRun()
      use TestCase_mod
      use TestResult_mod, only: TestResult, newTestResult
      use SimpleTestMethod_mod, only: SimpleTestMethod, newSimpleTestMethod
      use Assert_mod, only: assertEqual
      use SerialContext_mod
      type (SimpleTestMethod) :: method
      type (TestResult) :: aResult

      method = newSimpleTestMethod(testMethod = testWasRun, name = 'testWasRun')
      aResult = newTestResult()
      call method%run(aResult, newSerialContext())
      call assertEqual(1, aResult%runCount())
      call assertEqual(1, aResult%failureCount())

   end subroutine testMethodWasRun

   subroutine testWasRun()
      use Exception_mod, only: throw
      call throw('wasRun')
   end subroutine testWasRun

end module Test_SimpleTestMethod_mod

