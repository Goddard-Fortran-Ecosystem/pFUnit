#include "reflection.h"
module Test_RobustRunner_mod
   use Test_mod
   use RobustRunner_mod
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use TestMethod_mod, only: newTestMethod
      type (TestSuite) :: suite

      suite = newTestSuite('StringConversionUtilities')
#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))

      ADD(testRunVariety)

   end function suite

   subroutine testRunVariety()
      use iso_fortran_env
      use robustTestSuite_mod, only: remoteSuite => suite
      use SerialContext_mod, only: SerialContext
      use TestSuite_mod
      use TestResult_mod
      use Assert_mod

      type (RobustRunner) :: runner
      type (SerialContext) :: context
      type (TestSuite) :: suite
      type (TestResult) :: result

      integer :: unit

      open(newunit=unit, access='sequential',form='formatted',status='scratch')
      runner = RobustRunner('./tests/remote.x',unit)
      result = newTestResult()
      suite = remoteSuite()
      call runner%runWithResult(suite, context, result)

      call assertEqual(4, result%runCount(),'runCount()')
      call assertEqual(1, result%failureCount(), 'failureCount()')
      call assertEqual(2, result%errorCount(), 'errorCount()')

      close(unit)
      
   end subroutine testRunVariety

end module Test_RobustRunner_mod
