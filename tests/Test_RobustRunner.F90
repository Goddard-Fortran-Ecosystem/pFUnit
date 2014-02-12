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
      use AbstractPrinter_mod
      use ResultPrinter_mod
      use BaseTestRunner_mod, only: RETURN_FAILURE

      type (RobustRunner) :: runner
      type (SerialContext) :: context
      type (TestSuite) :: suite
      type (TestResult) :: result
      type (PrinterPointer) :: printers(1)
      integer :: returnCode

      integer :: unit

      open(newunit=unit, access='sequential',form='formatted',status='scratch')
      allocate(printers(1)%pPrinter, source=newResultPrinter(unit))
      runner = RobustRunner('./tests/remote.x', printers)
      result = newTestResult()
      suite = remoteSuite()
      call runner%runWithResult(suite, context, result, returnCode)

      call assertEqual(4, result%runCount(),'runCount()')
      call assertEqual(1, result%failureCount(), 'failureCount()')
      call assertEqual(2, result%errorCount(), 'errorCount()')
      call assertEqual(RETURN_FAILURE, returnCode)

      close(unit)
      
   end subroutine testRunVariety

end module Test_RobustRunner_mod
