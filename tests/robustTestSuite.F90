#include "reflection.h"
module robustTestSuite_mod
   use pFUnit_mod
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

      ADD(testRunSucceeds)
      ADD(testRunAssertFailure)
      ADD(testRunStops)
      ADD(testRunHangs)

   end function suite

   subroutine testRunSucceeds()
      ! do nothing
   end subroutine testRunSucceeds

   subroutine testRunAssertFailure()
      use Assert_mod
      ! do nothing
      call assertTrue(1 == 2)
   end subroutine testRunAssertFailure

   subroutine testRunStops()
      call runStops()
   end subroutine testRunStops

   ! This will stop the framework cold.  The robust runner
   ! should detect this - reporting it as an Error.
   subroutine runStops()
      stop
   end subroutine runStops

   ! This test will hang.  The robust runner
   ! should detect this - reporting it as a hung process.
   subroutine testRunHangs()
      do
      end do
   end subroutine testRunHangs

end module robustTestSuite_mod
