!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: robustTestSuite_mod
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
module robustTestSuite_mod
   use FUnit
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite_mod, only: TestSuite
      use PF_TestMethod_mod, only: TestMethod
      type (TestSuite) :: suite

      suite = TestSuite('robustTestSuite')
!#define ADD(method) call suite%addTest(TestMethod(REFLECT(method)))

      call suite%addTest( &
           &   TestMethod('testRunSucceeds', &
           &                  testRunSucceeds))
      call suite%addTest( &
           &   TestMethod('testRunMultipleExceptions', &
           &                  testRunMultipleExceptions))
      call suite%addTest( &
           &   TestMethod('testRunAssertFailure', &
           &                  testRunAssertFailure))
      call suite%addTest( &
           &   TestMethod('testRunStops', &
           &                  testRunStops))
      call suite%addTest( &
           &   TestMethod('testRunHangs', &
           &                  testRunHangs))

   end function suite

   subroutine testRunSucceeds()
     ! do nothing
   end subroutine testRunSucceeds

   subroutine testRunMultipleExceptions()
     use PF_Assert_mod
     ! do nothing
     call assertTrue(1 == 2)
     call assertTrue(1 == 3)
     call assertTrue(1 == 4)
   end subroutine testRunMultipleExceptions

   subroutine testRunAssertFailure()
      use PF_Assert_mod
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
