!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: robustTestSuite
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
module robustTestSuite
   use FUnit
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite, only: TestSuite
      use PF_TestMethod, only: TestMethod
      type (TestSuite) :: suite

      class (Test), allocatable :: t
      type(TimeoutAnnotation) :: timeout

      suite = TestSuite('robustTestSuite')

      call suite%addTest( TestMethod('testRunAssertFailure', testRunAssertFailure))
      call suite%addTest( TestMethod('testRunSucceeds', testRunSucceeds))

      t = TestMethod('testRunHangs', testRunHangs)
      timeout = TimeoutAnnotation(0.1)
      call t%insert(timeout%type_name(), timeout)
      call suite%addTest( t )

      call suite%addTest( TestMethod('testRunStops', testRunStops) )
      call suite%addTest( TestMethod('testRunMultipleExceptions', testRunMultipleExceptions))

   end function suite

   subroutine testRunSucceeds()
     ! do nothing
   end subroutine testRunSucceeds

   subroutine testRunMultipleExceptions()
     use PF_Assert
     ! do nothing
     call assertTrue(1 == 2,'messageA')
     call assertTrue(1 == 3,'message' // new_line('a') // 'B')
     call assertTrue(1 == 4,'C')
   end subroutine testRunMultipleExceptions

   subroutine testRunAssertFailure()
      use PF_Assert
      ! do nothing
      call assertTrue(1 == 2,'some message')
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

end module robustTestSuite
