!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: BaseTestRunner
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module BaseTestRunner_mod
   use TestListener_mod
   implicit none
   private

   integer, parameter :: RETURN_OK = 0, RETURN_FAILURE = 1

   public :: BaseTestRunner, getReturnCode, RETURN_OK, RETURN_FAILURE

   type, abstract, extends(TestListener) :: BaseTestRunner
      private

   contains
      procedure(run2), deferred :: run
   end type BaseTestRunner

   abstract interface

      ! TODO - report bug to NAG.  If this is named "run" then
      ! RubustRunner fails to compile with message about conflicting types

      function run2(this, aTest, context) result(result)
         use Test_mod
         use ParallelContext_mod
         use TestResult_mod
         import BaseTestRunner

         type (TestResult) :: result
         class (BaseTestRunner), intent(inout) :: this
         class (Test), intent(inout) :: aTest
         class (ParallelContext), intent(in) :: context
      end function run2

   end interface

contains

   integer function getReturnCode(aTestResult) result(returnCode)
      use TestResult_mod
      type (TestResult), intent(in) :: aTestResult
      if(aTestResult%wasSuccessful()) then
         returnCode = RETURN_OK
      else
         returnCode = RETURN_FAILURE
      end if
   end function getReturnCode

end module BaseTestRunner_mod
