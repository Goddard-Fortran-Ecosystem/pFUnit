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
   contains
      procedure(run), deferred :: run
   end type BaseTestRunner

   abstract interface
      subroutine run(this, aTest, context, returnCode)
         use Test_mod
         use ParallelContext_mod
         import BaseTestRunner
         
         class (BaseTestRunner), intent(inout) :: this
         class (Test), intent(inout) :: aTest
         class (ParallelContext), intent(in) :: context
         integer, intent(out) :: returnCode
      end subroutine run

!!$      subroutine startTest(this, testName)
!!$         import BaseTestRunner
!!$         class (BaseTestRunner), intent(inout) :: this
!!$         character(len=*), intent(in) :: testName
!!$      end subroutine startTest
!!$
!!$      subroutine endTest(this, testName)
!!$         import BaseTestRunner
!!$         class (BaseTestRunner), intent(inout) :: this
!!$         character(len=*), intent(in) :: testName
!!$      end subroutine endTest
!!$
!!$      subroutine addFailure(this, testName, exceptions)
!!$         import BaseTestRunner
!!$         use Exception_mod
!!$         class (BaseTestRunner), intent(inout) :: this
!!$         character(len=*), intent(in) :: testName
!!$         type (Exception), intent(in) :: exceptions(:)
!!$      end subroutine addFailure
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
