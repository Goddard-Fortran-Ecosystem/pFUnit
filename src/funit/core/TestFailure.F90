!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: TestFailure
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
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
module PF_TestFailure
   use PF_Exception
   use PF_ExceptionList
   implicit none
   private

   public :: TestFailure

   type TestFailure
      character(len=80) :: testName
      type (ExceptionList) :: exceptions
   end type TestFailure

end module PF_TestFailure
