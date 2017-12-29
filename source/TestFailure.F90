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
module PF_TestFailure_mod
   use PF_Exception_mod
   use PF_ExceptionList_mod
   implicit none
   private

   public :: TestFailure

   type TestFailure
      character(len=80) :: testName
      type (ExceptionList) :: exceptions
   end type TestFailure

!!$   interface TestFailure
!!$      module procedure newTestFailure
!!$   end interface TestFailure
!!$
!!$contains
!!$
!!$   function newTestFailure(testName, list)
!!$      character(len=*), intent(in) :: testName
!!$      type (Exception), intent(in) :: exceptions(:)
!!$   end function newTestFailure

end module PF_TestFailure_mod
