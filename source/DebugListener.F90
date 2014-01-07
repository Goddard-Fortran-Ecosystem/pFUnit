!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: DebugListener
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
module DebugListener_mod
   use TestListener_mod
   implicit none
   private

   public :: DebugListener

   type, extends(TestListener) :: DebugListener
      integer :: unit
   contains
      procedure :: addFailure
      procedure :: startTest
      procedure :: endTest
   end type DebugListener

   interface DebugListener
      module procedure newDebugListener_unit
      module procedure newDebugListener_default
   end interface DebugListener

contains

   function newDebugListener_unit(unit) result(listener)
      type (DebugListener) :: listener
      integer, intent(in) :: unit

      listener%unit = unit
   end function newDebugListener_unit

   function newDebugListener_default() result(listener)
      use iso_fortran_env, only: OUTPUT_UNIT
      type (DebugListener) :: listener

      listener = DebugListener(OUTPUT_UNIT)
   end function newDebugListener_default

   subroutine addFailure(this, testName, exceptions)
     use Exception_mod
     class (DebugListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (Exception), intent(in) :: exceptions(:)

     write(this%unit,*)'Failure in <',trim(testName),'>'
     flush(this%unit)

  end subroutine addFailure

  subroutine startTest(this, testName)
     class (DebugListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(this%unit,*)new_line('A')
     write(this%unit,*)'Start: <',trim(testName),'>'
     flush(this%unit)
   end subroutine startTest

  subroutine endTest(this, testName)
     class (DebugListener), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(this%unit,*)'  end: <',trim(testName),'>'
     flush(this%unit)

   end subroutine endTest

end module DebugListener_mod
