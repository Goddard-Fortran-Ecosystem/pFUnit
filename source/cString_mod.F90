!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: AssertString
!
!> @brief
!! Supports for interfacing with shared object libraries.  This significantly 
!! improves the user interface and obviates the need to create wrappers for each
!! test.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 12 Mar 2007
!!
! REVISION HISTORY:
! 12 Mar 2007 - Initial Version
! 12 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module cString_mod

  implicit none
  private


  ! methods
  public :: cString
  public :: NULL

  character (len=1), parameter :: NULL = achar (0)

contains

   !---------------------------------------------------------------------------
   !> Returns terminated string appending null at the end
   !!
   !! @param - given unterminated string
   !!
   !! @return terminated string with the null at the end of string
   !---------------------------------------------------------------------------
   function cString (unTerminatedString) result (terminatedString)
      ! arguments
      character (len=*), intent(in) :: unterminatedString
    
      ! local variables
      character (len=len(unterminatedString)+1) :: terminatedString

      terminatedString = trim (unterminatedString) // NULL

   end function cString


 end module cString_mod
