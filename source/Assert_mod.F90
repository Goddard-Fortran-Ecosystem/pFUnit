!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: Assert_mod
!
!> @brief
!! A set of assertion methods useful for writing tests
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 03 Feb 2008
!!
! REVISION HISTORY:
! 03 Feb 2008 - Initial Version
! 09 Jul 2010 - Added prologue for Doxygen 
!-------------------------------------------------------------------------------
module Assert_mod
   use pFUnitException_mod
   use AssertString_mod
   use AssertInteger_mod
   use AssertReal_mod
   use AssertComplex_mod
   use Params_mod, Only: MAX_LEN_MSG
   implicit none
   private

   ! Public interfaces
   Public :: AssertTrue
   Public :: AssertFalse
   Public :: AssertEqual
   Public :: AssertFailedAssert

Contains

   !---------------------------------------------------------------------------
   !> Asserts that a test is true.
   !!
   !! @param test - test to be checked
   !! @param message - message to be sent for information  (optional)
   !!
   !---------------------------------------------------------------------------
   Subroutine AssertTrue(test, message)
      Logical,                       Intent(In) :: test
      Character(Len=*),    Optional, Intent(In) :: message

      Character(Len=MAX_LEN_MSG) :: details
      Character(Len=*), Parameter :: fmt_logical_assertion='(a)'

      If (.not. test) Then
         If (Present(message)) Then
             Write(details,fmt_logical_assertion) message
         Else
             details = ' '
         End If
         Call throw(Exception(details))
      End If

   End Subroutine AssertTrue

   !---------------------------------------------------------------------------
   !> Asserts that a test is false.
   !!
   !! @param test -     test to be checked
   !! @param message - message to be sent for information (optional)
   !!
   !! @return .true. only if the two BaseAddresses are the same.
   !---------------------------------------------------------------------------
   subroutine AssertFalse(test, message)
      Logical,                       Intent(In) :: test
      Character(Len=*),    Optional, Intent(In) :: message

      Call AssertTrue(.not. test, message)

   end subroutine AssertFalse
end module Assert_mod
