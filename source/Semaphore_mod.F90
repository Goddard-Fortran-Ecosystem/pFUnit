!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: Semaphore_mod
!
!> @brief
!! Implements the semaphore for polling.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 16 Oct 2006
!!
! REVISION HISTORY:
! 16 Oct 2006 - Initial Version
! 12 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module Semaphore_mod
   use Params_mod, only: FUNIT_SUCCESS, FUNIT_FAILURE
   implicit none
   private

   public :: CreateSemaphore
   public :: DeleteSemaphore
   public :: SemaphoreExists
   public :: PollSemaphore

contains

   !---------------------------------------------------------------------------
   !> Polls the given semaphore with the given time 
   !!
   !! @param semaphore - given name of semampore in strng 
   !! @param t - given time for polling semampore
   !!
   !! @return status indicator whether it is successful or failure 
   !---------------------------------------------------------------------------
   integer function PollSemaphore(semaphore, t)
      character(len=*), intent(in) :: semaphore
      real,             intent(in) :: t

      integer :: c0, c1, cr

      call system_clock(c0, cr)

      do ! until success or time limit

         if (SemaphoreExists(semaphore)) then
            call DeleteSemaphore(semaphore)
            PollSemaphore = FUNIT_SUCCESS
            return
         end if

         call system_clock(c1)
         if ((c1 - c0) > cr * t) then
            PollSemaphore = FUNIT_FAILURE
            return
         end if
      end do
       
   end function PollSemaphore

   !---------------------------------------------------------------------------
   !> Returns if the given semaphore exists
   !!
   !! @param semaphore - given name of semampore in strng 
   !!
   !! @return .true. if semaphore exists.   Otherwise, .false.
   !---------------------------------------------------------------------------
   logical function SemaphoreExists(semaphore)
      use IO_Utilities_mod
      character(len=*), intent(in) :: semaphore
      integer :: stat
      integer :: unit

      inquire(file=trim(semaphore), exist = SemaphoreExists)

   end function SemaphoreExists

   !---------------------------------------------------------------------------
   !> Creates the constructor with given name of the semaphore in string
   !!
   !! @param semaphore - given name of semampore in strng 
   !---------------------------------------------------------------------------
   subroutine CreateSemaphore(semaphore)
      use IO_Utilities_mod
      use pFUnitException_mod
      character(len=*), intent(in) :: semaphore
      integer :: unit
      integer :: stat

      unit = OpenFile(semaphore, 'new', 'formatted')
      if (unit /= -1) close(unit)

   end subroutine CreateSemaphore
  
   !---------------------------------------------------------------------------
   !> Deletes the specific semaphore by the given name 
   !!
   !! @param semaphore - given name of semampore in strng 
   !---------------------------------------------------------------------------
   subroutine DeleteSemaphore(semaphore)
      use IO_Utilities_mod
      use pFUnitException_mod
      character(len=*), intent(in) :: semaphore

      call deleteFile(trim(semaphore))
    
   end subroutine DeleteSemaphore

end module Semaphore_mod
