!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE:  MpiCommand
!
!> @brief
!! Implements the object for MPI (Message Passing Interface) command to handle
!! the command such as reading and writing the command.
!!
!! @author
!! Tom Clune, NASA/GSFC SIVO
!!
!! @date
!! 15 Dec 2007
!!
! REVISION HISTORY:
! 15 Dec 2007 - Initial Version
! 15 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module MpiCommand_mod
   implicit none
   private

   public :: MpiCommand_type
   public :: MpiCommand
   public :: writeCommand
   public :: readCommand
   public :: numProcs
   type MpiCommand_type
      private
      integer :: numProcs
   end type MpiCommand_type

   interface writeCommand
      module procedure writeCommand_
   end interface

   interface readCommand
      module procedure readCommand_
   end interface

contains

   !---------------------------------------------------------------------------
   !> Constructs the MPI command with the given number of processes.
   !!
   !! @param numProcesses - given number of processes
   !!
   !! @return new object of MPI command
   !---------------------------------------------------------------------------
   function MpiCommand(numProcs) result(command)
      integer, intent(in) :: numProcs
      type (MpiCommand_type) :: command

      command % numProcs = numProcs
   end function MpiCommand

   !---------------------------------------------------------------------------
   !> Writes the command witht the the given unit number. 
   !!
   !! @param this - this MPI command object
   !! @param unit - given unit number
   !---------------------------------------------------------------------------
   subroutine writeCommand_(this, unit)
      type (MpiCommand_type), intent(in) :: this
      integer, intent(in) :: unit

      write(unit,'(a,i8.0)')'numProcs: ',this % numProcs

   end subroutine writeCommand_

   !---------------------------------------------------------------------------
   !> Reads the command witht the the given unit number. 
   !!
   !! @param this - this MPI command object
   !! @param unit - given unit number
   !---------------------------------------------------------------------------
   subroutine readCommand_(this, unit)
      type (MpiCommand_type), intent(out) :: this
      integer, intent(in) :: unit

      read(unit,'("numProcs: ",i8.0)') this % numProcs

   end subroutine readCommand_

   !---------------------------------------------------------------------------
   !> Gets the number of processes from the MPI command object.
   !!
   !! @param this - this MPI command object
   !!
   !! @return number of processes
   !---------------------------------------------------------------------------
   integer function numProcs(this)
      type (MpiCommand_type), intent(in) :: this
      numProcs = this % numProcs
   end function numProcs

end module MpiCommand_mod
