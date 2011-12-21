!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE:  MpiServices
!
!> @brief
!! Implements the object for Message Passing Interface (MPI) services using 
!! MPI mechanism.
!!
!! @author
!! Tom Clune, NASA/GSFC SIVO
!!
!! @date
!! 17 Dec 2007
!!
! REVISION HISTORY:
! 17 Dec 2007 - Initial Version
! 15 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module MpiServices_mod
#ifndef USE_MPI
   use MpiStubs_mod
#endif
   implicit none
   private

   public :: amRoot
   public :: barrier
   public :: getRank
   public :: numProcesses
   public :: commWorld

   interface amRoot
      module procedure amRoot_comm
   end interface

   interface barrier
      module procedure barrier_comm
   end interface

   interface numProcesses
      module procedure MpiNumProcesses
   end interface

#ifdef USE_MPI
   include 'mpif.h'
#endif

contains

   !---------------------------------------------------------------------------
   !> Gets the rank by the given optional type of MPI communicator.
   !!
   !! @param comm - given optional type of communicator
   !!
   !! @return rank from MPI communicator
   !---------------------------------------------------------------------------
   integer function getRank(comm)
      integer, intent(in), optional :: comm

      integer :: ier
      integer :: comm_

      comm_ = MPI_COMM_WORLD
      if (present(comm)) comm_ = comm

      call mpi_comm_rank(comm_, getRank, ier)
      
   end function getRank

   !---------------------------------------------------------------------------
   !> Gets the number of processes by the given optional type of MPI 
   !! communicator.
   !!
   !! @param comm - given optional type of communicator
   !!
   !! @return the number of processes from MPI communicator
   !---------------------------------------------------------------------------
   integer function MpiNumProcesses(comm)
      integer, intent(in), optional :: comm

      integer :: ier
      integer :: comm_

      comm_ = MPI_COMM_WORLD
      if (present(comm)) comm_ = comm

      call mpi_comm_size(comm_, MpiNumProcesses, ier)
      
   end function MpiNumProcesses

   !---------------------------------------------------------------------------
   !> Checks if it the root communicator by the given optional type of MPI 
   !! communicator.
   !!
   !! @param mpiCommunicator - given optional type of communicator
   !!
   !! @return .true. if it is root communicator, .false. otherwise.
   !---------------------------------------------------------------------------
   logical function amRoot_comm(mpiCommunicator)
      integer, intent(in), optional :: mpiCommunicator
      integer, parameter :: ROOT = 0

      amRoot_comm = (getRank(mpiCommunicator) == ROOT)

   end function amRoot_comm

   !---------------------------------------------------------------------------
   !> Checks if it the root communicator by the given optional type of MPI 
   !! communicator.
   !!
   !! @param mpiCommunicator - given optional type of communicator
   !!
   !! @return .true. if it is root communicator, .false. otherwise.
   !---------------------------------------------------------------------------
   subroutine barrier_comm(mpiCommunicator)
      integer, optional, intent(in) :: mpiCommunicator
      
      integer :: comm_
      integer :: ier

#ifdef USE_MPI
      comm_ = MPI_COMM_WORLD
      if (present(mpiCommunicator)) comm_ = mpiCommunicator
      call mpi_barrier(comm_, ier)
#else
      ! NO-OP
#endif

   end subroutine barrier_comm

   !---------------------------------------------------------------------------
   !> Returns the communication indicator.
   !!
   !! @return MPI Communictor Indicator
   !!
   !! @note  trivial function but prevents conflict with user code that 
   !!        has "include 'mpif.h'"
   !! 
   !---------------------------------------------------------------------------
   integer function commWorld()
      commWorld = MPI_COMM_WORLD 
   end function commWorld

end module MpiServices_mod
