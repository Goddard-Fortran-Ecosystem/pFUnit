!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE:  MpiStubs
!
!> @brief
!! Implements the object for Message Passing Interface (MPI) stubs for
!! non-MPI default values.
!!
!! @author
!! Joey Gurganus, NASA/GSFC SIVO
!!
!! @date
!! 15 Jul 2010
!!
!! @note 
!! This module has been pulled off from the original file 'MpiService_mod.F90'.
!!
! REVISION HISTORY:
! 15 Jul 2010 - Initial Version
!-------------------------------------------------------------------------------
module MpiStubs_mod
   implicit none
   private

   public :: MPI_COMM_WORLD
   public :: MPI_SUCCESS
   public :: MPI_STATUS_SIZE
   public :: Mpi_Comm_rank
   public :: Mpi_Comm_size

   integer :: MPI_COMM_WORLD = 0
   integer :: MPI_SUCCESS = 0
   integer :: MPI_STATUS_SIZE = 1

contains
   
   !---------------------------------------------------------------------------
   !> Gets the rank and error indicator by the given type of MPI communicator.
   !!
   !! @param comm - given type of communicator
   !! @param rank - output of the rank
   !! @param ier - output of the error indicator
   !---------------------------------------------------------------------------
   subroutine Mpi_Comm_rank(comm, rank, ier)
      integer, intent(in) :: comm
      integer, intent(out) :: rank
      integer, intent(out) :: ier

      rank = 0
      ier = MPI_SUCCESS

   end subroutine Mpi_Comm_rank

   !---------------------------------------------------------------------------
   !> Gets the size of the given type of MPI communicator.
   !!
   !! @param comm - given type of communicator
   !! @param npes - output for the number ofprocesses
   !! @param ier - output for the error indicator
   !---------------------------------------------------------------------------
   subroutine Mpi_Comm_size(comm, npes, ier)
      integer, intent(in) :: comm
      integer, intent(out) :: npes
      integer, intent(out) :: ier

      npes = 1
      ier = MPI_SUCCESS

   end subroutine Mpi_Comm_size
   
end module MpiStubs_mod
