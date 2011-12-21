module Halo_mod
   implicit none
   private

   public :: fill

   include 'mpif.h'

contains

   subroutine fill(array, comm)
      real, intent(inout) :: array(:,0:)
      integer, intent(in) :: comm

      integer :: rank, npes, ier
      integer :: pe_south, pe_north
      integer :: nx, ny
      integer :: tag_a = 10, tag_b = 11

      integer :: status(MPI_STATUS_SIZE)

      call mpi_comm_rank(comm, rank, ier)
      call mpi_comm_size(comm, npes, ier)

      pe_south = rank - 1
      if (pe_south == -1) pe_south = MPI_PROC_NULL
      pe_north = rank + 1
      if (pe_north == NPES) pe_north = MPI_PROC_NULL

      nx = size(array,1)
      ny = ubound(array,2)-1

      call mpi_sendrecv(array(:,1),    nx, MPI_REAL, pe_South, tag_a, &
           &            array(:,ny+1), nx, MPI_REAL, pe_North, tag_a, &
           &            comm, status, ier) 
      call mpi_sendrecv(array(:,ny),   nx, MPI_REAL, pe_North, tag_b, &
           &            array(:,0),    nx, MPI_REAL, pe_South, tag_b, &
           &            comm, status, ier) 

   end subroutine fill

end module Halo_mod
