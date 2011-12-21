program main
   implicit none
   include 'mpif.h'

   integer :: ier
   integer :: npes, rank

   call mpi_init(ier)
   call mpi_comm_size(MPI_COMM_WORLD, npes, ier)
   call mpi_comm_rank(MPI_COMM_WORLD, rank, ier)

   if (rank == 0) then
      write(*,*)npes
   end if
   call mpi_finalize(ier)
   stop

end program main
