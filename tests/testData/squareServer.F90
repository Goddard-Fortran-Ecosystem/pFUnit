! Returns square of integers passed in through stdin.
! Used for testing MpiClient.
program main
   implicit none
   include 'mpif.h'

   integer :: ier
   integer :: npes, rank
   integer :: n

   call mpi_init(ier)
   call mpi_comm_size(MPI_COMM_WORLD, npes, ier)
   call mpi_comm_rank(MPI_COMM_WORLD, rank, ier)
   if (rank == 0) then
      write(*,*)'ping' ! signal am alive
      do
         read(*,*,iostat = ier) n
         if (ier /= 0 .or. n == 0)  exit
         write(*,*) n**2
      end do
   end if

   call mpi_finalize(ier)
   stop

end program main
