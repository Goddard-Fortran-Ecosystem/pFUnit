! *** mpi test cases ***
module testHalo_mod
   use pFUnit
   implicit none
   private
   
   public :: testHaloFill

   integer, parameter, public :: NPROC_TESTHALOFILL(3) = (/ 1, 2, 5 /)

contains

   subroutine testHaloFill(info)
      use Halo_mod, only: fill
      type (TestInfo_type) :: info

      integer :: comm
      integer, parameter :: NX_LOC = 4
      integer, parameter :: NY_LOC = 4
      real :: array(NX_LOC,-1:NY_LOC+1)

      integer :: rank, npes
      integer :: ier

      integer :: pe_south, pe_north

      ! standard MPI stuff
      npes = numProcesses(info)
      rank = processRank(info)
      comm = mpiCommunicator(info)

      array = rank
      pe_south = rank - 1
      pe_north = rank + 1

      call fill(array, comm)

      if (rank > 0) call assertEqual(pe_south, array(:,-1))
      if (rank < npes-1) call assertEqual(pe_north, array(:,NY_LOC+1))

   end subroutine testHaloFill

end module testHalo_mod
