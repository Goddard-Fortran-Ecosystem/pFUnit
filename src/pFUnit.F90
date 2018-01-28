!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: FUnit
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module pFUnit_private
   use PF_MpiContext_mod
   use PF_MpiTestCase_mod
   use PF_MpiTestParameter_mod
   use PF_MpiTestMethod_mod
   implicit none
   private

   public :: MpiContext, newMpiContext
   public :: MpiTestCase
   public :: MpiTestParameter
   public :: MpiTestMethod, newMpiTestMethod

end module pFUnit_private

!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: pFUnit
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module pFUnit
   use sFUnit_private
   use pFUnit_private

   public :: initialize
   public :: finalize
   public :: get_context
contains

   subroutine initialize()
      use sFUnit, only: init_sfunit => initialize
      integer :: error

      call mpi_init(error)
      if (error /= MPI_SUCCESS) stop


      call init_sfunit()

   end subroutine initialize

   subroutine finalize(successful)
      use mpi
#ifdef NAG
      use f90_unix_proc, only: exit
#endif
      logical, intent(in) :: successful

      logical :: allSuccessful
      logical :: amRoot

      integer :: error
      integer :: rank

      allSuccessful = successful
      call MPI_Comm_rank(MPI_COMM_WORLD, rank, error)
      amRoot = (rank == 0)
      call MPI_Bcast(allSuccessful, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, error)
      call MPI_Finalize(error)

      if (amRoot) then
         call sfunit_finalize(allSuccessful)
      else
         stop
      end if

   end subroutine finalize

   function get_context(use_mpi) result(context)
      class (ParallelContext), allocatable :: context
      logical, intent(in) :: use_mpi

      if (use_mpi) then
         print*,'Cannot use MPI - need to link with pfunit not sfunit.'
         stop
      end if
      context = MpiContext()
   end function get_context

end module pFUnit
