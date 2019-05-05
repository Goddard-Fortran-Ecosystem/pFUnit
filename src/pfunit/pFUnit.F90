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
   use PF_MpiContext
   use PF_MpiTestCase
   use PF_MpiTestParameter
   use PF_MpiTestMethod
   implicit none
   private

   public :: MpiContext
   public :: MpiTestCase
   public :: MpiTestParameter
   public :: MpiTestMethod

   ! Hamcrest
!   public :: OnProcess, on_process
!   public :: OnEveryProcess, on_every_process
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
   ! Rename overlapping entities to avoid name conflicts
   ! Cannot use ONLY, because the list will change over time.
   use FUnit, funit_initialize => initialize
   use FUnit, funit_finalize => finalize
   use FUnit, funit_run => run
   use FUnit, funit_context => get_context
   use FUnit, funit_stub => stub
   use pFUnit_private
   use mpi
   implicit none
   
   public :: initialize
   public :: run
   public :: finalize
   public :: get_context
   public :: stub
   
contains

   subroutine initialize(extra_initialize)
      use FUnit, only: funit_initialize => initialize
      procedure() :: extra_initialize
      integer :: error

      call mpi_init(error)
      if (error /= MPI_SUCCESS) stop

      call funit_initialize(extra_initialize)

   end subroutine initialize


   logical function run(load_tests) result(status)
     use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
      procedure(LoadTests_interface) :: load_tests
      
      type(TestSuite) :: suite
      class(BaseTestRunner), allocatable :: runner
      type(TestResult) :: r
      class(ParallelContext), allocatable :: c

!!$      options = parse()
      suite = load_tests()
      allocate(runner, source=TestRunner(ResultPrinter(OUTPUT_UNIT)))
      c = get_context()
      r = runner%run(suite, c)
      status = r%wasSuccessful()

   end function run




   subroutine finalize(extra_finalize, successful)
      use mpi
#ifdef NAG
      use f90_unix_proc, only: exit
#endif
      use FUnit, only: funit_finalize => finalize
      procedure() :: extra_finalize
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
!!$         print*,'THIS NEEDS FIXING - must call extra_finalize() on all ranks.'
         call funit_finalize(extra_finalize, allSuccessful)
      else
         call funit_finalize(extra_finalize, successful=.true.)
      end if

   end subroutine finalize

   function get_context() result(context)
      class (ParallelContext), allocatable :: context

      context = MpiContext()

   end function get_context

   subroutine stub()
   end subroutine stub


end module pFUnit
