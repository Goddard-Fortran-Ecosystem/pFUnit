!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: MpiTestCase
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC 
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

module MpiTestCase_mod
   use MpiContext_mod
   use TestCase_mod
   use ParameterizedTestCase_mod, only: ParameterizedTestCase
!!$   use ParameterizedTestCase_mod, only: MAX_LEN_LABEL
   implicit none
   private

   public :: MpiTestCase

   type, abstract, extends(ParameterizedTestCase) :: MpiTestCase
      integer :: processRank
      integer :: numProcessesRequested
      type (MpiContext) :: context
      type (MpiContext) :: parentContext
   contains
      procedure :: setNumProcessesRequested
      procedure :: countTestCases => countTestCases_mpi
      procedure :: run
      procedure :: runBare
      procedure :: setUp
      procedure :: tearDown
      procedure :: setNumProcesses
      procedure :: getNumProcesses
      procedure :: getProcessRank
      procedure :: getMpiCommunicator
      procedure(runMethod), deferred :: runMethod
      procedure :: getContext
      procedure :: getParameterString
   end type MpiTestCase

   abstract interface
      recursive subroutine runMethod(this)
         import MpiTestCase
         class (MpiTestCase), intent(inOut) :: this
      end subroutine runMethod
   end interface

contains

   integer function countTestCases_mpi(this) result(countTestCases)
      class (MpiTestCase), intent(in) :: this
      countTestCases = 1
   end function countTestCases_mpi

   recursive subroutine run(this, tstResult, context)
      use TestResult_mod, only: TestResult
      use ParallelContext_mod
      use Exception_mod
      use SurrogateTestCase_mod
      class (MpiTestCase), intent(inout) :: this
      class (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context

      select type (context)
      type is (MpiContext)
         this%parentContext = context
      class default
         call throw('MPI test cannot run in a non-MPI context.')
         return
      end select

      call tstResult%run(this%getSurrogate(), context)

   end subroutine run

   recursive subroutine runBare(this)
      use Exception_mod
      use ParallelException_mod
      class (MpiTestCase), intent(inout) :: this

      ! create subcommunicator
      this%context = this%parentContext%makeSubcontext(this%numProcessesRequested)

      if (.not. anyExceptions(this%parentContext)) then
         if (this%context%isActive()) then
            call this%setUp()

            if (.not. anyExceptions(this%context)) then
               call this%runMethod()
               call this%tearDown()
            end if
         end if

      end if

      call gather(this%parentContext)

   end subroutine runBare

   subroutine setUp(this)
      class (MpiTestCase), intent(inout) :: this
   end subroutine setUp

   subroutine tearDown(this)
      class (MpiTestCase), intent(inout) :: this
   end subroutine tearDown

   integer function getMpiCommunicator(this) result(mpiCommunicator)
      class (MpiTestCase), intent(in) :: this
      mpiCommunicator = this%context%getMpiCommunicator()
   end function getMpiCommunicator

   subroutine setNumProcesses(this, numProcessesRequested)
      class (MpiTestCase), intent(inout) :: this
      integer, intent(in) :: numProcessesRequested

      this%numProcessesRequested = numProcessesRequested
   end subroutine setNumProcesses

   integer function getNumProcesses(this) result(numProcesses)
      class (MpiTestCase), intent(in) :: this
      numProcesses = this%context%getNumProcesses()
   end function getNumProcesses

   integer function getProcessRank(this) result(processRank)
      class (MpiTestCase), intent(in) :: this
      processRank = this%context%processRank()
   end function getProcessRank

   function getContext(this) result(context)
      class (MpiTestCase), intent(in) :: this
      class (MpiContext), allocatable :: context

      allocate(context, source=this%context)

   end function getContext

   subroutine setNumProcessesRequested(this, numProcesses)
      class (MpiTestCase), intent(inout) :: this
      integer, intent(in) :: numProcesses
      this%numProcessesRequested = numProcesses
   end subroutine setNumProcessesRequested

   function getParameterString(this) result(label)
      class (MpiTestCase), intent(in) :: this
      character(len=:), allocatable :: label

      allocate(character(len=100) :: label)
      write(label,'(a,i0)') 'npes=',this%numProcessesRequested
      label = trim(label)
   end function getParameterString

end module MpiTestCase_mod
