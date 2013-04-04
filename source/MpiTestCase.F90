module MpiTestCase_mod
   use MpiContext_mod
   use TestCase_mod, only: TestCase
   implicit none
   private

   public :: MpiTestCase

   type, abstract, extends(TestCase) :: MpiTestCase
      integer :: processRank
      integer :: numProcessesRequested
      type (MpiContext) :: context
      class (MpiContext), allocatable :: parentContext
   contains
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
      use Parallelcontext_mod
      use Exception_mod
      use SurrogateTestCase_mod
      class (MpiTestCase), intent(inout) :: this
      class (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context
      
      ! create subcommunicator
      select type (context)
      class is (MpiContext)
         allocate(this%parentContext, source=context)
      class default
         call throw('MPI test cannot run in a non-MPI context.')
         return
      end select

      call tstResult%run(this%getSurrogate(), context)

      call this%parentContext%barrier()
      deallocate(this%parentContext)

   end subroutine run

   recursive subroutine runBare(this)
      use Exception_mod
      class (MpiTestCase), intent(inout) :: this

      logical, allocatable :: noExcepts(:)

      this%context = this%parentContext%makeSubcontext(this%numProcessesRequested)

      allocate(noExcepts(this%parentContext%getNumProcesses()))
      call this%parentContext%gather([noExceptions()], noExcepts)

      if (all(noExcepts)) then
         if (this%context%isActive()) then
            call this%setUp()

            deallocate(noExcepts)
            allocate(noExcepts(this%context%getNumProcesses()))
            call this%context%gather([noExceptions()], noExcepts)

            if (all(noExcepts)) then
               call this%runMethod()
               call this%tearDown()
            end if
         end if

      end if

      call gatherExceptions(this%parentContext)

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

end module MpiTestCase_mod
