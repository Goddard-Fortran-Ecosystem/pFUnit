module MpiTestCase_mod
   use MpiContext_mod
   use TestCase_mod, only: TestCase
   implicit none
   private

   public :: MpiTestCase

   type, abstract, extends(TestCase) :: MpiTestCase
!!$      integer :: processRank
      integer :: numProcesses
      type (MpiContext) :: context
   contains
      procedure :: countTestCases => countTestCases_mpi
      procedure :: run
      procedure :: runBare
      procedure :: setUp
      procedure :: tearDown
      procedure(runTestMethod), deferred :: runTestMethod
      procedure :: gatherExceptions
      procedure :: getMpiCommunicator
   end type MpiTestCase

   abstract interface
      subroutine runTestMethod(this)
         import MpiTestCase
         class (MpiTestCase), intent(inOut) :: this
       end subroutine runTestMethod
   end interface

contains

   integer function countTestCases_mpi(this) result(countTestCases)
      class (MpiTestCase), intent(in) :: this
      countTestCases = 1
   end function countTestCases_mpi

   subroutine run(this, tstResult, context)
      use TestResult_mod, only: TestResult
      use Parallelcontext_mod
      use Exception_mod
      class (MpiTestCase), intent(inout) :: this
      type  (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context

      ! create subcommunicator
      select type (context)
      class is (MpiContext)
         this%context = context%makeSubcontext(this%numProcesses)
         if (.not. noExceptions()) return
      class default
         call throw('MPI test cannot run in a non-MPI context.')
         return
      end select

      if (this%context%isActive()) then
         call tstResult%run(this, this%context)
      end if
      
      select type (context)
      class is (MpiContext)
         call context%barrier()
      end select

   end subroutine run

   subroutine runBare(this)
      class (MpiTestCase), intent(inout) :: this
      
      call this%setUp()
      call this%runTestMethod()
      call this%tearDown()
      call this%gatherExceptions()

   end subroutine runBare

   subroutine setUp(this)
      class (MpiTestCase), intent(inout) :: this
   end subroutine setUp

   subroutine tearDown(this)
      class (MpiTestCase), intent(inout) :: this
   end subroutine tearDown

   subroutine gatherExceptions(this)
      class (MpiTestCase), intent(inout) :: this
   end subroutine gatherExceptions

   integer function getMpiCommunicator(this) result(mpiCommunicator)
      class (MpiTestCase), intent(in) :: this
      mpiCommunicator = this%context%getMpiCommunicator()
   end function getMpiCommunicator
end module MpiTestCase_mod
