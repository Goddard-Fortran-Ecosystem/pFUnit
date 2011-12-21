! Serial TestCase 
module TestCase_mod
   use GenericTestCase_mod
   public :: TestCase

   type, abstract, extends(GenericTestCase) :: TestCase
   contains
      procedure :: getNumProcesses
      procedure :: run
   end type TestCase

contains

   integer function getNumProcesses(this) result(numProcesses)
      class (TestCase), intent(in) :: this
      numProcesses = 1
   end function getNumProcesses

! Implement deferred method from class Test
   subroutine run(this, tstResult, context)
      use SerialContext_mod
      use TestResult_mod
      use ParallelContext_mod
      class (TestCase), intent(inout) :: this
      type (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context

      ! Always run serial tests in a serial context.
      if (context%isRootProcess()) call tstResult%run(this, THE_SERIAL_CONTEXT)

   end subroutine run

end module TestCase_mod
