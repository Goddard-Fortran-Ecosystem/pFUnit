! Unused - and untested?
module TestDecorator_mod
   use Test_mod
   implicit none
   private
  
   type, abstract, extends(Test) :: TestDecorator
      class(Test), pointer :: fTest => null()
   contains
      procedure :: countTestCases
      procedure :: run
      procedure :: basicRun
   end type TestDecorator

contains

   integer function countTestCases(this)
      class (TestDecorator), intent(in) :: this
      countTestCases = this%fTest%countTestCases()
    end function countTestCases

   subroutine run(this, tstResult, context)
      use TestResult_mod
      use ParallelContext_mod
      class (TestDecorator), intent(inout) :: this
      class (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context
      call this%basicRun(tstResult, context)
   end subroutine run

   subroutine basicRun(this, tstResult, context)
      use TestResult_mod
      use ParallelContext_mod
      class (TestDecorator), intent(inout) :: this
      class (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context
      call this%fTest%run(tstResult, context)
   end subroutine basicRun

end module TestDecorator_mod
