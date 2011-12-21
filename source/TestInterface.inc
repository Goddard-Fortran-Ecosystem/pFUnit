abstract interface

  integer function I_CountTestCases(this)
    import Test
    class (Test), intent(in) :: this
  end function I_CountTestCases

end interface
  
abstract interface

  subroutine I_Run(this, tstResult, context)
     use ParallelContext_mod
    import Test
    import TestResult
    class (Test), intent(inout) :: this
    type (TestResult), intent(inout) :: tstResult
    class (ParallelContext), intent(in) :: context
  end subroutine I_Run

end interface
