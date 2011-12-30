function getName(this) result(name)
   class(Test), intent(in) :: this
   character(len=len_trim(this%name)) :: name
   name = trim(this%name)
end function getName

subroutine setName(this, name)
   class(Test), intent(inout) :: this
   character(len=*), intent(in) :: name
   this%name = trim(name)
end subroutine setName

!TODO should be deferred; workaround for ifort
integer function countTestCases(this)
   class (Test), intent(in) :: this
   countTestCases = -1 ! should be overrridden
end function countTestCases

subroutine iRun(this, tstResult, context)
   use ParallelContext_mod
   class (Test), intent(inout) :: this
   type (TestResult), intent(inout) :: tstResult
   class (ParallelContext), intent(in) :: context
end subroutine IRun
