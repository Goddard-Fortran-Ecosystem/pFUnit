! Implement deffered method from class Test
integer function countTestCases_TestCase(this) result(countTestCases)
   class (GenericTestCase), intent(in) :: this
   countTestCases = 1
end function countTestCases_TestCase

! Remaining methods are extensions to the class Test
subroutine runBare(this)
   use Exception_mod, only: noExceptions
   class (GenericTestCase), intent(inout) :: this

   call this%setUp()
   if (noExceptions()) then
      call this%runTestMethod()
      call this%tearDown()
   end if

end subroutine runBare

subroutine setUp(this)
   class (GenericTestCase), intent(inout) :: this
end subroutine setUp

subroutine tearDown(this)
   class (GenericTestCase), intent(inout) :: this
end subroutine tearDown
