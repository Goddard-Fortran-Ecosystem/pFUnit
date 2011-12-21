abstract interface
   subroutine I_method(this)
      import GenericTestCase
      class (GenericTestCase), intent(inout) :: this
   end subroutine I_method

   integer function getNumProcessesMethod(this)
      import GenericTestCase
      class (GenericTestCase), intent(in) :: this
   end function getNumProcessesMethod

!!$   function I_suite(this)
!!$      import GenericTestCase
!!$      import TestSuite
!!$      class (GenericTestCase), intent(inOut) :: this
!!$      type (TestSuite), pointer :: I_suite
!!$   end function I_suite

end interface
   
