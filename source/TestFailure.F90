module TestFailure_mod
   use Exception_mod
   implicit none
   private

   public :: TestFailure

   type TestFailure
      character(len=80) :: testName
      type (Exception), allocatable :: exceptions(:)
   end type TestFailure

!!$   interface TestFailure
!!$      module procedure newTestFailure
!!$   end interface TestFailure
!!$
!!$contains
!!$
!!$   function newTestFailure(testName, list)
!!$      character(len=*), intent(in) :: testName
!!$      type (Exception), intent(in) :: exceptions(:)
!!$   end function newTestFailure

end module TestFailure_mod
