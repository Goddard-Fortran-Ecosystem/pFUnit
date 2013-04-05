module TestFailure_mod
   use Exception_mod
   implicit none
   private

   public :: TestFailure

   type TestFailure
      type (Exception) :: exception
      character(len=40) :: testName
   end type TestFailure

end module TestFailure_mod
