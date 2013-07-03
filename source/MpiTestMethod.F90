module MpiTestMethod_mod
   use Test_mod
   use TestCase_mod
   use MpiTestCase_mod
   implicit none
   private

   public :: MpiTestMethod
   public :: newMpiTestMethod

   type, extends(MpiTestCase) :: MpiTestMethod
      procedure(mpiMethod), pointer :: userMethod => null()
   contains
      procedure :: runMethod
   end type MpiTestMethod

   abstract interface
      subroutine mpiMethod(this)
         import MpiTestMethod
         class (MpiTestMethod), intent(inout) :: this
      end subroutine mpiMethod
   end interface

contains

   function newMpiTestMethod(name, userMethod, numProcesses) result(mpiTest)
      character(len=*), intent(in) :: name
      procedure (runMethod) :: userMethod
      integer, intent(in) :: numProcesses
      type (MpiTestMethod) :: mpiTest

      call mpiTest%setName(name)
      mpiTest%userMethod => userMethod
      call mpiTest%setNumProcesses(numProcesses)

   end function newMpiTestMethod

   subroutine runMethod(this)
      class (MpiTestMethod), intent(inout) :: this
      call this%userMethod()
   end subroutine runMethod

end module MpiTestMethod_mod
