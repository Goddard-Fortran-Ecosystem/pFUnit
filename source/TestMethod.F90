module TestMethod_mod
   use TestCase_mod, only: TestCase
   implicit none

   public :: TestMethod
   public :: newTestMethod

   type, extends(TestCase) :: TestMethod
      procedure(empty), nopass, pointer :: userMethod => null()
      procedure(empty), nopass, pointer :: userSetUp => null()
      procedure(empty), nopass, pointer :: userTearDown => null()
   contains
     procedure :: runMethod
   end type TestMethod

   abstract interface
      subroutine empty()
      end subroutine empty
   end interface
   
contains

   function newTestMethod(name, method) result(this)
      type (TestMethod), pointer :: this
      character(len=*), intent(in) :: name
      procedure(empty) :: method

      allocate(this)

      call this%setSurrogate()
      call this%setName(name)

      this%userMethod => method

   end function newTestMethod

   subroutine runMethod(this)
      class (TestMethod), intent(inOut) :: this

      call this%userMethod()

   end subroutine runMethod

end module TestMethod_mod
