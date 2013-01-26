module SimpleTestMethod_mod
   use TestCase_mod, only: TestCase
   implicit none

   public :: SimpleTestMethod
   public :: newSimpleTestMethod

   type, extends(TestCase) :: SimpleTestMethod
      procedure(empty), nopass, pointer :: internalMethod => null()
   contains
     procedure :: runTestMethod
   end type SimpleTestMethod

   abstract interface
      subroutine empty()
      end subroutine empty
   end interface
   
contains

   function newSimpleTestMethod(testMethod, name) result(this)
      type (SimpleTestMethod), pointer :: this
      procedure(empty) :: testMethod
      character(len=*), intent(in) :: name

      allocate(this)
      this%internalMethod => testMethod
      call this%setName(name)
   end function newSimpleTestMethod

   subroutine runTestMethod(this)
      class (SimpleTestMethod), intent(inOut) :: this
      call this%internalMethod()
   end subroutine runTestMethod

end module SimpleTestMethod_mod
