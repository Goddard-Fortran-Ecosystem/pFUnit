module DynamicTestCase_mod
   use TestCase_mod
   implicit none
   private

   public :: DynamicTestCase
   public :: newDynamicTestCase
   public :: delete

   type, extends(TestCase) :: DynamicTestCase
      integer :: placeholder
      procedure(I_method), pointer :: testMethod => null()
   contains
      procedure :: runTestMethod
   end type DynamicTestCase
   
   abstract interface
      subroutine I_method(this)
         import DynamicTestCase
         class (DynamicTestCase), intent(inOut) :: this
      end subroutine I_method
   end interface
   
   interface delete
      module procedure delete_
   end interface
   
contains

   function newDynamicTestCase(testMethod, name) result(this)
      type (DynamicTestCase), pointer :: this
      character(len=*), intent(in) :: name
      interface
         subroutine testMethod(this)
            import DynamicTestCase
            class (DynamicTestCase), intent(inout) :: this
         end subroutine testMethod
      end interface

      allocate(this)
      this%name = trim(name)
      this%testMethod => testMethod

   end function newDynamicTestCase

   subroutine runTestMethod(this)
      class(DynamicTestCase), intent(inOut) :: this
      call this%testMethod()
   end subroutine runTestMethod

   subroutine delete_(this)
      type (DynamicTestCase), intent(inOut) :: this
      nullify(this%testMethod)
   end subroutine delete_

end module DynamicTestCase_mod
