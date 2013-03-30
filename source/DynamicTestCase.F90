module DynamicTestCase_mod
   use TestCase_mod
   implicit none
   private

   public :: DynamicTestCase
   public :: newDynamicTestCase
   public :: delete

   type, extends(TestCase) :: DynamicTestCase
      procedure(testMethod), pointer :: testMethod => null()
   contains
      procedure :: runMethod
   end type DynamicTestCase
   
   abstract interface
      subroutine testmethod(this)
         import DynamicTestCase
         class (DynamicTestCase), intent(inOut) :: this
       end subroutine testMethod
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
      call this%setName(trim(name))
      this%testMethod => testMethod

   end function newDynamicTestCase

   subroutine delete_(this)
      type (DynamicTestCase), intent(inOut) :: this
      nullify(this%testMethod)
   end subroutine delete_

   subroutine runMethod(this)
      class (DynamicTestCase), intent(inout) :: this
      call this%testMethod
   end subroutine runMethod

end module DynamicTestCase_mod
