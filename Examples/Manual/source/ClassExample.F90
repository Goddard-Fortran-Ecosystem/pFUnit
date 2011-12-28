#include 'reflection.h'
module ClassExample_mod
   use TestCase_mod, only: TestCase
   implicit none
   private

   public :: suite
   public :: newClassExample
   public :: ClassExample
   public :: method1, method2
   public :: methodWith2Exceptions

   type, extends(TestCase) :: ClassExample
      character(len=20), public :: runLog
      procedure(method1), pointer :: testMethod => null()
   contains
      procedure :: runTestMethod
   end type ClassExample

   abstract interface
      subroutine I_method(this)
        use Test_mod
        import ClassExample
        class (ClassExample), intent(inOut) :: this
      end subroutine I_method
   end interface

contains

   function suite()
     use TestSuite_mod, only: TestSuite, newTestSuite
      type (TestSuite), pointer :: suite

      suite => newTestSuite('ClassExample')

#define ADD(method) call suite%addTest(newClassExample(REFLECT(method)))

      ADD(method1)
      ADD(method2)
      ADD(methodWith2Exceptions)
      
   end function suite

   function newClassExample(method, name) result(this)
      type(ClassExample), pointer :: this
      procedure(I_method) :: method
      character(len=*), intent(in) :: name

      allocate(this)
      this%testMethod => method
      call this%setName(name)

    end function newClassExample

   recursive subroutine runTestMethod(this)
      class(ClassExample), intent(inOut) :: this
      call this%testMethod()
   end subroutine runTestMethod

   subroutine method1(this)
      class (ClassExample), intent(inOut) :: this
      this%runLog = 'run method1'
   end subroutine method1

   subroutine method2(this)
      class (ClassExample), intent(inOut) :: this
      this%runLog = 'run method2'
   end subroutine method2

   subroutine methodWith2Exceptions(this)
      use Exception_mod, only: throw
      class (ClassExample), intent(inOut) :: this

      call throw('failure A')
      call throw('failure B')
   end subroutine methodWith2Exceptions

   subroutine delete_(this)
      type (ClassExample), intent(inOut) :: this
!!$      nullify(this%testMethod)
   end subroutine delete_

end module ClassExample_mod

