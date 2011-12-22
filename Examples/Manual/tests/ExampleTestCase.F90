#include 'reflection.h'
module ExampleTestCase_mod
   use TestCase_mod, only: TestCase
   implicit none
   private

   public :: suite
   public :: newExampleTestCase
   public :: ExampleTestCase
   public :: method1, method2
   public :: methodWith2Exceptions

   type, extends(TestCase) :: ExampleTestCase
      character(len=20), public :: runLog
      procedure(method1), pointer :: testMethod => null()
   contains
      procedure :: runTestMethod
   end type ExampleTestCase

   abstract interface
      subroutine I_method(this)
        use Test_mod
        import ExampleTestCase
        class (ExampleTestCase), intent(inOut) :: this
      end subroutine I_method
   end interface

contains

   function suite()
     use TestSuite_mod, only: TestSuite, newTestSuite
      type (TestSuite), pointer :: suite

      suite => newTestSuite('ExampleTestCase')

#define ADD(method) call suite%addTest(newExampleTestCase(REFLECT(method)))

      ADD(method1)
      ADD(method2)
      ADD(methodWith2Exceptions)
      
   end function suite

   function newExampleTestCase(method, name) result(this)
      type(ExampleTestCase), pointer :: this
      procedure(I_method) :: method
      character(len=*), intent(in) :: name

      allocate(this)
      this%testMethod => method
      call this%setName(name)

    end function newExampleTestCase

   recursive subroutine runTestMethod(this)
      class(ExampleTestCase), intent(inOut) :: this
      call this%testMethod()
   end subroutine runTestMethod

   subroutine method1(this)
      class (ExampleTestCase), intent(inOut) :: this
      this%runLog = 'run method1'
   end subroutine method1

   subroutine method2(this)
      class (ExampleTestCase), intent(inOut) :: this
      this%runLog = 'run method2'
   end subroutine method2

   subroutine methodWith2Exceptions(this)
      use Exception_mod, only: throw
      class (ExampleTestCase), intent(inOut) :: this

      call throw('failure A')
      call throw('failure B')
   end subroutine methodWith2Exceptions

   subroutine delete_(this)
      type (ExampleTestCase), intent(inOut) :: this
!!$      nullify(this%testMethod)
   end subroutine delete_

end module ExampleTestCase_mod

