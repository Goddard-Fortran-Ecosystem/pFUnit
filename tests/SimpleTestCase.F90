#include "reflection.h"
module SimpleTestCase_mod
   use TestCase_mod, only: TestCase
   implicit none
   private

   public :: suite
   public :: newSimpleTestCase
   public :: SimpleTestCase
   public :: method1, method2
   public :: methodWith2Exceptions

   type, extends(TestCase) :: SimpleTestCase
      character(len=20), public :: runLog
      procedure(method1), pointer :: testMethod => null()
   contains
      procedure :: runTestMethod
   end type SimpleTestCase

   abstract interface
      subroutine I_method(this)
        use Test_mod
        import SimpleTestCase
        class (SimpleTestCase), intent(inOut) :: this
      end subroutine I_method
   end interface

contains

   function suite()
     use TestSuite_mod, only: TestSuite, newTestSuite
      type (TestSuite), pointer :: suite

      suite => newTestSuite('SimpleTestCase')

#define ADD(method) call suite%addTest(newSimpleTestCase(REFLECT(method)))

      ADD(method1)
      ADD(method2)
      ADD(methodWith2Exceptions)
      
   end function suite

   function newSimpleTestCase(method, name) result(this)
      type(SimpleTestCase), pointer :: this
      procedure(I_method) :: method
      character(len=*), intent(in) :: name

      allocate(this)
      this%testMethod => method
      call this%setName(name)

    end function newSimpleTestCase

   recursive subroutine runTestMethod(this)
      class(SimpleTestCase), intent(inOut) :: this
      call this%testMethod()
   end subroutine runTestMethod

   subroutine method1(this)
      class (SimpleTestCase), intent(inOut) :: this
      this%runLog = 'run method1'
   end subroutine method1

   subroutine method2(this)
      class (SimpleTestCase), intent(inOut) :: this
      this%runLog = 'run method2'
   end subroutine method2

   subroutine methodWith2Exceptions(this)
      use Exception_mod, only: throw
      class (SimpleTestCase), intent(inOut) :: this

      call throw('failure A')
      call throw('failure B')
   end subroutine methodWith2Exceptions

   subroutine delete_(this)
      type (SimpleTestCase), intent(inOut) :: this
!!$      nullify(this%testMethod)
   end subroutine delete_

end module SimpleTestCase_mod

