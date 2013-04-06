module TestTracer_mod
   use pfunit_mod
   implicit none
   private

   public :: TestTracer

@testCase
   type, extends(TestCase) :: TestTracer
      procedure, pointer :: userMethod => null()
   end type TestTracer

   interface TestTracer
      module procedure newTestTracer
   end interface TestTracer

contains

   function newTestTracer(name, userMethod) result(aTest)
      type (TestTracer) :: aTest
      character(len=*), intent(in) :: name
      procedure(method) :: userMethod
   end function newTestTracer

@test
subroutine




end module TestTracer_mod
