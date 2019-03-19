module TestA_mod
   use pfunit_mod
   implicit none

contains

   !@test
   ! First test
   subroutine testMethodA()
   end subroutine testMethodA

   !@test
   ! Second test
   subroutine testMethodB()
   end subroutine testMethodB
   
   !@mpitest(npes=[1,3,5])
   subroutine testMethodC(this)
      class (MpiTestMethod), intent(inout) :: this
   end subroutine testMethodC

end module TestA_mod



module WrapTestA_mod
   use FUnit
   use TestA_mod
   implicit none
   private

contains


end module WrapTestA_mod

function TestA_mod_suite() result(suite)
   use FUnit
   use TestA_mod
   use WrapTestA_mod
   implicit none
   type (TestSuite) :: suite

   class (Test), allocatable :: t

   suite = TestSuite('TestA_mod_suite')

   t = TestMethod('testMethodA', testMethodA)
   call suite%addTest(t)

   t = TestMethod('testMethodB', testMethodB)
   call suite%addTest(t)

   t = MpiTestMethod('testMethodC', testMethodC, 1)
   call suite%addTest(t)
   t = MpiTestMethod('testMethodC', testMethodC, 3)
   call suite%addTest(t)
   t = MpiTestMethod('testMethodC', testMethodC, 5)
   call suite%addTest(t)


end function TestA_mod_suite

