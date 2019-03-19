! First test
!@test
subroutine testMethodA()
end subroutine testMethodA

! Second test
!@test
subroutine testMethodB
end subroutine testMethodB

! An MPI test
!@mpitest(npes=[1,3,5])
subroutine testMethodC(this)
   use pfunit_mod
   class (MpiTestMethod), intent(inout) :: this
end subroutine testMethodC



module Wrapsimple
   use FUnit
   implicit none
   private

contains


end module Wrapsimple

function simple_suite() result(suite)
   use FUnit
   use Wrapsimple
   implicit none
   type (TestSuite) :: suite

   class (Test), allocatable :: t

   external testMethodA
   external testMethodB
   external testMethodC


   suite = TestSuite('simple_suite')

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


end function simple_suite

