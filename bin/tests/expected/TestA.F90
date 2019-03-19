module TestA
   use pfunit
   implicit none

contains

   ! First test
   !@test
   subroutine testMethodA()
   end subroutine testMethodA

   ! Second test
   !@test
   subroutine testMethodB()
   end subroutine testMethodB
   
   !@mpitest(npes=[1,3,5])
   subroutine testMethodC(this)
      class (MpiTestMethod), intent(inout) :: this
   end subroutine testMethodC

end module TestA



module WrapTestA
   use pFUnit
   use TestA
   implicit none
   private

contains


end module WrapTestA

function TestA_suite() result(suite)
   use pFUnit
   use WrapTestA
   use TestA
   type (TestSuite) :: suite

   integer, allocatable :: npes(:)

   suite = newTestSuite('TestA_suite')

   call suite%addTest(newTestMethod('testMethodA', testMethodA))

   call suite%addTest(newTestMethod('testMethodB', testMethodB))

   call suite%addTest(newMpiTestMethod('testMethodC', testMethodC, 1))
   call suite%addTest(newMpiTestMethod('testMethodC', testMethodC, 3))
   call suite%addTest(newMpiTestMethod('testMethodC', testMethodC, 5))


end function TestA_suite

