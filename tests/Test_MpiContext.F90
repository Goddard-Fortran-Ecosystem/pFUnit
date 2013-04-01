module Test_MpiContext_mod
   use ParallelContext_mod
   use TestCase_mod
   use MpiTestCase_mod
   use MpiTestMethod_mod
   use Assert_mod
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite
      use TestSuite_mod, only: newTestSuite

      type (TestSuite) :: suite

      suite = newTestSuite('MpiContext')
      call suite%addTest(newMpiTestMethod('testNumProcesses1', userMethod=testNumProcesses1,  numProcesses = 1))
      call suite%addTest(newMpiTestMethod('testNumProcesses3', userMethod=testNumProcesses3,  numProcesses = 3))

   end function suite

   subroutine testNumProcesses1(context)
      class (MpiTestMethod), intent(inout) :: context
      call assertEqual(1, context%getNumProcesses())
      print*,__LINE__,__FILE__
   end subroutine testNumProcesses1

   subroutine testNumProcesses3(context)
      class (MpiTestMethod), intent(inout) :: context
      call assertEqual(3, context%getNumProcesses())
   end subroutine testNumProcesses3

end module Test_MpiContext_mod


