module Test_MpiContext_mod
   use pfunit_mod
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite
      use TestSuite_mod, only: newTestSuite
      use TestMethod_mod, only: newTestMethod, TestMethod

      type (TestSuite) :: suite

      suite = newTestSuite('MpiContext')
!!$      call suite%addTest(newMpiTestMethod(testMethod=testNumProcesses, name='testNumProcesses', numProcesses = 1))

   end function suite

   subroutine testNumProcesses(context)
      class (ParallelContext), intent(in) :: context
      call assertEqual(1, context%getNumProcesses())
   end subroutine testNumProcesses

end module Test_MpiContext_mod


