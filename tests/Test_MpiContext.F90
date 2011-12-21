module Test_MpiContext_mod
   use F2kUnit
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite
!!$      use TestSuite_mod, only: newTestSuite
!!$      use SimpleTestMethod_mod, only: newSimpleTestMethod, SimpleTestMethod
!!$
      type (TestSuite), pointer :: suite
!!$
!!$      allocate(suite)
!!$      suite = newTestSuite('MpiContext')
!!$      call suite%addTest(newSimpleTestMethod(testMethod=testNumProcesses, name='testNumProcesses', numProcesses = 1))
!!$   end function suite
!!$
!!$   subroutine testNumProcesses(context)
!!$      class (ParallelContext), intent(in) :: context
!!$      call assertEqual(1, context%getNumProcesses())
!!$
!!$   end subroutine testNumProcesses
   end function suite
end module Test_MpiContext_mod


