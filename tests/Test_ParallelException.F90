#include "reflection.h"
module Test_ParallelException_mod
   use ParallelException_mod, only: anyExceptions
   use MpiTestMethod_mod
   implicit none

   private
   
   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite
      use TestSuite_mod, only: newTestSuite
      use TestMethod_mod, only: newTestMethod!, TestMethod

      type (TestSuite) :: suite

      suite = newTestSuite('ExceptionTests')

#define ADD(method, npes) call suite%addTest(newMpiTestMethod(REFLECT(method), numProcesses=npes))
      ADD(test_anyExceptions_none, 3)

   end function suite

   subroutine test_anyExceptions_none(this)
      use Assert_mod
      class (MpiTestMethod), intent(inout) :: this

      call assertFalse(anyExceptions(this%getContext()))
      
   end subroutine test_anyExceptions_none

end module Test_ParallelException_mod
