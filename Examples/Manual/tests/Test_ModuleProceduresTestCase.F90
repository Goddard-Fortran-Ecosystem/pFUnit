#include 'reflection.h'
module Test_ModuleProcedures_mod
   use ModuleProcedures, only: iProc
   use Assert_mod
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use SimpleTestMethod_mod, only: newSimpleTestMethod, SimpleTestMethod
      type (TestSuite), pointer :: suite

      allocate(suite)
      suite = newTestSuite('ModuleProcedures')
#define ADD(method) call suite%addTest(newSimpleTestMethod(REFLECT(method)))

      ADD(testiProc_1)
      ADD(testiProc_2)
      ADD(testiProc_3)

   end function suite

   subroutine testiProc_1()
!      call assertEqual('1', iProc_1())
   end subroutine testiProc_1


   subroutine testiProc_2()
!      call assertEqual('2', iProc_2())
   end subroutine testiProc_2

   subroutine testiProc_3()
!      call assertEqual('3',iProc_3())
   end subroutine testiProc_3


end module Test_ModuleProcedures_mod
