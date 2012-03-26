module TestAssert_mod
   use Assert_mod
   use pFUnitException_mod
   implicit none

contains

   subroutine test_FailNoMsg()
      call fail() ! fail
      if (.not. catch()) then
         call throw("Procedure 'fail()' did not throw an exception.")
      end if
   end subroutine test_FailNoMsg

   subroutine test_Fail()
      call fail('message') ! fail
      if (.not. catch('message')) then
         call throw("Procedure 'fail()' did not throw expected exception.")
      end if
   end subroutine test_Fail
   
end module TestAssert_mod
