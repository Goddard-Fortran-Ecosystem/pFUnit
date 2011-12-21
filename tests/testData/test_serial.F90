subroutine test_serial(info)
   use pFUnit
   implicit none
   type (TestInfo_type) :: info

   if (query(info)) then
      call setTestType(info, SERIAL)
      return
   end if

   call assertEqual(SERIAL, getTestType(info))

end subroutine test_serial

subroutine test_serialFail(info)
   use pFUnit
   implicit none
   type (TestInfo_type) :: info

   if (query(info)) then
      call setTestType(info, SERIAL)
      return
   end if

   ! Want this test to fail simply to prove that the post-setup is actually run.
   call assertFalse(1 == 1)

end subroutine test_serialFail
