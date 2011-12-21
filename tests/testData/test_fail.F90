subroutine test_fail()
   use pfunit
   implicit none

   call assertTrue(.false.)
   
end subroutine test_fail
