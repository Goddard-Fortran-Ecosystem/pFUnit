module pf_AssertThat_mod
   implicit none
   private

   public :: AssertThat

   interface AssertThat
      module procedure assertThat_msg_int32
      module procedure assertThat_msg_real32
   end interface AssertThat


   ! TODO:  What about pointers?   Need different interface, but analogous.
   ! Or maybe just don't support things like "same" object.
   ! AssertIsAssociated?
   
contains


   function assertThat_reason(reason, actual, matcher) result(t)
      character(*), intent(in) :: reason
      class(*), intent(in) :: actual
      class(Matcher), intent(in) :: actual

      t = matcher%assertThat(reason, actual, matcher)


   end function assertThat_reason


   function assertThat_(actual, matcher) result(t)
      class(*), intent(in) :: actual
      class(Matcher), intent(in) :: actual

      t = assertThat('', actual, matcher)
      
   end function assertThat_

   
end module pf_AssertThat_mod
