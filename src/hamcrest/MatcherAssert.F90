module pf_MatcherAssert_mod
   use pf_AbstractMatcher_mod
   use pf_SourceLocation_mod
   use funit, only: throw
   implicit none
   private

   public :: assert_that
   
   interface assert_that
      module procedure :: assert_that_no_reason
      module procedure :: assert_that_reason
   end interface assert_that

contains

   subroutine assert_that_no_reason(actual, matcher, location)
      class(*), intent(in) :: actual
      class(AbstractMatcher), intent(in) :: matcher
      type (SourceLocation), optional, intent(in) :: location

      call assert_that('', actual, matcher, location=location)
      
   end subroutine assert_that_no_reason
 

   subroutine assert_that_reason(reason, actual, matcher, location)
      use pf_StringDescription_mod
      character(*), intent(in) :: reason
      class(*), intent(in) :: actual
      class(AbstractMatcher), intent(in) :: matcher
      type (SourceLocation), optional, intent(in) :: location

      type (StringDescription) :: description

      if (.not. matcher%matches(actual)) then
         description = StringDescription()
         call description%append_text(reason)
         call description%append_text(new_line('a')//'Expected: ')
         call description%append_description_of(matcher)
         call description%append_text(new_line('a')//'     but: ')
         call matcher%describe_mismatch(actual, description)
         call throw(description%to_string(), location)
      end if
      
   end subroutine assert_that_reason
 
end module pf_MatcherAssert_mod
