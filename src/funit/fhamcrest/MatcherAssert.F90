module pf_MatcherAssert
   use pf_AbstractMatcher
   use pf_SourceLocation
   use pf_Array
   use pf_ExceptionList, only: throw
   implicit none
   private

   public :: assert_that
   
   interface assert_that
      module procedure :: assert_that_no_reason
      module procedure :: assert_that_reason

      module procedure :: assert_that_1d
      module procedure :: assert_that_1d_reason
   end interface assert_that

contains

   subroutine assert_that_no_reason(actual, matcher, location)
      class(*), intent(in) :: actual
      class(AbstractMatcher), intent(in) :: matcher
      type (SourceLocation), optional, intent(in) :: location

      call assert_that('', actual, matcher, location=location)
      
   end subroutine assert_that_no_reason
 

   subroutine assert_that_reason(reason, actual, matcher, location)
      use pf_StringDescription
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
 
   subroutine assert_that_1d(actual, matcher, location)
     use pf_StringDescription
     class(*), intent(in) :: actual(:)
     class(AbstractMatcher), intent(in) :: matcher
     type (SourceLocation), optional, intent(in) :: location

     call assert_that('', actual, matcher, location)
     
   end subroutine assert_that_1d

   subroutine assert_that_1d_reason(reason, actual, matcher, location)
     use pf_StringDescription
     character(*), intent(in) :: reason
     class(*), intent(in) :: actual(:)
     class(AbstractMatcher), intent(in) :: matcher
     type (SourceLocation), optional, intent(in) :: location

     call assert_that(reason, wrap_array(actual), matcher, location)
     
   end subroutine assert_that_1d_reason

end module pf_MatcherAssert
