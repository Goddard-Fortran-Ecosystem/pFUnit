module pf_RelationalMatcher
   use iso_fortran_env
   use pf_BaseMatcher
   use pf_MatcherDescription
   implicit none
   private

   public :: RelationalMatcher

   type, abstract, extends(BaseMatcher) :: RelationalMatcher
      ! private
      class(*), allocatable :: expected_value

      character(:), allocatable :: relationship
      character(:), allocatable :: err_msg
   contains
      procedure :: super ! a hack
      procedure :: describe_to
      procedure :: describe_mismatch
      procedure :: describe_numeric_mismatch
   end type RelationalMatcher

contains
   subroutine super(this, expected_value, relationship, err_msg)
      class(RelationalMatcher), intent(inout) :: this
      class(*), intent(in) :: expected_value
      character(*), intent(in) :: relationship
      character(*), intent(in) :: err_msg

      this%expected_value = expected_value
      this%relationship = relationship
      this%err_msg = err_msg
   end subroutine super

   subroutine describe_to(this, description)
      class(RelationalMatcher), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      call description%append_text("integer or real value ")
      call description%append_text(this%relationship)
      call description%append_value(this%expected_value)
   end subroutine describe_to

   subroutine describe_mismatch(this, actual, description)
      class(RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual
      class(MatcherDescription), intent(inout) :: description

      select type (actual)
      type is (integer(kind=INT32))
         call this%describe_numeric_mismatch(actual, description)
      type is (integer(kind=INT64))
         call this%describe_numeric_mismatch(actual, description)
      type is (real(kind=REAL32))
         call this%describe_numeric_mismatch(actual, description)
      type is (real(kind=REAL64))
         call this%describe_numeric_mismatch(actual, description)
      class default
         call description%append_value(actual)
         call description%append_text(" is not integer or real")
      end select
   end subroutine describe_mismatch

   subroutine describe_numeric_mismatch(this, actual, description)
      class(RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual
      class(MatcherDescription), intent(inout) :: description

      call description%append_value(actual)
      call description%append_text(this%err_msg)
      call description%append_value(this%expected_value)
   end subroutine describe_numeric_mismatch
end module pf_RelationalMatcher