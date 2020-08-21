module pf_IsGreaterOrEqual
   use iso_fortran_env
   use pf_BaseMatcher
   use pf_MatcherDescription
   implicit none
   private

   public :: IsGreaterOrEqual
   public :: greater_than_or_equal_to

   type, extends(BaseMatcher) :: IsGreaterOrEqual
      private
      class(*), allocatable :: expected_value
   contains
      procedure :: matches
      procedure :: describe_to
      procedure :: describe_mismatch
      procedure :: describe_numeric_mismatch
   end type IsGreaterOrEqual

contains
   function greater_than_or_equal_to(operand) result(matcher)
      type(IsGreaterOrEqual) :: matcher
      class(*), intent(in) :: operand

      matcher%expected_value = operand
   end function greater_than_or_equal_to

   recursive logical function matches(this, actual_value)
      class(IsGreaterOrEqual), intent(in) :: this
      class(*), intent(in) :: actual_value

      select type(e => this%expected_value)
      type is (integer(kind=INT32))
         select type(a => actual_value)
         type is (integer(kind=INT32))
            matches = (a >= e)
         class default
            matches = .false.
         end select
      type is (integer(kind=INT64))
         select type(a => actual_value)
         type is (integer(kind=INT64))
            matches = (a >= e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL32))
         select type(a => actual_value)
         type is (real(kind=REAL32))
            matches = (a >= e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL64))
         select type(a => actual_value)
         type is (real(kind=REAL64))
            matches = (a >= e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL128))
         select type(a => actual_value)
         type is (real(kind=REAL128))
            matches = (a >= e)
         class default
            matches = .false.
         end select
      class default
         matches = .false.
      end select
   end function matches

   subroutine describe_to(this, description)
      class(IsGreaterOrEqual), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      call description%append_text("integer or real value greater than or equal to ")
      call description%append_value(this%expected_value)
   end subroutine describe_to

   subroutine describe_mismatch(this, actual, description)
      class(IsGreaterOrEqual), intent(in) :: this
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
      class(IsGreaterOrEqual), intent(in) :: this
      class(*), intent(in) :: actual
      class(MatcherDescription), intent(inout) :: description

      call description%append_value(actual)
      call description%append_text(" is strictly less than ")
      call description%append_value(this%expected_value)
   end subroutine describe_numeric_mismatch
end module pf_IsGreaterOrEqual