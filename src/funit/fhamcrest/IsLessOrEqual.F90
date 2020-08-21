module pf_IsLessOrEqual
   use iso_fortran_env
   use pf_BaseMatcher
   use pf_MatcherDescription
   implicit none
   private

   public :: IsLessOrEqual
   public :: less_than_or_equal_to

   type, extends(BaseMatcher) :: IsLessOrEqual
      private
      class(*), allocatable :: expected_value
   contains
      procedure :: matches
      procedure :: describe_to
      procedure :: describe_mismatch
      procedure :: describe_numeric_mismatch
   end type IsLessOrEqual

contains
   function less_than_or_equal_to(operand) result(matcher)
      type(IsLessOrEqual) :: matcher
      class(*), intent(in) :: operand

      matcher%expected_value = operand
   end function less_than_or_equal_to

   recursive logical function matches(this, actual_value)
      class(IsLessOrEqual), intent(in) :: this
      class(*), intent(in) :: actual_value

      select type(e => this%expected_value)
      type is (integer(kind=INT32))
         select type(a => actual_value)
         type is (integer(kind=INT32))
            matches = (a <= e)
         class default
            matches = .false.
         end select
      type is (integer(kind=INT64))
         select type(a => actual_value)
         type is (integer(kind=INT64))
            matches = (a <= e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL32))
         select type(a => actual_value)
         type is (real(kind=REAL32))
            matches = (a <= e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL64))
         select type(a => actual_value)
         type is (real(kind=REAL64))
            matches = (a <= e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL128))
         select type(a => actual_value)
         type is (real(kind=REAL128))
            matches = (a <= e)
         class default
            matches = .false.
         end select
      class default
         matches = .false.
      end select
   end function matches

   subroutine describe_to(this, description)
      class(IsLessOrEqual), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      call description%append_text("integer or real value less than or equal to ")
      call description%append_value(this%expected_value)
   end subroutine describe_to

   subroutine describe_mismatch(this, actual, description)
      class(IsLessOrEqual), intent(in) :: this
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
      class(IsLessOrEqual), intent(in) :: this
      class(*), intent(in) :: actual
      class(MatcherDescription), intent(inout) :: description

      call description%append_value(actual)
      call description%append_text(" is strictly greater than ")
      call description%append_value(this%expected_value)
   end subroutine describe_numeric_mismatch
end module pf_IsLessOrEqual
