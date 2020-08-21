module pf_IsLessOrEqual
   use iso_fortran_env
   use pf_RelationalMatcher
   implicit none
   private

   public :: IsLessOrEqual
   public :: less_than_or_equal_to

   type, extends(RelationalMatcher) :: IsLessOrEqual
   contains
      procedure :: matches
   end type IsLessOrEqual

contains
   function less_than_or_equal_to(operand) result(matcher)
      type(IsLessOrEqual) :: matcher
      class(*), intent(in) :: operand

      call matcher%super(operand, "less than or equal to ", " is strictly greater than ")
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
end module pf_IsLessOrEqual
