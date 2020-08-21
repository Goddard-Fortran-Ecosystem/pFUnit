module pf_IsGreaterOrEqual
   use iso_fortran_env
   use pf_RelationalMatcher
   implicit none
   private

   public :: IsGreaterOrEqual
   public :: greater_than_or_equal_to

   type, extends(RelationalMatcher) :: IsGreaterOrEqual
   contains
      procedure :: matches
   end type IsGreaterOrEqual

contains
   function greater_than_or_equal_to(operand) result(matcher)
      type(IsGreaterOrEqual) :: matcher
      class(*), intent(in) :: operand

      call matcher%super(operand, "greater than or equal to ", " is strictly less than ")
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
end module pf_IsGreaterOrEqual