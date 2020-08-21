module pf_IsLess
   use iso_fortran_env
   use pf_RelationalMatcher
   implicit none
   private

   public :: IsLess
   public :: less_than

   type, extends(RelationalMatcher) :: IsLess
   contains
      procedure :: matches
   end type IsLess

contains
   function less_than(operand) result(matcher)
      type(IsLess) :: matcher
      class(*), intent(in) :: operand

      call matcher%super(operand, "strictly less than ", " is greater than or equal to ")
   end function less_than

   recursive logical function matches(this, actual_value)
      class(IsLess), intent(in) :: this
      class(*), intent(in) :: actual_value

      select type(e => this%expected_value)
      type is (integer(kind=INT32))
         select type(a => actual_value)
         type is (integer(kind=INT32))
            matches = (a < e)
         class default
            matches = .false.
         end select
      type is (integer(kind=INT64))
         select type(a => actual_value)
         type is (integer(kind=INT64))
            matches = (a < e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL32))
         select type(a => actual_value)
         type is (real(kind=REAL32))
            matches = (a < e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL64))
         select type(a => actual_value)
         type is (real(kind=REAL64))
            matches = (a < e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL128))
         select type(a => actual_value)
         type is (real(kind=REAL128))
            matches = (a < e)
         class default
            matches = .false.
         end select
      class default
         matches = .false.
      end select
   end function matches
end module pf_IsLess
