module pf_IsGreater
   use iso_fortran_env
   use pf_RelationalMatcher
   implicit none
   private

   public :: IsGreater
   public :: greater_than

   type, extends(RelationalMatcher) :: IsGreater
   contains
      procedure :: matches
   end type IsGreater

contains
   function greater_than(operand) result(matcher)
      type(IsGreater) :: matcher
      class(*), intent(in) :: operand

      call matcher%super(operand, "strictly greater than ", " is less than or equal to ")
   end function greater_than

   recursive logical function matches(this, actual_value)
      class(IsGreater), intent(in) :: this
      class(*), intent(in) :: actual_value

      select type(e => this%expected_value)
      type is (integer(kind=INT32))
         select type(a => actual_value)
         type is (integer(kind=INT32))
            matches = (a > e)
         class default
            matches = .false.
         end select
      type is (integer(kind=INT64))
         select type(a => actual_value)
         type is (integer(kind=INT64))
            matches = (a > e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL32))
         select type(a => actual_value)
         type is (real(kind=REAL32))
            matches = (a > e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL64))
         select type(a => actual_value)
         type is (real(kind=REAL64))
            matches = (a > e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL128))
         select type(a => actual_value)
         type is (real(kind=REAL128))
            matches = (a > e)
         class default
            matches = .false.
         end select
      class default
         matches = .false.
      end select
   end function matches
end module pf_IsGreater