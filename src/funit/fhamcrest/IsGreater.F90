#include "unused_dummy.fh"

module pf_IsGreater
   use iso_fortran_env
   use pf_AbstractMatcher
   use pf_BaseMatcher
   use pf_MatcherDescription
   use pf_AbstractArrayWrapper
   use pf_ArrayWrapper
   implicit none
   private

   public :: IsGreater
   public :: greater_than

   type, extends(BaseMatcher) :: IsGreater
      private
      class(*), allocatable :: expected_value
   contains
      procedure :: matches
      procedure :: describe_to
      procedure :: describe_mismatch

!      procedure :: matches_intrinsic
   end type IsGreater

contains
   function greater_than(operand) result(matcher)
      type(IsGreater) :: matcher
      class(*), intent(in) :: operand

      matcher%expected_value = operand
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

   subroutine describe_to(this, description)
      class(IsGreater), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      call description%append_value(this%expected_value)
   end subroutine describe_to

   subroutine describe_mismatch(this, actual, description)
      class(IsGreater), intent(in) :: this
      class(*), intent(in) :: actual
      class(MatcherDescription), intent(inout) :: description

      _UNUSED_DUMMY(this)

      call description%append_text("was ")
      call description%append_value(actual)
   end subroutine describe_mismatch
end module pf_IsGreater