module pf_Real128RelationalMatcher
   use iso_fortran_env
   use pf_RelationalMatcherV3
   implicit none
   private

   public :: less_than
   public :: less_than_or_equal_to
   public :: greater_than
   public :: greater_than_or_equal_to

   type, extends(RelationalMatcher) :: Real128Matcher
   contains
      procedure, nopass :: less
   end type Real128Matcher

   interface less_than
      module procedure :: less_than_real128
   end interface less_than

   interface less_than_or_equal_to
      module procedure :: less_than_or_equal_to_real128
   end interface less_than_or_equal_to

   interface greater_than
      module procedure :: greater_than_real128
   end interface greater_than

   interface greater_than_or_equal_to
      module procedure :: greater_than_or_equal_to_real128
   end interface greater_than_or_equal_to
contains
   function less_than_real128(operand) result(matcher)
      type(Real128Matcher) :: matcher
      real(kind=REAL128), intent(in) :: operand

      call matcher%super('<', operand)
   end function less_than_real128

   function less_than_or_equal_to_real128(operand) result(matcher)
      type(Real128Matcher) :: matcher
      real(kind=REAL128), intent(in) :: operand

      call matcher%super('<=', operand)
   end function less_than_or_equal_to_real128

   function greater_than_real128(operand) result(matcher)
      type(Real128Matcher) :: matcher
      real(kind=REAL128), intent(in) :: operand

      call matcher%super('>', operand)
   end function greater_than_real128

   function greater_than_or_equal_to_real128(operand) result(matcher)
      type(Real128Matcher) :: matcher
      real(kind=REAL128), intent(in) :: operand

      call matcher%super('>=', operand)
   end function greater_than_or_equal_to_real128

   recursive logical function less(actual, operand)
      class(*), intent(in) :: actual
      class(*), intent(in) :: operand

      select type(o => operand)
      type is(real(kind=REAL128))
         select type(a => actual)
         type is(real(kind=REAL128))
            less = (a < o)
         class default
            less = .false.
         end select
      class default
         less = .false.
      end select
   end function less
end module pf_Real128RelationalMatcher