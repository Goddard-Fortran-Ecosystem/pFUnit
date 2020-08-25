module pf_CharRelationalMatcher
   use iso_fortran_env
   use pf_RelationalMatcherV3
   implicit none
   private

   public :: less_than
   public :: less_than_or_equal_to
   public :: greater_than
   public :: greater_than_or_equal_to

   public :: CharRelationalMatcher

   type, extends(RelationalMatcher) :: CharRelationalMatcher
   contains
      procedure, nopass :: less
   end type CharRelationalMatcher

   interface less_than
      module procedure :: less_thanchar
   end interface less_than

   interface less_than_or_equal_to
      module procedure :: less_than_or_equal_tochar
   end interface less_than_or_equal_to

   interface greater_than
      module procedure :: greater_thanchar
   end interface greater_than

   interface greater_than_or_equal_to
      module procedure :: greater_than_or_equal_tochar
   end interface greater_than_or_equal_to
contains
   function less_thanchar(operand) result(matcher)
      type(CharRelationalMatcher) :: matcher
      character(*), intent(in) :: operand

      call matcher%super('<', operand)
   end function less_thanchar

   function less_than_or_equal_tochar(operand) result(matcher)
      type(CharRelationalMatcher) :: matcher
      character(*), intent(in) :: operand

      call matcher%super('<=', operand)
   end function less_than_or_equal_tochar

   function greater_thanchar(operand) result(matcher)
      type(CharRelationalMatcher) :: matcher
      character(*), intent(in) :: operand

      call matcher%super('>', operand)
   end function greater_thanchar

   function greater_than_or_equal_tochar(operand) result(matcher)
      type(CharRelationalMatcher) :: matcher
      character(*), intent(in) :: operand

      call matcher%super('>=', operand)
   end function greater_than_or_equal_tochar

   recursive logical function less(actual, operand)
      class(*), intent(in) :: actual
      class(*), intent(in) :: operand

      select type(o => operand)
      type is(character(*))
         select type(a => actual)
         type is(character(*))
            less = (a < o)
         class default
            less = .false.
         end select
      class default
         less = .false.
      end select
   end function less
end module pf_CharRelationalMatcher