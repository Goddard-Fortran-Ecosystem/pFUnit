module pf_Int64RelationalMatcher
   use iso_fortran_env
   use pf_RelationalMatcherV3
   implicit none
   private

   public :: less_than
   public :: less_than_or_equal_to
   public :: greater_than
   public :: greater_than_or_equal_to

   public :: Int64RelationalMatcher

   type, extends(RelationalMatcher) :: Int64RelationalMatcher
   contains
      procedure, nopass :: less
   end type Int64RelationalMatcher

   interface less_than
      module procedure :: less_than_int64
   end interface less_than

   interface less_than_or_equal_to
      module procedure :: less_than_or_equal_to_int64
   end interface less_than_or_equal_to

   interface greater_than
      module procedure :: greater_than_int64
   end interface greater_than

   interface greater_than_or_equal_to
      module procedure :: greater_than_or_equal_to_int64
   end interface greater_than_or_equal_to
contains
   function less_than_int64(operand) result(matcher)
      type(Int64RelationalMatcher) :: matcher
      integer(kind=INT64), intent(in) :: operand

      call matcher%super('<', operand)
   end function less_than_int64

   function less_than_or_equal_to_int64(operand) result(matcher)
      type(Int64RelationalMatcher) :: matcher
      integer(kind=INT64), intent(in) :: operand

      call matcher%super('<=', operand)
   end function less_than_or_equal_to_int64

   function greater_than_int64(operand) result(matcher)
      type(Int64RelationalMatcher) :: matcher
      integer(kind=INT64), intent(in) :: operand

      call matcher%super('>', operand)
   end function greater_than_int64

   function greater_than_or_equal_to_int64(operand) result(matcher)
      type(Int64RelationalMatcher) :: matcher
      integer(kind=INT64), intent(in) :: operand

      call matcher%super('>=', operand)
   end function greater_than_or_equal_to_int64

   recursive logical function less(actual, operand)
      class(*), intent(in) :: actual
      class(*), intent(in) :: operand

      select type(o => operand)
      type is(integer(kind=INT64))
         select type(a => actual)
         type is(integer(kind=INT64))
            less = (a < o)
         class default
            less = .false.
         end select
      class default
         less = .false.
      end select
   end function less
end module pf_Int64RelationalMatcher