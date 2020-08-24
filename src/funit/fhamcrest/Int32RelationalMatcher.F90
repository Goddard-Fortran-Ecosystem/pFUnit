module pf_Int32RelationalMatcher
   use pf_RelationalMatcher
   implicit none
   private


   type :: Int32RelationalMatcher
      private
      integer(kind=INT32) :: operand
   contains
      procedure, nopass :: less
      procedure :: matches_safely
      procedure :: expects_type_of
   end type Int32RelationalMatcher

   interface less_than
      module procedure new_Int32RelationalMatcher
   end interface less_than

contains

   function new_Int32RelationalMatcher(operand) result(matcher)
      type(Int32RelationalMatcher) :: matcher
      integer(kind=INT32), intent(in) :: operand

      matcher%operand = operand
      call matcher%super('<', description_of(operand))

   end function new_Int32RelationalMatcher


   logical function less(a, b)
      class(*), intent(in) :: a
      class(*), intent(in) :: b

      select type (a)
      type is (integer(kind=INT32))
         select type (b)
         type is (integer(kind=INT32))
            less = a < b
         end select
      end select

   end function less

   logical function expects_type_of(this, actual)
      class(Int32RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual

      select type (actual)
      type is (integer(kind=INT32)
         expects_type_of = .true.
      class default
         expects_type_of = .false.
      end select

   end function expects_type_of
end module pf_Int32RelationalMatcher
