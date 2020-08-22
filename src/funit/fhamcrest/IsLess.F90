module pf_IsLess
   use iso_fortran_env
   use pf_RelationalMatcher
   implicit none
   private

   public :: IsLess
   public :: less_than

   type, extends(RelationalMatcher) :: IsLess
   contains
      procedure :: relation_int32
      procedure :: relation_int64
      procedure :: relation_real32
      procedure :: relation_real64
      procedure :: relation_real128
   end type IsLess

contains
   function less_than(operand) result(matcher)
      type(IsLess) :: matcher
      class(*), intent(in) :: operand

      call matcher%super(operand, "strictly less than ", " is greater than or equal to ")
   end function less_than

   recursive logical function relation_int32(this, expected, actual)
      class(IsLess), intent(in) :: this
      integer(kind=INT32), intent(in) :: expected
      integer(kind=INT32), intent(in) :: actual

      relation_int32 = (actual < expected)
   end function relation_int32

   recursive logical function relation_int64(this, expected, actual)
      class(IsLess), intent(in) :: this
      integer(kind=INT64), intent(in) :: expected
      integer(kind=INT64), intent(in) :: actual

      relation_int64 = (actual < expected)
   end function relation_int64

   recursive logical function relation_real32(this, expected, actual)
      class(IsLess), intent(in) :: this
      real(kind=REAL32), intent(in) :: expected
      real(kind=REAL32), intent(in) :: actual

      relation_real32 = (actual < expected)
   end function relation_real32

   recursive logical function relation_real64(this, expected, actual)
      class(IsLess), intent(in) :: this
      real(kind=REAL64), intent(in) :: expected
      real(kind=REAL64), intent(in) :: actual

      relation_real64 = (actual < expected)
   end function relation_real64

   recursive logical function relation_real128(this, expected, actual)
      class(IsLess), intent(in) :: this
      real(kind=REAL128), intent(in) :: expected
      real(kind=REAL128), intent(in) :: actual

      relation_real128 = (actual < expected)
   end function relation_real128
end module pf_IsLess