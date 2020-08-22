module pf_IsLessOrEqual
   use iso_fortran_env
   use pf_RelationalMatcher
   implicit none
   private

   public :: IsLessOrEqual
   public :: less_than_or_equal_to

   type, extends(RelationalMatcher) :: IsLessOrEqual
   contains
      procedure :: relation_int32
      procedure :: relation_int64
      procedure :: relation_real32
      procedure :: relation_real64
      procedure :: relation_real128
   end type IsLessOrEqual

contains
   function less_than_or_equal_to(operand) result(matcher)
      type(IsLessOrEqual) :: matcher
      class(*), intent(in) :: operand

      call matcher%super(operand, "less than or equal to ", " is strictly greater than ")
   end function less_than_or_equal_to

   recursive logical function relation_int32(this, expected, actual)
      class(IsLessOrEqual), intent(in) :: this
      integer(kind=INT32), intent(in) :: expected
      integer(kind=INT32), intent(in) :: actual

      relation_int32 = (actual <= expected)
   end function relation_int32

   recursive logical function relation_int64(this, expected, actual)
      class(IsLessOrEqual), intent(in) :: this
      integer(kind=INT64), intent(in) :: expected
      integer(kind=INT64), intent(in) :: actual

      relation_int64 = (actual <= expected)
   end function relation_int64

   recursive logical function relation_real32(this, expected, actual)
      class(IsLessOrEqual), intent(in) :: this
      real(kind=REAL32), intent(in) :: expected
      real(kind=REAL32), intent(in) :: actual

      relation_real32 = (actual <= expected)
   end function relation_real32

   recursive logical function relation_real64(this, expected, actual)
      class(IsLessOrEqual), intent(in) :: this
      real(kind=REAL64), intent(in) :: expected
      real(kind=REAL64), intent(in) :: actual

      relation_real64 = (actual <= expected)
   end function relation_real64

   recursive logical function relation_real128(this, expected, actual)
      class(IsLessOrEqual), intent(in) :: this
      real(kind=REAL128), intent(in) :: expected
      real(kind=REAL128), intent(in) :: actual

      relation_real128 = (actual <= expected)
   end function relation_real128
end module pf_IsLessOrEqual