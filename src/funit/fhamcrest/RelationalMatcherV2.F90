module pf_RelationalMatcherV2
   use iso_fortran_env
   use pf_BaseMatcher
   use pf_MatcherDescription
   implicit none
   private

   public :: less_than
   public :: less_than_or_equal_to
   public :: greater_than
   public :: greater_than_or_equal_to

   integer, parameter :: less_than_value    = -1
   integer, parameter :: equal_to_value     = 0
   integer, parameter :: greater_than_value = 1

   type, extends(BaseMatcher) :: RelationalMatcher
      private
      class(*), allocatable :: expected_value

      character(:), allocatable :: relation_description
      character(:), allocatable :: mismatch_msg

      integer :: min_compare
      integer :: max_compare
   contains
      procedure :: super
      procedure :: describe_to
      procedure :: describe_mismatch
      procedure :: describe_numeric_mismatch
      procedure :: matches

      procedure :: compare_int32
      procedure :: compare_int64
      procedure :: compare_real32
      procedure :: compare_real64
      procedure :: compare_real128
   end type RelationalMatcher

   interface signum
      module procedure :: signum_int32
      module procedure :: signum_int64
      module procedure :: signum_real32
      module procedure :: signum_real64
      module procedure :: signum_real128
   end interface signum
contains
   function less_than(operand) result(matcher)
      type(RelationalMatcher) :: matcher
      class(*), intent(in) :: operand

      call matcher%super('<', operand)
   end function less_than

   function less_than_or_equal_to(operand) result(matcher)
      type(RelationalMatcher) :: matcher
      class(*), intent(in) :: operand

      call matcher%super('<=', operand)
   end function less_than_or_equal_to

   function greater_than(operand) result(matcher)
      type(RelationalMatcher) :: matcher
      class(*), intent(in) :: operand

      call matcher%super('>', operand)
   end function greater_than

   function greater_than_or_equal_to(operand) result(matcher)
      type(RelationalMatcher) :: matcher
      class(*), intent(in) :: operand

      call matcher%super('>=', operand)
   end function greater_than_or_equal_to

   subroutine super(this, operation, expected_value)
      class(RelationalMatcher), intent(inout) :: this
      character(*), intent(in) :: operation
      class(*), intent(in) :: expected_value

      this%expected_value = expected_value

      select case(operation)
      case('<')
         this%relation_description = 'strictly less than '
         this%mismatch_msg = ' is greater than or equal to '

         this%min_compare = less_than_value
         this%max_compare = less_than_value
      case('<=')
         this%relation_description = 'less than or equal to '
         this%mismatch_msg = ' is strictly greater than '

         this%min_compare = less_than_value
         this%max_compare = equal_to_value
      case('>')
         this%relation_description = 'strictly greater than '
         this%mismatch_msg = ' is less than or equal to '

         this%min_compare = greater_than_value
         this%max_compare = greater_than_value
      case('>=')
         this%relation_description = 'greater than or equal to '
         this%mismatch_msg = ' is strictly less than '

         this%min_compare = equal_to_value
         this%max_compare = greater_than_value
      case default
         error stop
      end select
   end subroutine super

   subroutine describe_to(this, description)
      class(RelationalMatcher), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      call description%append_text("integer or real value ")
      call description%append_text(this%relation_description)
      call description%append_value(this%expected_value)
   end subroutine describe_to

   subroutine describe_mismatch(this, actual, description)
      class(RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual
      class(MatcherDescription), intent(inout) :: description

      select type (actual)
      type is (integer(kind=INT32))
         call this%describe_numeric_mismatch(actual, description)
      type is (integer(kind=INT64))
         call this%describe_numeric_mismatch(actual, description)
      type is (real(kind=REAL32))
         call this%describe_numeric_mismatch(actual, description)
      type is (real(kind=REAL64))
         call this%describe_numeric_mismatch(actual, description)
      class default
         call description%append_value(actual)
         call description%append_text(" is not integer or real")
      end select
   end subroutine describe_mismatch

   subroutine describe_numeric_mismatch(this, actual, description)
      class(RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual
      class(MatcherDescription), intent(inout) :: description

      call description%append_value(actual)
      call description%append_text(this%mismatch_msg)
      call description%append_value(this%expected_value)
   end subroutine describe_numeric_mismatch

   integer function signum_int32(x)
      integer(kind=INT32), intent(in) :: x

      signum_int32 = merge(1, merge(0, -1, x==0), x > 0)
   end function signum_int32

   integer function signum_int64(x)
      integer(kind=INT64), intent(in) :: x

      signum_int64 = merge(1, merge(0, -1, x==0), x > 0)
   end function signum_int64

   integer function signum_real32(x)
      real(kind=REAL32), intent(in) :: x

      signum_real32 = merge(1, merge(0, -1, x==0), x > 0)
   end function signum_real32

   integer function signum_real64(x)
      real(kind=REAL64), intent(in) :: x

      signum_real64 = merge(1, merge(0, -1, x==0), x > 0)
   end function signum_real64

   integer function signum_real128(x)
      real(kind=REAL128), intent(in) :: x

      signum_real128 = merge(1, merge(0, -1, x==0), x > 0)
   end function signum_real128

   recursive logical function compare_int32(this, actual, expected)
      class(RelationalMatcher), intent(in) :: this
      integer(kind=INT32), intent(in) :: actual
      integer(kind=INT32), intent(in) :: expected

      integer :: compare

      compare = signum(actual - expected)
      compare_int32 = ((this%min_compare <= compare) .and. (compare <= this%max_compare))
   end function compare_int32

   recursive logical function compare_int64(this, actual, expected)
      class(RelationalMatcher), intent(in) :: this
      integer(kind=INT64), intent(in) :: actual
      integer(kind=INT64), intent(in) :: expected

      integer :: compare

      compare = signum(actual - expected)
      compare_int64 = ((this%min_compare <= compare) .and. (compare <= this%max_compare))
   end function compare_int64

   recursive logical function compare_real32(this, actual, expected)
      class(RelationalMatcher), intent(in) :: this
      real(kind=REAL32), intent(in) :: actual
      real(kind=REAL32), intent(in) :: expected

      real :: compare

      compare = signum(actual - expected)
      compare_real32 = ((this%min_compare <= compare) .and. (compare <= this%max_compare))
   end function compare_real32

   recursive logical function compare_real64(this, actual, expected)
      class(RelationalMatcher), intent(in) :: this
      real(kind=REAL64), intent(in) :: actual
      real(kind=REAL64), intent(in) :: expected

      real :: compare

      compare = signum(actual - expected)
      compare_real64 = ((this%min_compare <= compare) .and. (compare <= this%max_compare))
   end function compare_real64

   recursive logical function compare_real128(this, actual, expected)
      class(RelationalMatcher), intent(in) :: this
      real(kind=REAL128), intent(in) :: actual
      real(kind=REAL128), intent(in) :: expected

      real :: compare

      compare = signum(actual - expected)
      compare_real128 = ((this%min_compare <= compare) .and. (compare <= this%max_compare))
   end function compare_real128

   recursive logical function matches(this, actual_value)
      class(RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual_value

      logical :: result

      select type(e => this%expected_value)
      type is (integer(kind=INT32))
         select type(a => actual_value)
         type is (integer(kind=INT32))
            matches = this%compare_int32(a, e)
         class default
            matches = .false.
         end select
      type is (integer(kind=INT64))
         select type(a => actual_value)
         type is (integer(kind=INT64))
            matches = this%compare_int64(a, e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL32))
         select type(a => actual_value)
         type is (real(kind=REAL32))
            matches = this%compare_real32(a, e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL64))
         select type(a => actual_value)
         type is (real(kind=REAL64))
            matches = this%compare_real64(a, e)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL128))
         select type(a => actual_value)
         type is (real(kind=REAL128))
            matches = this%compare_real128(a, e)
         class default
            matches = .false.
         end select
      class default
         matches = .false.
      end select
   end function matches
end module pf_RelationalMatcherV2