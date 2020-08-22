module pf_RelationalMatcher
   use iso_fortran_env
   use pf_BaseMatcher
   use pf_MatcherDescription
   implicit none
   private

   public :: RelationalMatcher

   type, abstract, extends(BaseMatcher) :: RelationalMatcher
      ! private
      class(*), allocatable :: expected_value

      character(:), allocatable :: relationship
      character(:), allocatable :: err_msg
   contains
      procedure :: super ! a hack
      procedure :: describe_to
      procedure :: describe_mismatch
      procedure :: describe_numeric_mismatch

      procedure :: matches

      procedure(i_relation_int32), deferred :: relation_int32
      procedure(i_relation_int64), deferred :: relation_int64
      procedure(i_relation_real32), deferred :: relation_real32
      procedure(i_relation_real64), deferred :: relation_real64
      procedure(i_relation_real128), deferred :: relation_real128
   end type RelationalMatcher

   abstract interface
      recursive logical function i_relation_int32(this, expected, actual)
         use, intrinsic :: iso_fortran_env, only: INT32
         import RelationalMatcher

         class(RelationalMatcher), intent(in) :: this
         integer(kind=INT32), intent(in) :: expected
         integer(kind=INT32), intent(in) :: actual
      end function i_relation_int32

      recursive logical function i_relation_int64(this, expected, actual)
         use, intrinsic :: iso_fortran_env, only: INT64
         import RelationalMatcher

         class(RelationalMatcher), intent(in) :: this
         integer(kind=INT64), intent(in) :: expected
         integer(kind=INT64), intent(in) :: actual
      end function i_relation_int64

      recursive logical function i_relation_real32(this, expected, actual)
         use, intrinsic :: iso_fortran_env, only: REAL32
         import RelationalMatcher

         class(RelationalMatcher), intent(in) :: this
         real(kind=REAL32), intent(in) :: expected
         real(kind=REAL32), intent(in) :: actual
      end function i_relation_real32

      recursive logical function i_relation_real64(this, expected, actual)
         use, intrinsic :: iso_fortran_env, only: REAL64
         import RelationalMatcher

         class(RelationalMatcher), intent(in) :: this
         real(kind=REAL64), intent(in) :: expected
         real(kind=REAL64), intent(in) :: actual
      end function i_relation_real64

      recursive logical function i_relation_real128(this, expected, actual)
         use, intrinsic :: iso_fortran_env, only: REAL128
         import RelationalMatcher

         class(RelationalMatcher), intent(in) :: this
         real(kind=REAL128), intent(in) :: expected
         real(kind=REAL128), intent(in) :: actual
      end function i_relation_real128
   end interface

contains
   subroutine super(this, expected_value, relationship, err_msg)
      class(RelationalMatcher), intent(inout) :: this
      class(*), intent(in) :: expected_value
      character(*), intent(in) :: relationship
      character(*), intent(in) :: err_msg

      this%expected_value = expected_value
      this%relationship = relationship
      this%err_msg = err_msg
   end subroutine super

   subroutine describe_to(this, description)
      class(RelationalMatcher), intent(in) :: this
      class(MatcherDescription), intent(inout) :: description

      call description%append_text("integer or real value ")
      call description%append_text(this%relationship)
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
      call description%append_text(this%err_msg)
      call description%append_value(this%expected_value)
   end subroutine describe_numeric_mismatch

   recursive logical function matches(this, actual_value)
      class(RelationalMatcher), intent(in) :: this
      class(*), intent(in) :: actual_value

      logical :: result

      select type(e => this%expected_value)
      type is (integer(kind=INT32))
         select type(a => actual_value)
         type is (integer(kind=INT32))
            matches = this%relation_int32(e, a)
         class default
            matches = .false.
         end select
      type is (integer(kind=INT64))
         select type(a => actual_value)
         type is (integer(kind=INT64))
            matches = this%relation_int64(e, a)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL32))
         select type(a => actual_value)
         type is (real(kind=REAL32))
            matches = this%relation_real32(e, a)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL64))
         select type(a => actual_value)
         type is (real(kind=REAL64))
            matches = this%relation_real64(e, a)
         class default
            matches = .false.
         end select
      type is (real(kind=REAL128))
         select type(a => actual_value)
         type is (real(kind=REAL128))
            matches = this%relation_real128(e, a)
         class default
            matches = .false.
         end select
      class default
         matches = .false.
      end select
   end function matches
end module pf_RelationalMatcher