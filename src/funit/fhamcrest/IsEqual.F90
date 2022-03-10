#include "unused_dummy.fh"

module pf_IsEqual
  use iso_fortran_env
  use pf_AbstractMatcher
  use pf_BaseMatcher
  use pf_MatcherDescription
  use pf_AbstractArrayWrapper
  use pf_ArrayWrapper
  implicit none
  private

  public :: IsEqual
  public :: equal_to

  type, extends(BaseMatcher) :: IsEqual
     private
     class(*), allocatable :: expected_value
   contains
     procedure :: matches
     procedure :: describe_to
     procedure :: describe_mismatch

     procedure :: matches_array_1d
     procedure :: matches_array_2d
     procedure :: matches_array_3d
     procedure :: matches_intrinsic
  end type IsEqual

  interface equal_to
     module procedure :: equal_to_scalar
     module procedure :: equal_to_array_1d
     module procedure :: equal_to_array_2d
     module procedure :: equal_to_array_3d
  end interface equal_to

contains


  function equal_to_scalar(operand) result(matcher)
    type (IsEqual) :: matcher
    class(*), intent(in) :: operand

    matcher%expected_value = operand

  end function equal_to_scalar


  function equal_to_array_1d(operand) result(matcher)
    type (IsEqual) :: matcher
    class(*), intent(in) :: operand(:)

    matcher%expected_value = ArrayWrapper(operand)

  end function equal_to_array_1d

  function equal_to_array_2d(operand) result(matcher)
    type (IsEqual) :: matcher
    class(*), intent(in) :: operand(:,:)

    matcher%expected_value = ArrayWrapper(operand)

  end function equal_to_array_2d


  function equal_to_array_3d(operand) result(matcher)
    type (IsEqual) :: matcher
    class(*), intent(in) :: operand(:,:,:)

    matcher%expected_value = ArrayWrapper(operand)

  end function equal_to_array_3d


  subroutine describe_to(this, description)
    class(IsEqual), intent(in):: this
    class(MatcherDescription), intent(inout) :: description
    call description%append_value(this%expected_value)
  end subroutine describe_to


  subroutine describe_mismatch(this, actual, description)
    class(IsEqual), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    _UNUSED_DUMMY(this)

    call description%append_text("was ")
    call description%append_value(actual)
  end subroutine describe_mismatch
     

  ! Derived types are expected to have type-bound OPERATOR(==) that
  ! allows CLASS(*) on RHS.
  ! Intrinsics are checked case by case.
  recursive logical function matches(this, actual_value)
    use pf_Matchable
    class(IsEqual), intent(in) :: this
    class(*), intent(in) :: actual_value

    select type (e => this%expected_value)
    type is (ArrayWrapper_1d)
       matches = this%matches_array_1d(e%items, actual_value)
    type is (ArrayWrapper_2d)
       matches = this%matches_array_2d(e%items, actual_value)
    type is (ArrayWrapper_3d)
       matches = this%matches_array_3d(e%items, actual_value)
    class is (Matchable)
       matches = (e == actual_value)
    class default ! intrinsics
       matches = this%matches_intrinsic(actual_value)
    end select
       
  end function matches


  logical function matches_array_1d(this, expected_items, actual_value)
    class(IsEqual), intent(in) :: this
    class(*), intent(in) :: expected_items(:)
    class(*), intent(in) :: actual_value

    integer :: i, n_items
    type (IsEqual) :: m

    _UNUSED_DUMMY(this)

    select type (a => actual_value)
    type is (ArrayWrapper_1d)
       n_items = size(expected_items)
       if (size(a%items) == n_items) then
          do i = 1, n_items
             m = equal_to(expected_items(i))
             if (.not. m%matches(a%items(i))) then
                matches_array_1d = .false.
                return
             end if
          end do
          matches_array_1d = .true.
       else
          matches_array_1d = .false. ! differing number of elements
       end if
    class default
       matches_array_1d = .false. ! differing types
    end select

  end function matches_array_1d


  logical function matches_array_2d(this, expected_items, actual_value)
    class(IsEqual), intent(in) :: this
    class(*), intent(in) :: expected_items(:,:)
    class(*), intent(in) :: actual_value

    integer :: i, j
    type (IsEqual) :: m

    _UNUSED_DUMMY(this)

    select type (a => actual_value)
    type is (ArrayWrapper_2d)
       if (all(shape(a%items) == shape(expected_items))) then
          do j = 1, size(a%items,2)
             do i = 1, size(a%items,1)
                m = equal_to(expected_items(i,j))
                if (.not. m%matches(a%items(i,j))) then
                   matches_array_2d = .false.
                   return
                end if
             end do
          end do
          matches_array_2d = .true.
       else
          matches_array_2d = .false. ! differing shape/size
       end if
    class default
       matches_array_2d = .false. ! differing types
    end select

  end function matches_array_2d


  logical function matches_array_3d(this, expected_items, actual_value)
    class(IsEqual), intent(in) :: this
    class(*), intent(in) :: expected_items(:,:,:)
    class(*), intent(in) :: actual_value

    integer :: i, j, k
    type (IsEqual) :: m

    _UNUSED_DUMMY(this)

    select type (a => actual_value)
    type is (ArrayWrapper_3d)
       if (all(shape(a%items) == shape(expected_items))) then
          do k = 1, size(a%items,3)
             do j = 1, size(a%items,2)
                do i = 1, size(a%items,1)
                   m = equal_to(expected_items(i,j,k))
                   if (.not. m%matches(a%items(i,j,k))) then
                      matches_array_3d = .false.
                      return
                   end if
                end do
             end do
          end do
          matches_array_3d = .true.
       else
          matches_array_3d = .false. ! differing shape/size
       end if
    class default
       matches_array_3d = .false. ! differing types
    end select

  end function matches_array_3d



  logical function matches_intrinsic(this, actual_value)
    class(IsEqual), intent(in) :: this
    class(*), target, intent(in) :: actual_value

     integer, parameter :: DP = kind(1.d0)

    select type (e => this%expected_value)
    type is (logical)
       select type(a => actual_value)
       type is (logical)
          matches_intrinsic = (e .eqv. a)
       class default
          matches_intrinsic = .false.
       end select
    type is (integer(kind=INT32))
       select type(a => actual_value)
       type is (integer(kind=INT32))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    type is (integer(kind=INT64))
       select type(a => actual_value)
       type is (integer(kind=INT64))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    type is (real)
       select type(a => actual_value)
       type is (real)
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    type is (real(kind=DP))
       select type(a => actual_value)
       type is (real(kind=DP))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
#if (defined(_ISO_REAL32) && (_ISO_REAL32 != _REAL_DEFAULT_KIND) && (_ISO_REAL32 != _DOUBLE_DEFAULT_KIND))
    type is (real(kind=REAL32))
       select type(a => actual_value)
       type is (real(kind=REAL32))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
#endif
#if (defined(_ISO_REAL64) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL64 != _DOUBLE_DEFAULT_KIND))
    type is (real(kind=REAL64))
       select type(a => actual_value)
       type is (real(kind=REAL64))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
#endif
#if (defined(_ISO_REAL128) && (_ISO_REAL128 != _REAL_DEFAULT_KIND) && (_ISO_REAL128 != _DOUBLE_DEFAULT_KIND))
    type is (real(kind=REAL128))
       select type(a => actual_value)
       type is (real(kind=REAL128))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
#endif
    type is (complex)
       select type(a => actual_value)
       type is (complex)
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
     type is (complex(kind=DP))
       select type(a => actual_value)
       type is (complex(kind=DP))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
#if (defined(_ISO_REAL32) && (_ISO_REAL32 != _REAL_DEFAULT_KIND) && (_ISO_REAL32 != _DOUBLE_DEFAULT_KIND))
    type is (complex(kind=REAL32))
       select type(a => actual_value)
       type is (complex(kind=REAL32))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
#endif
#if (defined(_ISO_REAL64) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL64 != _DOUBLE_DEFAULT_KIND))
    type is (complex(kind=REAL64))
       select type(a => actual_value)
       type is (complex(kind=REAL64))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
#endif
#if (defined(_ISO_REAL128) && (_ISO_REAL128 != _REAL_DEFAULT_KIND) && (_ISO_REAL128 != _DOUBLE_DEFAULT_KIND))
    type is (complex(kind=REAL128))
       select type(a => actual_value)
       type is (complex(kind=REAL128))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
#endif
    type is (character(*))
       select type(a => actual_value)
       type is (character(*))
          ! Fortran pads strings of different lengths when comparing.
          ! But we want to test for strict equality.
          ! May add a PAD option later on
          matches_intrinsic = (len(e) == len(a))
          if (matches_intrinsic) matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    class default
       matches_intrinsic = .false. ! unsupported intrinsic?
    end select

  end function matches_intrinsic

end module pf_IsEqual
