#include "unused_dummy.fh"

module pf_IsEqual
  use iso_fortran_env
  use pf_AbstractMatcher
  use pf_BaseMatcher
  use pf_MatcherDescription
  use pf_Array
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

     procedure :: matches_list
     procedure :: matches_intrinsic
  end type IsEqual

  interface equal_to
     module procedure :: equal_to_scalar
     module procedure :: equal_to_array_1d
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

    matcher%expected_value = wrap_array(operand)

  end function equal_to_array_1d


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
    type is (internal_array_1d)
       matches = this%matches_list(e%items, actual_value)
    class is (Matchable)
       matches = (e == actual_value)
    class default ! intrinsics
       matches = this%matches_intrinsic(actual_value)
    end select
       
  end function matches


  logical function matches_list(this, expected_items, actual_value)
    class(IsEqual), intent(in) :: this
    class(*), intent(in) :: expected_items(:)
    class(*), intent(in) :: actual_value

    integer :: i, n_items
    type (IsEqual) :: m

    _UNUSED_DUMMY(this)

    select type (a => actual_value)
    type is (internal_array_1d)
       n_items = size(expected_items)
       if (size(a%items) == n_items) then
          do i = 1, n_items
             m = equal_to(expected_items(i))
             if (.not. m%matches(a%items(i))) then
                matches_list = .false.
                return
             end if
          end do
          matches_list = .true.
       else
          matches_list = .false. ! differing number of elements
       end if
    end select

  end function matches_list



  logical function matches_intrinsic(this, actual_value)
    class(IsEqual), intent(in) :: this
    class(*), target, intent(in) :: actual_value

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
    type is (real(kind=REAL32))
       select type(a => actual_value)
       type is (real(kind=REAL32))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    type is (real(kind=REAL64))
       select type(a => actual_value)
       type is (real(kind=REAL64))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    type is (real(kind=REAL128))
       select type(a => actual_value)
       type is (real(kind=REAL128))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    type is (complex(kind=REAL32))
       select type(a => actual_value)
       type is (complex(kind=REAL32))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    type is (complex(kind=REAL64))
       select type(a => actual_value)
       type is (complex(kind=REAL64))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    type is (complex(kind=REAL128))
       select type(a => actual_value)
       type is (complex(kind=REAL128))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    type is (character(*))
       select type(a => actual_value)
       type is (character(*))
          matches_intrinsic = (e == a)
       class default
          matches_intrinsic = .false.
       end select
    class default
       matches_intrinsic = .false. ! unsupported intrinsic?
    end select

  end function matches_intrinsic

end module pf_IsEqual
