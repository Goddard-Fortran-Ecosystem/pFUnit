module pf_IsEqual_mod
  use iso_fortran_env
  use pf_AbstractMatcher_mod
  use pf_MatcherDescription_mod
  implicit none
  private

  public :: IsEqual
  public :: equal_to

  type, extends(AbstractMatcher) :: IsEqual
     private
     class(*), allocatable :: expected_value
   contains
     procedure :: matches
     procedure :: describe_to
     procedure :: describe_mismatch
  end type IsEqual

  interface equal_to
     module procedure :: equal_to_
  end interface equal_to

contains


  function equal_to_(operand) result(matcher)
    type (IsEqual) :: matcher
    class(*), intent(in) :: operand

    matcher%expected_value = operand

  end function equal_to_


  subroutine describe_to(this, description)
    class(IsEqual), intent(in):: this
    class(MatcherDescription), intent(inout) :: description
    call description%append_value(this%expected_value)
  end subroutine describe_to


  subroutine describe_mismatch(this, actual, description)
    class(IsEqual), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description
    call description%append_text("was ")
    call description%append_value(actual)
  end subroutine describe_mismatch
     

  ! Derived types are expected to have type-bound OPERATOR(==) that
  ! allows CLASS(*) on RHS.
  ! Intrinsics are checked case by case.
  logical function matches(this, actual_value)
    class(IsEqual), intent(in) :: this
    class(*), intent(in) :: actual_value

    select type (a => actual_value)
       ! Intrinsics and then derived types
    type is (logical)
       select type(e => this%expected_value)
       type is (logical)
          matches = (e .eqv. a)
       class default
          matches = .false.
       end select
    type is (integer(kind=INT32))
       select type(e => this%expected_value)
       type is (integer(kind=INT32))
          matches = (e == a)
       class default
          matches = .false.
       end select
    type is (integer(kind=INT64))
       select type(e => this%expected_value)
       type is (integer(kind=INT64))
          matches = (e == a)
       class default
          matches = .false.
       end select
    type is (real(kind=REAL32))
       select type(e => this%expected_value)
       type is (real(kind=REAL32))
          matches = (e == a)
       class default
          matches = .false.
       end select
    type is (real(kind=REAL64))
       select type(e => this%expected_value)
       type is (real(kind=REAL64))
          matches = (e == a)
       class default
          matches = .false.
       end select
    type is (real(kind=REAL128))
       select type(e => this%expected_value)
       type is (real(kind=REAL128))
          matches = (e == a)
       class default
          matches = .false.
       end select
    type is (complex(kind=REAL32))
       select type(e => this%expected_value)
       type is (complex(kind=REAL32))
          matches = (e == a)
       class default
          matches = .false.
       end select
    type is (complex(kind=REAL64))
       select type(e => this%expected_value)
       type is (complex(kind=REAL64))
          matches = (e == a)
       class default
          matches = .false.
       end select
    type is (complex(kind=REAL128))
       select type(e => this%expected_value)
       type is (complex(kind=REAL128))
          matches = (e == a)
       class default
          matches = .false.
       end select
    type is (character(*))
       select type(e => this%expected_value)
       type is (character(*))
          matches = (e == a)
       class default
          matches = .false.
       end select
    class default
!!$       matches = (e == actual_value)
    end select

  end function matches

end module pf_IsEqual_mod
