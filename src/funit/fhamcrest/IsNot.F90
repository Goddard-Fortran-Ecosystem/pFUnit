#include "unused_dummy.fh"

module pf_IsNot
  use pf_AbstractMatcher
  use pf_BaseMatcher
  use pf_MatcherDescription
  use pf_IsEqual
  use, intrinsic :: iso_fortran_env
  implicit none
  private

  public :: IsNot
  public :: not

  type, extends(BaseMatcher) :: IsNot
     private
     class(AbstractMatcher), allocatable :: matcher
   contains
     procedure :: matches
     procedure :: describe_to
     procedure :: describe_mismatch
  end type IsNot


  interface not
     module procedure not_
  end interface not


contains


  function not_(value)
    type (IsNot) :: not_
    class(*), intent(in) :: value

    select type (value)
    class is (AbstractMatcher)
       not_%matcher = value
    class default
       not_%matcher = equal_to(value)
    end select

  end function not_


  logical function matches(this, actual_value)
    class(IsNot), intent(in) :: this
    class(*), intent(in) :: actual_value

    matches = .not. this%matcher%matches(actual_value)

  end function matches

  subroutine describe_to(this, description)
    class(IsNot), intent(in):: this
    class(MatcherDescription), intent(inout) :: description

    call description%append_text("not ")
    call description%append_description_of(this%matcher)

  end subroutine describe_to

  subroutine describe_mismatch(this, actual, description)
    class(IsNot), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    _UNUSED_DUMMY(this)

    call description%append_text("was ")
    call description%append_value(actual)

  end subroutine describe_mismatch


end module pf_IsNot
