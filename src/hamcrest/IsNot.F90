module pf_IsNot_mod
  use pf_AbstractMatcher_mod
  use pf_MatcherDescription_mod
  use pf_IsEqual_mod
  implicit none
  private

  public :: IsNot
  public :: not

  type, extends(AbstractMatcher) :: IsNot
     private
     class(AbstractMatcher), allocatable :: matcher
   contains
     procedure :: matches
     procedure :: describe_to
     procedure :: describe_mismatch
  end type IsNot


  interface not
     module procedure not_matcher
     module procedure not_value
  end interface not

  interface IsNot
     module procedure not_matcher
  end interface IsNot


contains


  function not_matcher(matcher) result(not)
    type (IsNot) :: not
    class(AbstractMatcher), intent(in) :: matcher
    not%matcher = matcher
  end function not_matcher

  function not_value(value) result(not)
    type (IsNot) :: not
    type (Integer), intent(in) :: value
    not%matcher = equal_to(value)
  end function not_value


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

    call this%matcher%describe_mismatch(actual, description)

  end subroutine describe_mismatch


end module pf_IsNot_mod
