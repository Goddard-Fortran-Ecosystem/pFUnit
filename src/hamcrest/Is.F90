module pf_Is_mod
  use pf_AbstractMatcher_mod
  use pf_MatcherDescription_mod
  use pf_IsEqual_mod
  implicit none
  private

  public :: Is

  type, extends(AbstractMatcher) :: Is
     private
     class(AbstractMatcher), allocatable :: matcher
   contains
     procedure :: matches
     procedure :: describe_to
     procedure :: describe_mismatch
  end type Is


  interface is
     module procedure is_matcher
     module procedure is_value
  end interface is


contains


  function is_matcher(matcher) result(is_)
    type (Is) :: is_
    class(AbstractMatcher), intent(in) :: matcher
    is_%matcher = matcher
  end function is_matcher

  function is_value(value) result(is_)
    type (Is) :: is_
    type(Integer) :: value
    is_%matcher = equal_to(value)
  end function is_value


  logical function matches(this, actual_value)
    class(Is), intent(in) :: this
    class(*), intent(in) :: actual_value

    matches = this%matcher%matches(actual_value)

  end function matches

  subroutine describe_to(this, description)
    class(Is), intent(in):: this
    class(MatcherDescription), intent(inout) :: description

    call description%append_text("is ")
    call description%append_description_of(this%matcher)

  end subroutine describe_to

  subroutine describe_mismatch(this, actual, description)
    class(Is), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    call this%matcher%describe_mismatch(actual, description)

  end subroutine describe_mismatch


end module pf_Is_mod
