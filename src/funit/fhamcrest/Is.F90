module pf_Is
  use pf_AbstractMatcher
  use pf_BaseMatcher
  use pf_MatcherDescription
  use pf_IsEqual
  use, intrinsic :: iso_fortran_env
  implicit none
  private

  public :: Is

  type, extends(BaseMatcher) :: Is
     private
     class(AbstractMatcher), allocatable :: matcher
   contains
     procedure :: matches
     procedure :: describe_to
     procedure :: describe_mismatch
  end type Is


  interface is
     module procedure is_
  end interface is


contains



  function is_(value)
    type (Is) :: is_
    class(*), intent(in) :: value

    select type (value)
    class is (AbstractMatcher)
       is_%matcher = value
    class default
       is_%matcher = equal_to(value)
    end select

  end function is_


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


end module pf_Is
