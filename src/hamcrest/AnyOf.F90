module pf_AnyOf_mod
  use pf_AbstractMatcher_mod
  use pf_BaseMatcher_mod
  use pf_MatcherDescription_mod
  implicit none
  private

  public :: AnyOf
  public :: any_of

  type, extends(BaseMatcher) :: AnyOf
     class(AbstractMatcher), allocatable :: matchers(:)
   contains
     procedure :: matches
     procedure :: describe_to
     procedure :: describe_to_op
     procedure :: describe_mismatch
  end type AnyOf


  interface any_of
     module procedure any_of
  end interface any_Of


contains


  function any_of(matchers)
    type (AnyOf) :: any_of
    class(AbstractMatcher), intent(in) :: matchers(:)

    allocate(any_of%matchers, source=matchers)

  end function any_of


  logical function matches(this, actual_value)
    class(AnyOf), intent(in) :: this
    class(*), intent(in) :: actual_value

    integer :: i
    
    matches = .false.
    do i = 1, size(this%matchers)
       if (this%matchers(i)%matches(actual_value)) then
          matches = .true.
          exit
       end if
    end do

  end function matches

  subroutine describe_to(this, description)
    class(AnyOf), intent(in):: this
    class(MatcherDescription), intent(inout) :: description

    integer :: i

    call this%describe_to_op(description, "or")

  end subroutine describe_to

  subroutine describe_to_op(this, description, operator)
    class(AnyOf), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description
    character(*), intent(in) :: operator

    call description%append_list("(", " " // operator // " ", ")", this%matchers)
  end subroutine describe_to_op
  

  subroutine describe_mismatch(this, actual, description)
    class(AnyOf), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    call description%append_text("was ")
    call description%append_value(actual)

  end subroutine describe_mismatch


end module pf_AnyOf_mod
