module pf_DescribedAs
  use pf_AbstractMatcher
  use pf_MatcherDescription
  use pf_BaseMatcher
  implicit none
  private

  public :: DescribedAs
  public :: described_as

  type, extends(BaseMatcher) :: DescribedAs
     private
     character(:), allocatable :: text_description
     class(AbstractMatcher), allocatable :: submatcher
   contains
     procedure :: matches
     procedure :: describe_mismatch
     procedure :: describe_to
  end type DescribedAs


  interface described_as
     module procedure described_as_
  end interface described_as


contains


  ! Not a template as in Java hamcrest - at least for now
  function described_as_(text_description, submatcher) result(matcher)
    type(DescribedAs) :: matcher
    character(*), intent(in) :: text_description
    class(AbstractMatcher), intent(in) :: submatcher

    matcher%text_description = text_description
    matcher%submatcher = submatcher
    
  end function described_as_


  logical function matches(this, actual_value)
    class(DescribedAs), intent(in) :: this
    class(*), intent(in) :: actual_value

    matches = this%submatcher%matches(actual_value)

  end function matches

  subroutine describe_to(this, description)
    class(DescribedAs), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description

    call description%append_text(this%text_description)

  end subroutine describe_to


  subroutine describe_mismatch(this, actual, description)
    class(DescribedAs), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    call this%submatcher%describe_mismatch(actual, description)

  end subroutine describe_mismatch

end module pf_DescribedAs
