module pf_StringEndsWith
  use pf_AbstractMatcher
  use pf_MatcherDescription
  use pf_SubstringMatcher
  implicit none
  private

  public :: StringEndsWith
  public :: ends_with

  type, extends(SubstringMatcher) :: StringEndsWith
     private
   contains
     procedure :: eval_substring_of
  end type StringEndsWith

  interface ends_with
     module procedure ends_with_
  end interface ends_with

contains

  function ends_with_(substring, ignoring_case) result(matcher)
    type(StringEndsWith) :: matcher
    character(*), intent(in) :: substring
    logical, optional, intent(in) :: ignoring_case

    call matcher%super("ending with", substring, ignoring_case)

  end function ends_with_


  logical function eval_substring_of(this, item)
    class(StringEndsWith), intent(in) :: this
    character(*), intent(in) :: item

    integer :: idx_substring

    idx_substring = index(this%converted(item), this%converted(this%get_substring()),back=.true.)

    eval_substring_of = idx_substring == (len(item) - len(this%get_substring()) + 1)
  end function eval_substring_of

end module pf_StringEndsWith
