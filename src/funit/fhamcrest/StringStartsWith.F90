module pf_StringStartsWith
  use pf_AbstractMatcher
  use pf_MatcherDescription
  use pf_SubstringMatcher
  implicit none
  private

  public :: StringStartsWith
  public :: starts_with

  type, extends(SubstringMatcher) :: StringStartsWith
     private
   contains
     procedure :: eval_substring_of
  end type StringStartsWith

  interface starts_with
     module procedure starts_with_
  end interface starts_with

contains

  function starts_with_(substring, ignoring_case) result(matcher)
    type(StringStartsWith) :: matcher
    character(*), intent(in) :: substring
    logical, optional, intent(in) :: ignoring_case

    call matcher%super("starting with", substring, ignoring_case)

  end function starts_with_


  logical function eval_substring_of(this, item)
    class(StringStartsWith), intent(in) :: this
    character(*), intent(in) :: item
    eval_substring_of = index(this%converted(item), this%converted(this%get_substring())) == 1
  end function eval_substring_of

end module pf_StringStartsWith
