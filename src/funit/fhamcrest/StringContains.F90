module pf_StringContains
  use pf_MatcherDescription
  use pf_SubstringMatcher
  implicit none
  private

  public :: StringContains
  public :: contains_string

  type, extends(SubstringMatcher) :: StringContains
     private
   contains
     procedure :: eval_substring_of
  end type StringContains


  interface contains_string
     module procedure contains_string_
  end interface contains_string

contains

  function contains_string_(substring, ignoring_case) result(matcher)
    type(StringContains) :: matcher
    character(*), intent(in) :: substring
    logical, optional, intent(in) :: ignoring_case

    call matcher%super("containing", substring, ignoring_case)

  end function contains_string_



  logical function eval_substring_of(this, item)
    class(StringContains), intent(in) :: this
    character(*), intent(in) :: item

    eval_substring_of = index(this%converted(item), this%converted(this%get_substring())) > 0
  end function eval_substring_of

end module pf_StringContains
