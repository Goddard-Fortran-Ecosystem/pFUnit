#include "unused_dummy.fh"

module pf_SubstringMatcher
  use pf_MatcherDescription
  use pf_AbstractMatcher
  use pf_TypeSafeMatcher
  implicit none
  private

  public :: SubstringMatcher

  type, abstract, extends(TypeSafeMatcher) :: SubstringMatcher
     private
     character(:), allocatable :: substring
     character(:), allocatable :: relationship
     logical :: ignoring_case = .false.
   contains
     procedure :: super ! a hack
     procedure :: get_substring
     procedure :: describe_to
     procedure :: matches_safely
     procedure :: describe_mismatch_safely
     procedure(eval_substring_of), deferred :: eval_substring_of
     procedure :: converted ! case converter
     procedure :: expects_type_of
  end type SubstringMatcher

  abstract interface

     logical function eval_substring_of(this, item)
       import SubstringMatcher
       class(SubstringMatcher), intent(in) :: this
       character(*), intent(in) :: item
     end function eval_substring_of

  end interface

contains

  ! Cannot have constructor for abstract class.  But we need a
  ! mechanism for the subclasses to set fields.  Rather than multiple
  ! setters, which would be somewhat tedious here, a single setter
  ! that is suggestive of Java's "super" constructors.
  subroutine super(this, relationship, substring, ignoring_case)
    class(SubstringMatcher), intent(inout) :: this
    character(*), intent(in) :: relationship
    character(*), intent(in) :: substring
    logical, optional, intent(in) :: ignoring_case

    this%relationship = relationship
    this%substring = substring
    if (present(ignoring_case)) then
       this%ignoring_case = ignoring_case
    else
       this%ignoring_case = .false.
    end if

  end subroutine super

  function get_substring(this) result(substring)
    class(SubstringMatcher), intent(in) :: this
    character(:), allocatable :: substring
    substring = this%substring
  end function get_substring

  subroutine describe_to(this, description)
    class(SubstringMatcher), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description

    call description%append_text("a string ")
    call description%append_text(this%relationship)
    call description%append_text(" ")
    call description%append_value(this%substring)
    if (this%ignoring_case) then
       call description%append_text(" ignoring case")
    end if
    
  end subroutine describe_to

  logical function matches_safely(this, actual_value)
    class(SubstringMatcher), intent(in) :: this
    class(*), intent(in) :: actual_value

    select type(actual_value)
    type is (character(*))
       matches_safely = this%eval_substring_of(actual_value)
    end select

  end function matches_safely



  subroutine describe_mismatch_safely(this, actual, description)
    class(SubstringMatcher), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    _UNUSED_DUMMY(this)
    
    call description%append_text('was "')
    select type (actual)
    type is (character(*))
       call description%append_text(actual)
    end select
    call description%append_text('"')
    
  end subroutine describe_mismatch_safely

  
  function converted(this, string) result(converted_string)
    use pf_StringUtilities, only: to_lower
    character(:), allocatable :: converted_string
    class(SubstringMatcher), intent(in) :: this
    character(*), intent(in) :: string

    if (this%ignoring_case) then
       converted_string = to_lower(string)
    else
       converted_string = string
    end if
  end function converted


  logical function expects_type_of(this, actual)
    class(SubstringMatcher), intent(in) :: this
    class(*), intent(in) :: actual

    _UNUSED_DUMMY(this)
    
    select type (actual)
    type is (character(*))
       expects_type_of = .true.
    class default
       expects_type_of = .false.
    end select

  end function expects_type_of

  
end module pf_SubstringMatcher
