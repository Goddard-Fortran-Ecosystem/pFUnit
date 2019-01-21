module pf_SubstringMatcher_mod
  use pf_MatcherDescription_mod
  use pf_TypeSafeMatcher_String_mod
  implicit none
  private

  public :: SubstringMatcher

  type, abstract, extends(TypeSafeMatcher_String) :: SubstringMatcher
     private
     character(:), allocatable :: substring
     character(:), allocatable :: relationship
     logical :: ignoring_case = .false.
   contains
     procedure :: super ! a hack
     procedure :: get_substring
     procedure :: matches_safely
     procedure :: describe_to
     procedure :: describe_mismatch_safely
     procedure(eval_substring_of), deferred :: eval_substring_of
     procedure :: converted ! case converter
  end type SubstringMatcher

  abstract interface

     logical function eval_substring_of(this, item)
       import SubstringMatcher
       class(SubstringMatcher), intent(in) :: this
       character(*), intent(in) :: item
     end function eval_substring_of

  end interface

contains

  ! Cannot have constructor for abstract class.   But we need a mechanism
  ! for the subclasses to set fields.  Rather than multiple setters, which
  ! would be somewhat tedious here, a single setter that is suggestive
  ! of Java's "super" constructors.
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

  logical function matches_safely(this, item)
    class(SubstringMatcher), intent(in) :: this
    character(*), intent(in) :: item

    matches_safely = this%eval_substring_of(item)
  end function matches_safely

  subroutine describe_mismatch_safely(this, item, description)
    class(SubstringMatcher), intent(in) :: this
    character(*), intent(in) :: item
    class(MatcherDescription), intent(inout) :: description

    call description%append_text('was "')
    call description%append_text(item)
    call description%append_text('"')
    
  end subroutine describe_mismatch_safely

  
  function converted(this, string) result(converted_string)
    character(:), allocatable :: converted_string
    class(SubstringMatcher), intent(in) :: this
    character(*), intent(in) :: string

    if (this%ignoring_case) then
       converted_string = to_lower(string)
    else
       converted_string = string
    end if
  end function converted

  function to_lower(string) result(converted_string)
    character(:), allocatable :: converted_string
    character(*), intent(in) :: string

    integer :: i, n, ascii
    integer :: delta


    delta = iachar('a') - iachar('A')
    n = len(string)
    allocate(character(len=n) :: converted_string)
    do i = 1, n
       ascii = iachar(string(i:i))
       if (ascii >= iachar('A') .and. ascii <= iachar('Z')) then
          converted_string(i:i) = achar(ascii + delta)
       else
          converted_string(i:i) = string(i:i)
       end if
    end do
  end function to_lower


  
end module pf_SubstringMatcher_mod
