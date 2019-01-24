module pf_TypeSafeMatcher
  use pf_AbstractMatcher
  use pf_BaseMatcher
  use pf_SelfDescribing
  use pf_MatcherDescription, only: MatcherDescription

  use pf_Array
  use iso_fortran_env
  implicit none
  private

  public :: TypeSafeMatcher

  type, abstract, extends(BaseMatcher) :: TypeSafeMatcher
   contains
     procedure(expects_type_of), deferred :: expects_type_of
     procedure(matches_safely), deferred :: matches_safely
     procedure(describe_mismatch_safely), deferred :: describe_mismatch_safely
     procedure :: matches
     procedure :: describe_mismatch
  end type TypeSafeMatcher

  abstract interface

     logical function expects_type_of(this, actual)
       import TypeSafeMatcher
       class(TypeSafeMatcher), intent(in) :: this
       class(*), intent(in) :: actual
     end function expects_type_of
     

     logical function matches_safely(this, actual_value)
       use iso_fortran_env
       import MatcherDescription
       import TypeSafeMatcher
       class(TypeSafeMatcher), intent(in) :: this
       class(*), intent(in) :: actual_value
     end function matches_safely


     subroutine describe_mismatch_safely(this, actual, description)
       use iso_fortran_env
       import MatcherDescription
       import TypeSafeMatcher
       class(TypeSafeMatcher), intent(in) :: this
       class(*), intent(in) :: actual
       class(MatcherDescription), intent(inout) :: description
     end subroutine describe_mismatch_safely

  end interface


contains


  logical function matches(this, actual_value)
    class(TypeSafeMatcher), intent(in) :: this
    class(*), intent(in) :: actual_value

    if (this%expects_type_of(actual_value)) then
       matches = this%matches_safely(actual_value)
    else
       matches = .false.
    end if

  end function matches
  
  subroutine describe_mismatch(this, actual, description)
    class(TypeSafeMatcher), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    character(:), allocatable :: type_name

    if (this%expects_type_of(actual)) then
       call this%describe_mismatch_safely(actual, description)
    else
       call description%append_text("was ")
       type_name = type_of(actual)
       call description%append_text(guess_a_or_an(type_name))
       call description%append_text(" ")
       call description%append_text(type_name)
       call description%append_text(" (")
       call description%append_value(actual)
       call description%append_text(")")
    end if
       
  end subroutine describe_mismatch

  function guess_a_or_an(string) result(noun_determiner)
    character(:), allocatable :: noun_determiner
    character(*), intent(in) :: string
    
    if (len(string) == 0) then
       noun_determiner = "a"
    else
       if (scan(string(1:1), 'aAeEiIoOuU') == 1 ) then
          noun_determiner = "an"
       else
          noun_determiner = "a"
       end if
    end if
  end function guess_a_or_an
  
  recursive function type_of(actual) result(type_name)
    character(:), allocatable :: type_name
    class(*), intent(in) :: actual

    character(16) :: buffer
    character(:), allocatable :: item_type

     select type (actual)
     type is (character(*))
        write(buffer,'(i0)') len(actual)
        type_name = "character(" // trim(buffer) // ")"
     type is (logical)
        type_name = "logical"
     type is (integer)
        type_name = "integer"
#if (defined(_ISO_INT32) && (_ISO_INT32 != _INT_DEFAULT_KIND))
     type is (integer(kind=INT32))
        type_name = "integer(kind=INT32)"
#endif
#if (defined(_ISO_INT64) && (_ISO_INT64 != _INT_DEFAULT_KIND))
     type is (integer(kind=INT64))
        type_name = "integer(kind=INT64)"
#endif
     type is (real)
        type_name = "real"
     type is (real(kind(1.0d0)))
        type_name = "double precision"
#if (defined(_ISO_REAL32) && (_ISO_REAL32 != _REAL_DEFAULT_KIND) && (_ISO_REAL32 != _DOUBLE_DEFAULT_KIND))
     type is (real(kind=REAL32))
        type_name = "real(kind=REAL32)"
#endif
#if (defined(_ISO_REAL64) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL64 != _DOUBLE_DEFAULT_KIND))
     type is (real(kind=REAL64))
        type_name = "real(kind=REAL64)"
#endif
#if (defined(_ISO_REAL128) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL128 != _DOUBLE_DEFAULT_KIND))
     type is (real(kind=REAL128))
        type_name = "real(kind=REAL128)"
#endif
     type is (complex)
        type_name = "complex"
     type is (complex(kind=kind(1.d0)))
        type_name = "double complex"
#if (defined(_ISO_REAL32) && (_ISO_REAL32 != _REAL_DEFAULT_KIND) && (_ISO_REAL32 != _DOUBLE_DEFAULT_KIND))
     type is (complex(kind=REAL32))
        type_name = "complex(kind=REAL32)"
#endif
#if (defined(_ISO_REAL64) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL64 != _DOUBLE_DEFAULT_KIND))
     type is (complex(kind=REAL64))
        type_name = "complex(kind=REAL64)"
#endif
#if (defined(_ISO_REAL128) && (_ISO_REAL128 != _REAL_DEFAULT_KIND) && (_ISO_REAL128 != _DOUBLE_DEFAULT_KIND))
     type is (complex(kind=REAL128))
        type_name = "complex(kind=REAL128)"
#endif
     class is (SelfDescribing)
        type_name = actual%get_type_name()
     class is (internal_array_1d)
        if (size(actual%items) == 0) then ! no type
           type_name = "1-D array of zero size"
           item_type = type_of(actual%items(1))
           type_name = "1-D array of type(" // item_type // ")"
        end if
     class default
        ERROR STOP __FILE__ // ' :: unknown type'
     end select
   end function type_of

end module pf_TypeSafeMatcher
