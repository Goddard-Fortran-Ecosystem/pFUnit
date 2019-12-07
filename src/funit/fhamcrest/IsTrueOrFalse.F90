#include "unused_dummy.fh"

module pf_IsTrueOrFalse
  use pf_TypeSafeMatcher
  use pf_MatcherDescription
  implicit none
  private

  public :: IsTrueOrFalse
  public :: True
  public :: False

  type, extends(TypeSafeMatcher) :: IsTrueOrFalse
     private
     logical :: value
   contains
     procedure :: matches_safely
     procedure :: describe_mismatch_safely
     procedure :: expects_type_of
     procedure :: describe_to
  end type IsTrueOrFalse


contains

  function true() result(obj)
    type(IsTrueOrFalse) :: obj
    obj%value = .true.
  end function true

  function False() result(obj)
    type(IsTrueOrFalse) :: obj
    obj%value = .false.
  end function False
  
  logical function matches_safely(this, actual_value)
    class(IsTrueOrFalse), intent(in) :: this
    class(*), intent(in) :: actual_value

    select type (actual_value)
    type is (logical)
       matches_safely = (actual_value .eqv. this%value)
    end select
    
  end function matches_safely

  subroutine describe_mismatch_safely(this, actual, description)
    class(IsTrueOrFalse), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    select type (actual)
    type is (logical)
       call description%append_text("was ")
       call description%append_value(actual)
    end select

  end subroutine describe_mismatch_safely
  
  logical function expects_type_of(this, actual) result(supported)
    class(IsTrueOrFalse), intent(in) :: this
    class(*), intent(in) :: actual

    _UNUSED_DUMMY(this)
    
    select type (actual)
    type is (logical)
       supported = .true.
    class default
       supported = .false.
    end select

  end function expects_type_of

  subroutine describe_to(this, description)
    class(IsTrueOrFalse), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description

    call description%append_value(this%value)

  end subroutine describe_to

end module pf_IsTrueOrFalse
     
