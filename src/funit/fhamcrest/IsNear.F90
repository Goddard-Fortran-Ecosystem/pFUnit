#include "unused_dummy.fh"

module pf_IsNear
  use pf_TypeSafeMatcher
  use pf_MatcherDescription
  implicit none
  private

  public :: IsNear
  public :: near

  type, extends(TypeSafeMatcher) :: IsNear
     private
     real :: tolerance
     real :: value
   contains
     procedure :: matches_safely
     procedure :: describe_mismatch_safely
     procedure :: expects_type_of
     procedure :: delta
     procedure :: describe_to
  end type IsNear

  interface near
     module procedure near_real
  end interface near
  

contains


  function near_real(value, tolerance) result(near)
    type(IsNear) :: near
    real, intent(in) :: value
    real, intent(in) :: tolerance

    near%value = value
    near%tolerance = tolerance

  end function near_real


  function relatively_near_real(value, tolerance) result(relatively_near)
    type(IsNear) :: relatively_near
    real, intent(in) :: value
    real, intent(in) :: tolerance

    relatively_near%value = value
    relatively_near%tolerance = tolerance

  end function relatively_near_real


  logical function matches_safely(this, actual_value)
    class(IsNear), intent(in) :: this
    class(*), intent(in) :: actual_value

    select type (actual_value)
    type is (real)
       matches_safely = abs(actual_value - this%value) <= this%tolerance
    end select
    
  end function matches_safely

  real function delta(this, actual)
    class(IsNear), intent(in) :: this
    real, intent(in) :: actual

    delta = abs(actual - this%value)
  end function delta


  subroutine describe_mismatch_safely(this, actual, description)
    class(IsNear), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    real :: d

    select type (actual)
    type is (real)
       d = this%delta(actual)
       call description%append_value(actual)
       call description%append_text(" differed by ")
       call description%append_value(d)
       call description%append_text(" which exceeds the tolerance by ")
       call description%append_value(d - this%tolerance)
    end select

  end subroutine describe_mismatch_safely

  logical function expects_type_of(this, actual) result(supported)
    class(IsNear), intent(in) :: this
    class(*), intent(in) :: actual

    _UNUSED_DUMMY(this)
    
    select type (actual)
    type is (real)
       supported = .true.
    class default
       supported = .false.
    end select

  end function expects_type_of


  subroutine describe_to(this, description)
    class(IsNear), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description

    call description%append_text("a numeric value within ")
    call description%append_value(this%tolerance)
    call description%append_text(" of ")
    call description%append_value(this%value)

    

  end subroutine describe_to

end module pf_IsNear
