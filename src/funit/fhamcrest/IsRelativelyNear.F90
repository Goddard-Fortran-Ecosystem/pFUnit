#include "unused_dummy.fh"

module pf_IsRelativelyNear
  use pf_TypeSafeMatcher
  use pf_MatcherDescription
  implicit none
  private

  public :: IsRelativelyNear
  public :: relatively_near


  type, extends(TypeSafeMatcher) :: IsRelativelyNear
     private
     real :: tolerance
     real :: value
     character(:), allocatable :: text_description
   contains
     procedure :: matches_safely
     procedure :: describe_mismatch_safely
     procedure :: expects_type_of
     procedure :: delta
     procedure :: describe_to
  end type IsRelativelyNear


  interface relatively_near
     module procedure relatively_near_real
  end interface relatively_near
  

contains


  function relatively_near_real(value, tolerance) result(relatively_near)
    type(IsRelativelyNear) :: relatively_near
    real, intent(in) :: value
    real, intent(in) :: tolerance

    relatively_near%value = value
    relatively_near%tolerance = tolerance

  end function relatively_near_real


  logical function matches_safely(this, actual_value)
    class(IsRelativelyNear), intent(in) :: this
    class(*), intent(in) :: actual_value

    select type (actual_value)
    type is (real)
       ! Note: multiplication is used on RHS to avoid divide-by-zero issues
       matches_safely = this%delta(actual_value) <= this%tolerance
    end select
    
  end function matches_safely


  real function delta(this, actual)
    class(IsRelativelyNear), intent(in) :: this
    real, intent(in) :: actual

    delta = abs(actual - this%value) / abs(this%value)

  end function delta


  subroutine describe_mismatch_safely(this, actual, description)
    class(IsRelativelyNear), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    real :: d

    select type (actual)
    type is (real)
       d = this%delta(actual)
       call description%append_value(actual)
       call description%append_text(" has a relative error of ")
       call description%append_value(d)
       call description%append_text(" which exceeds the tolerance by ")
       call description%append_value(d - this%tolerance)
    end select

  end subroutine describe_mismatch_safely

  logical function expects_type_of(this, actual) result(supported)
    class(IsRelativelyNear), intent(in) :: this
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
    class(IsRelativelyNear), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description

    call description%append_text("a numeric value whose relative error from ")
    call description%append_value(this%value)
    call description%append_text(" is less than ")
    call description%append_value(this%tolerance)

  end subroutine describe_to

end module pf_IsRelativelyNear
