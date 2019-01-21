module pf_TypeSafeMatcher_mod
  use pf_AbstractMatcher_mod
  use pf_MatcherDescription_mod, only: MatcherDescription
  use iso_fortran_env
  implicit none
  private

  public :: TypeSafeMatcher

  type, abstract, extends(AbstractMatcher) :: TypeSafeMatcher
   contains
     procedure :: matches
     procedure(matches_safely), deferred :: matches_safely
     procedure :: describe_mismatch
     procedure(describe_mismatch_safely), deferred :: describe_mismatch_safely
  end type TypeSafeMatcher

  abstract interface

     logical function matches_safely(this, item)
       use iso_fortran_env
       import MatcherDescription
       import TypeSafeMatcher
       class(TypeSafeMatcher), intent(in) :: this
       type({case.type}(kind={case.kind})), intent(in) :: item
     end function matches_safely

     subroutine describe_mismatch_safely(this, item, description)
       use iso_fortran_env
       import MatcherDescription
       import TypeSafeMatcher
       class(TypeSafeMatcher), intent(in) :: this
       type({case.type}(kind={case.kind})), intent(in) :: item
       class(MatcherDescription), intent(inout) :: description
     end subroutine describe_mismatch_safely

  end interface


contains


  logical function matches(this, actual_value)
    class(TypeSafeMatcher), intent(in) :: this
    class(*), intent(in) :: actual_value

    select type(actual_value)
    type is ({case.type}(kind={case.kind}))
       matches = this%matches_safely(actual_value)
    class default
       matches = .false.
    end select

  end function matches
  
  subroutine describe_mismatch(this, actual, description)
    class(TypeSafeMatcher), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    select type (actual)
    type is ({case.type}(kind={case.kind}))
       call this%describe_mismatch_safely(actual, description)
    class default
       call description%append_text("was not a ")
       call description%append_text("type({case.type}(kind={case.kind}))")
    end select
       
  end subroutine describe_mismatch
  
end module pf_TypeSafeMatcher_mod
