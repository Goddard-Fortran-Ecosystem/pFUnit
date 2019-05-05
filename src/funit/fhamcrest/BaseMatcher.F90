#include "unused_dummy.fh"

module pf_BaseMatcher
  use pf_AbstractMatcher
  use pf_MatcherDescription
  implicit none
  private

  public :: BaseMatcher

  type, abstract, extends(AbstractMatcher) :: BaseMatcher
     private
   contains
     procedure :: describe_mismatch
  end type BaseMatcher

contains

  subroutine describe_mismatch(this, actual, description)
    class(BaseMatcher), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    _UNUSED_DUMMY(this)
    call description%append_text("was ")
    call description%append_value(actual)

  end subroutine describe_mismatch
    
end module pf_BaseMatcher
