module pf_Matchable
  use pf_TypeSafeSelfDescribing
  implicit none
  private

  public :: Matchable

  type, abstract, extends(TypeSafeSelfDescribing) :: Matchable
   contains
     procedure(equals), deferred :: equals
     generic :: operator(==) => equals
  end type Matchable

  abstract interface

     logical function equals(this, other)
       import Matchable
       class(Matchable), intent(in) :: this
       class(*), intent(in) :: other
     end function equals

  end interface
     
end module pf_Matchable
