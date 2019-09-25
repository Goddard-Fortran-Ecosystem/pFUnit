! Array wrappers allow the framework to treat arrays as single
! objects.  This separates concerns of overloading by type (via
! unlimited polymorphic) vs overloading by rank.

module pf_AbstractArrayWrapper
  implicit none
  private

  public :: AbstractArrayWrapper

  type, abstract :: AbstractArrayWrapper
  end type AbstractArrayWrapper

end module pf_AbstractArrayWrapper
