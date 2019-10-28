! Array wrappers allow the framework to treat arrays as single
! objects.  This separates concerns of overloading by type (via
! unlimited polymorphic) vs overloading by rank.

module pf_AbstractArrayWrapper
  implicit none
  private

  public :: AbstractArrayWrapper

  type, abstract :: AbstractArrayWrapper
   contains
     procedure(get_ith), deferred :: get_ith
     procedure(get), deferred :: get
  end type AbstractArrayWrapper

  abstract interface

     function get_ith(this, i) result(item)
       import AbstractArrayWrapper
       class(*), allocatable :: item
       class(AbstractArrayWrapper), target, intent(in) :: this
       integer, intent(in) :: i
     end function get_ith

     function get(this) result(list)
       import AbstractArrayWrapper
       class(*), allocatable :: list(:)
       class(AbstractArrayWrapper), intent(in) :: this
     end function get

  end interface

end module pf_AbstractArrayWrapper


