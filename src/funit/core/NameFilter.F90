module pf_NameFilter
  use pf_TestFilter
  use pf_Test
  implicit none
  private

  public :: NameFilter

  type, extends(TestFilter) :: NameFilter
     character(:), allocatable :: pattern
   contains
     procedure :: filter
  end type NameFilter


  interface NameFilter
     module procedure new_NameFilter
  end interface NameFilter

contains

  function new_NameFilter(pattern) result(filter)
    type(NameFilter) :: filter
    character(*), intent(in) :: pattern

    filter%pattern = pattern
  end function new_NameFilter


  logical function filter(this, a_test)
    class(NameFilter), intent(in) :: this
    class(Test), intent(in) :: a_test

    filter = index(a_test%getName(), this%pattern) > 0
  end function filter


end module pf_NameFilter
