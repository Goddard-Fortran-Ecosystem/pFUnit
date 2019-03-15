module pf_TestFilter
  implicit none
  private

  public :: TestFilter

  type, abstract :: TestFilter
   contains
     procedure(filter), deferred :: filter
  end type TestFilter


  abstract interface

     logical function filter(this, a_test)
       use pf_Test
       import TestFilter
       class(TestFilter), intent(in) :: this
       class(Test), intent(in) :: a_test
     end function filter

  end interface

end module pf_TestFilter
