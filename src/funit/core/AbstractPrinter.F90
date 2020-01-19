module pf_AbstractPrinter
  use pf_TestListener
  implicit none
  private

  public :: AbstractPrinter

  type, abstract, extends(TestListener) :: AbstractPrinter
   contains
     procedure(print), deferred :: print
  end type AbstractPrinter


  abstract interface

     subroutine print(this, result, elapsed_time)
       use pf_AbstractTestResult
       import AbstractPrinter
       
       class(AbstractPrinter), intent(in) :: this
       class(AbstractTestResult), intent(in) :: result
       real, intent(in) :: elapsed_time
     end subroutine print
  end interface
  
end module pf_AbstractPrinter
