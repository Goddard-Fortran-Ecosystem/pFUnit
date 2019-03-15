module pf_TimeoutAnnotation
  use pf_TestAnnotation
  use pf_TestTimer
  implicit none
  private

  public :: TimeoutAnnotation ! singleton
  
  type, extends(TestAnnotation) :: TimeoutAnnotation
     private
     real :: timeout
   contains
     procedure, nopass :: type_name
     procedure :: make_timer
  end type TimeoutAnnotation
  
  interface TimeoutAnnotation
     module procedure new_TimeoutAnnotation
  end interface TimeoutAnnotation
    
contains

  function new_TimeoutAnnotation(timeout) result(annotation)
    type(TimeoutAnnotation) :: annotation
    real, intent(in) :: timeout

    annotation%timeout = timeout
    
  end function new_TimeoutAnnotation


  function make_timer(this) result(timer)
    type(TestTimer) :: timer
    class(TimeoutAnnotation), intent(in) :: this
    timer = TestTimer(this%timeout)
  end function make_timer

  function type_name()
    character(:), allocatable :: type_name
    
    type_name = 'Timeout'
    
  end function type_name
  

end module pf_TimeoutAnnotation
