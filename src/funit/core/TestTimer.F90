module pf_TestTimer
  use, intrinsic :: iso_fortran_env, only: INT64
  use, intrinsic :: iso_c_binding, only: C_INT
  implicit none
  private

  public :: TestTimer
  
  type :: TestTimer
     private
     integer(kind=INT64) :: start_count
     integer(kind=INT64) :: max_count
   contains
     procedure :: get_ms_remaining
     procedure :: get_elapsed_time
  end type TestTimer

  interface TestTimer
     module procedure new_TestTimer
  end interface TestTimer

contains

  
  function new_TestTimer(max_time) result(timer)
    type(TestTimer) :: timer
    real, intent(in) :: max_time
    
    integer(kind=INT64) :: count_rate

    call system_clock(timer%start_count, count_rate)
    timer%max_count = timer%start_count + ceiling(1000*max_time) * count_rate/1000
    
  end function new_TestTimer

  function get_ms_remaining(this) result(ms)
    integer(kind=C_INT) :: ms
    class(TestTimer), intent(in) :: this
    integer(kind=INT64) :: count, count_rate

    call system_clock(count, count_rate)
    ms = max(0_INT64, 1000*(this%max_count - count)/count_rate)

  end function get_ms_remaining

  function get_elapsed_time(this) result(elapsed)
    real :: elapsed
    class(TestTimer), intent(in) :: this
    integer(kind=INT64) :: count, count_rate

    call system_clock(count, count_rate)
    elapsed = real(count - this%start_count)/count_rate

  end function get_elapsed_time
  
end module pf_TestTimer
