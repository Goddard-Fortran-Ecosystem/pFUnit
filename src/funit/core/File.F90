  ! Wraps a Posix file descriptor with useful interfaces
module pf_File
  use, intrinsic :: iso_c_binding, only : C_INT, C_LONG
  use, intrinsic :: iso_c_binding, only : C_NULL_CHAR
  use pf_TestTimer
  use pf_Posix
  implicit none
  private

  public :: File
  public :: SUCCESS
  public :: TIMER_EXPIRED
  public :: END_OF_FILE
  public :: UNEXPECTED
  public :: FAILED_TO_OPEN

  type :: File
     private
     integer(kind=C_INT) :: file_descriptor
   contains
     procedure :: open => file_open
     procedure :: close => file_close
     procedure :: timed_read_line
  end type File

  ! Return codes
  integer, parameter :: SUCCESS = 0
  integer, parameter :: TIMER_EXPIRED = -1
  integer, parameter :: END_OF_FILE = -2
  integer, parameter :: FAILED_TO_OPEN = -3
  integer, parameter :: UNEXPECTED = -4

contains


  subroutine file_open(this, filename, timer, rc)
    class(File), intent(inout) :: this
    character(*) :: filename
    type (TestTimer), intent(inout) :: timer
    integer, intent(out) :: rc

    integer(kind=C_INT) :: fd
    type(pollfd) :: fds(1)
    integer(kind=C_INT) :: timeout

    fd = open(filename // C_NULL_CHAR, mode_t(ior(O_RDONLY, O_NONBLOCK)))
    if (fd == -1) then
       rc = FAILED_TO_OPEN
       return
    end if

    this%file_descriptor = fd
    fds(1)%fd = fd
    fds(1)%events = POLLIN ! wait for handshake

    timeout = timer%get_ms_remaining()
    rc = poll(fds, 1, timeout)
    if (rc /= 1) then
       rc = TIMER_EXPIRED
       return
    else
       rc = SUCCESS
    end if
  end subroutine file_open

  subroutine timed_read_line(this, line, timer, rc)
    class(File), intent(inout) :: this
    character(:), allocatable, intent(inout) :: line
    type(TestTimer), intent(in) :: timer
    integer, intent(out) :: rc

    integer :: status
    type(pollfd) :: fds(1)
    integer(kind=C_INT) :: timeout

    integer :: buffer_len
    integer :: i
    integer(kind=C_INT) :: errno
    character(:), allocatable :: buffer, tmp

    ! Start with a reasonable size buffer.  Grow if necessary.

    timeout = timer%get_ms_remaining()
    if (timeout <= 0) then
       line = ''
       rc = TIMER_EXPIRED
       return
    end if

    buffer_len = 80
    allocate(character(buffer_len) :: buffer)

    fds(1)%fd = this%file_descriptor
    fds(1)%events = POLLIN ! wait for handshake
    i = 1
    do
       timeout = timer%get_ms_remaining()
       status = poll(fds, 1, timeout)

       select case (status)
       case (0)
          line = ''
          rc = TIMER_EXPIRED
          return
       case (1)
          ! continue below to read data
       case default
          line = ''
          rc = UNEXPECTED
          return
       end select


       status = read(this%file_descriptor, buffer(i:i), size_t(1_C_LONG))
       select case (status)
       case (0) ! EOF
          line = buffer(1:i)
          rc = END_OF_FILE
          return
       case (-1) ! maybe no more data for
           errno = get_errno()
           if (errno /= EAGAIN) then
              ERROR STOP "problem in read"
           else
              ERROR STOP "poll() said there was more data but did not even get 1 char?"
           end if
        case (1) ! expected 1 got 1
          if (buffer(i:i) == C_NULL_CHAR) then
             rc = SUCCESS
             line = buffer(1:i-1)
             return
          else
             if (i == buffer_len) then
                call move_alloc(from=buffer, to=tmp)
                buffer_len = 2*buffer_len
                allocate(character(buffer_len) :: buffer)
                buffer(1:i) = tmp
                deallocate(tmp)
             end if
             i = i + 1
          end if
          cycle
       end select
    end do
  end subroutine timed_read_line


  subroutine file_close(this, rc)
    class(File), intent(inout) :: this
    integer(kind=C_INT), intent(out) :: rc

    rc = close(this%file_descriptor)
  end subroutine file_close

end module pf_File
