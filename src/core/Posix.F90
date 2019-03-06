module pf_Posix
  use iso_c_binding
  implicit none
  private

  ! Posix types
  public :: mode_t
  public :: size_t
  public :: pollfd

  ! Posix interfaces
  public :: mkfifo
  public :: open
  public :: close
  public :: remove
  public :: poll
  public :: read

  ! Posix parameteres
  public :: O_RDONLY, O_NONBLOCK
  public :: POLLIN!, POLLHUP, POLLERR, POLLNVAL

  ! Interface to C errno (macro)
  public :: get_errno
  public :: EAGAIN

  include 'posix_predefined.inc'

  type, bind(c) :: mode_t
     integer(kind=C_SHORT) :: mode_t
  end type mode_t

  type, bind(c) :: size_t
     integer(kind=C_LONG) :: size_t
  end type size_t

  type, bind(c) :: pollfd
     integer(kind=C_INT) :: fd
     integer(kind=C_SHORT) :: events
     integer(kind=C_SHORT) :: revents
  end type pollfd

  interface

     function mkfifo(pathname, mode) result(rc) bind(C, name='mkfifo')
       use, intrinsic :: iso_c_binding
       import mode_t
       integer(kind=C_INT) :: rc
       character(kind=C_CHAR), intent(in) :: pathname(*)
       type(mode_t), value, intent(in) :: mode
     end function mkfifo
     
     function open(pathname, mode) result(fd) bind(c,name="open")
       use, intrinsic :: iso_c_binding
       import mode_t
       integer(kind=C_INT) :: fd
       character(len=1) :: pathname(*)
       type(mode_t), value :: mode
     end function open

     function close(fd) result(rc) bind(c,name="close")
       use, intrinsic :: iso_c_binding
       integer(kind=C_INT) :: rc
       integer(kind=C_INT), value :: fd
     end function close

     function read(fd, buf, count) result(rc) bind(c, name="read")
       use, intrinsic :: iso_c_binding
       import size_t
       integer(kind=C_INT) :: rc
       integer(kind=C_INT), value :: fd
       character(len=1), intent(out) :: buf(*)
       type(size_t), value :: count
     end function read

     function remove(pathname) result(rc) bind(C, name='remove')
       use, intrinsic :: iso_c_binding
       integer(kind=C_INT) :: rc
       character(len=1) :: pathname(*)
     end function remove

     function poll(fds, nfd, timeout) result(rc) bind(c, name='poll')
       use, intrinsic :: iso_c_binding
       import pollfd
       integer(kind=C_INT) :: rc
       type(pollfd) :: fds(*)
       integer(kind=C_INT), value :: nfd
       integer(kind=C_INT), value :: timeout
     end function poll

     function get_errno() result(errno) bind(c, name='get_errno')
       use iso_c_binding
       integer(kind=C_INT) :: errno
     end function get_errno
  end interface

end module pf_Posix


