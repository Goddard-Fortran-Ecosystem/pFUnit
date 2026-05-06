#ifndef _WIN32
module pf_regex_module
! Fortran interface to POSIX regex, using ISO_C_BINDING.
! Adapted from MAPL3 utilities/regex/regex_module.F90
!
! API:
!
! Compile a regex into a regex object
!  subroutine regcomp(this,pattern,flags,status)
!    type(regex_type), intent(out) :: this           ! new regex object
!    character(len=*), intent(in) :: pattern         ! regex pattern string
!    character(len=*), intent(in), optional :: flags ! flag characters:
!                                           ! x = extended regex (REG_EXTENDED)
!                                           ! m = multi-line     (REG_NEWLINE)
!                                           ! i = case-insensitive (REG_ICASE)
!                                           ! n = no MATCH required (REG_NOSUB)
!    integer, intent(out), optional :: status ! If absent, errors are fatal
!  end subroutine regcomp
!
! Execute a compiled regex against a string
!  function regexec(this,string,matches,flags,status) result(match)
!    logical :: match ! .TRUE. if the pattern matched
!    type(regex_type), intent(in) :: this ! regex object
!    character(len=*), intent(in) :: string ! target string
!    character(len=*), intent(in), optional :: flags ! flag characters (for partial lines):
!                                       ! b = no beginning-of-line (REG_NOTBOL)
!                                       ! e = no end-of-line (REG_NOTEOL)
!    integer, intent(out), optional :: matches(:,:) ! match locations
!    integer, intent(out), optional :: status ! If absent, errors are fatal
!  end function
!
! Release 
!  subroutine regfree(this)
!    type(regex_type), intent(inout) :: this
!  end subroutine regfree
!-------------------------------------------------------------------
  use ISO_C_Binding, only: C_ptr, C_int, C_size_t, C_char, &
                           C_NULL_char, C_NULL_ptr, C_associated
  use ISO_Fortran_Env, only: ERROR_UNIT

  implicit none
  private

  public :: regex_type
  public :: regcomp
  public :: regexec
  public :: regfree
  public :: regerror

  integer, parameter  :: NO_MATCH = -1
! Fortran regex structure holds a pointer to an opaque C structure
  type regex_type
    type(C_ptr) :: preg = C_NULL_ptr
  end type regex_type
  
  interface
    subroutine C_regalloc(preg_return) &
        bind(C,name="C_regalloc")
      import
      type(C_ptr), intent(out) :: preg_return
    end subroutine C_regalloc
    
    subroutine C_regcomp(preg,pattern,flags,status) &
        bind(C,name="C_regcomp")
      import
      type(C_ptr), intent(in), value :: preg
      character(len=1,kind=C_char), intent(in) :: pattern(*)
      character(len=1,kind=C_char), intent(in) :: flags(*)
      integer(C_int), intent(inout) :: status
    end subroutine C_regcomp
    
    subroutine C_regexec(preg,string,nmatch,matches,flags,status) &
        bind(C,name="C_regexec")
      import
      type(C_ptr), intent(in), value :: preg
      character(len=1,kind=C_char), intent(in) :: string(*)
      integer(C_int), intent(in), value :: nmatch
      integer(C_int), intent(out) :: matches(2,nmatch)
      character(len=1,kind=C_char), intent(in) :: flags(*)
      integer(C_int), intent(out) :: status
    end subroutine C_regexec
    
    function C_regerror(errcode, preg, errbuf, errbuf_size) &
        result(regerror) bind(C,name="regerror")
      import
      integer(C_size_t) :: regerror
      integer(C_int), value :: errcode
      type(C_ptr), intent(in), value :: preg
      character(len=1,kind=C_char), intent(out) :: errbuf
      integer(C_size_t), value :: errbuf_size
    end function C_regerror
    
    subroutine C_regfree(preg) bind(C,name="regfree")
      import
      type(C_ptr), intent(in), value :: preg
    end subroutine C_regfree
  end interface
  
contains

  subroutine regcomp(this,pattern,flags,status)
    type(regex_type), intent(out) :: this
    character(len=*), intent(in) :: pattern
    character(len=*), intent(in), optional :: flags
    integer, intent(out), optional :: status
! local
    integer(C_int) :: status_
    character(len=10,kind=C_char) :: flags_
! begin
    flags_=' '
    if (present(flags)) flags_=flags
    this%preg = C_NULL_ptr
    call C_regalloc(this%preg)
    call C_regcomp(this%preg, trim(pattern)//C_NULL_char, &
                   trim(flags_)//C_NULL_char, status_)
    if (present(status)) then
      status=status_
    end if
  end subroutine regcomp
  
  logical function regexec(this,string,matches,flags,status) &
        result(match)
    type(regex_type), intent(in) :: this
    character(len=*), intent(in) :: string
    character(len=*), intent(in), optional :: flags
    integer, intent(out), optional :: matches(:,:)
    integer, intent(out), optional :: status
! local
    integer(C_int) :: status_, matches_(2,1)
    character(len=10,kind=C_char) :: flags_
! begin
    flags_=' '
    if (present(flags)) flags_=flags
    if (present(matches)) then
      matches = NO_MATCH
      call C_regexec(this%preg, trim(string)//C_NULL_char, &
                   size(matches,2),matches, &
                   trim(flags_)//C_NULL_char, status_)
    else
      call C_regexec(this%preg, trim(string)//C_NULL_char, &
                   int(0,C_int),matches_, &
                   trim(flags_)//C_NULL_char, status_)
    end if
    match = status_==0
    if (status_ == 1 ) status_ = 0 ! value "1" is not an error
   if (present(status)) then
      status=status_
    end if
  end function regexec
  
  subroutine regerror(this,errcode,errmsg,errmsg_len)
    type(regex_type), intent(in) :: this
    integer, intent(in) :: errcode
    character, intent(out) :: errmsg
    integer, intent(out) :: errmsg_len
    errmsg_len = C_regerror(int(errcode,C_int), this%preg, &
                 errmsg, int(len(errmsg),C_size_t))
  end subroutine regerror
  
  subroutine regfree(this)
    type(regex_type), intent(inout) :: this
    if (c_associated(this%preg)) then
      call C_regfree(this%preg)
      this%preg = C_NULL_ptr
    end if
  end subroutine regfree
  
end module pf_regex_module
#endif
