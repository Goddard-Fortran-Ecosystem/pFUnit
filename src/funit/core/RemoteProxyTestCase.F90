!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: RemoteProxyTestCase
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module PF_RemoteProxyTestCase
  use pf_SourceLocation
   use PF_UnixProcess
   use PF_ExceptionList
   use pf_DisableAnnotation
   use PF_Test
   use PF_TestCase
   use iso_c_binding
   use pf_Posix
   use pf_File
   use pf_TestAnnotation
   use pf_TimeoutAnnotation
   use pf_TestTimer
   implicit none
   private

   public :: RemoteProxyTestCase

   type, extends(TestCase) :: RemoteProxyTestCase
      private
      type(File) :: posix_file
      type(TestTimer) :: timer
      logical :: errored = .false.
   contains
     procedure :: runMethod
     procedure :: encountered_errors
   end type RemoteProxyTestCase

   interface RemoteProxyTestCase
      module procedure newRemoteProxyTestCase
   end interface RemoteProxyTestCase

contains

   function newRemoteProxyTestCase(a_test, f, max_time) result(proxy)
      type (RemoteProxyTestCase) :: proxy
      class (Test), intent(in) :: a_test
      type(File), intent(in) :: f
      real, intent(in) :: max_time

      class(TestAnnotation), pointer :: annotation

      if (a_test%count('Timeout') == 1) then
         annotation => a_test%at('Timeout')
         ! cast
         select type (annotation)
         class is (TimeoutAnnotation)
            proxy%timer = annotation%make_timer()
         end select
      else
         proxy%timer = TestTimer(max_time)
      end if

      proxy%posix_file = f
      call proxy%setName(a_test%getName())

      if(a_test%is_disabled()) then
         call proxy%insert(Disable%type_name(),Disable)
      end if
      
   end function newRemoteProxyTestCase


   subroutine runMethod(this)
     use PF_UnixPipeInterfaces
     class (RemoteProxyTestCase), intent(inout) :: this

     character(:), allocatable :: line
     character(:), allocatable :: file_name
     integer :: line_number
     character(:), allocatable :: num_exceptions_str
     character(:), allocatable :: line_number_str
     integer :: num_exceptions
     character(:), allocatable :: message
     integer :: i_exception
     integer :: rc


     call this%posix_file%timed_read_line(line, this%timer, rc)
     if (rc /= SUCCESS) then
        this%errored = .true.
        call throw('RUNTIME-ERROR: failure to read start message from remote')
        return
     end if

     if (line /= 'started: '// trim(this%getName())) then
        this%errored = .true.
        call throw('RUNTIME-ERROR: wrong start message from remote')
        return
     end if

     call this%posix_file%timed_read_line(line, this%timer, rc)
     if (rc /= SUCCESS) then
        this%errored = .true.
        call throw('RUNTIME-ERROR: failure to message from remote')
        return
     end if
     if (line == 'ended: ' // trim(this%getName())) then ! success
        this%errored = .false.
        return
     else ! probably failure messages
        if (index(line, 'failed: # exceptions = ') /= 0) then
           this%errored = .true.
           call throw('RUNTIME-ERROR: misformatted message from remote')
           return
        end if

        num_exceptions_str = content_scan(line)
        read(num_exceptions_str,*) num_exceptions

        do i_exception = 1, num_exceptions
           ! File name
           call this%posix_file%timed_read_line(line, this%timer, rc)
           if (rc /= SUCCESS) then
              this%errored = .true.
              call throw('RUNTIME-ERROR: failure to parse message from remote')
              return
           end if
           file_name = content_scan(line)

           ! Line number
           call this%posix_file%timed_read_line(line, this%timer, rc)
           if (rc /= SUCCESS) then
              this%errored = .true.
              call throw('RUNTIME-ERROR: failure to parse message from remote')
              return
           end if
           line_number_str = content_scan(line)
           read(line_number_str,*) line_number

           ! Message
           call this%posix_file%timed_read_line(line, this%timer, rc)
           if (rc /= SUCCESS) then
              this%errored = .true.
              call throw('RUNTIME-ERROR: failure to parse message from remote')
              return
           end if
           message = content_scan(line)
           
           call throw(trim(message), SourceLocation(file_name, line_number))
        end do

        call this%posix_file%timed_read_line(line, this%timer, rc)
        if (rc /= SUCCESS) then
           this%errored = .true.
           call throw('RUNTIME-ERROR: failure to message from remote')
           return
        end if
        if (line == 'ended: ' // trim(this%getName())) then ! success
           this%errored = .false.
           return
        else
           this%errored = .true.
           call throw('RUNTIME-ERROR: failure to message from remote')
           return
        end if


     end if

   contains
        
      function content_scan(string) result(valueString)
         character(len=*), intent(in) :: string
         character(len=:), allocatable :: valueString
         
         integer :: i0, i1
         i0 = scan(string,'<<<') + 3
         i1 = scan(string,'>>>',back=.true.) - 3

         valueString = string(i0:i1)
      end function content_scan
       

   end subroutine runMethod

   logical function encountered_errors(this)
     class(RemoteProxyTestCase), intent(in) :: this

     encountered_errors = this%errored
   end function encountered_errors

end module PF_RemoteProxyTestCase
