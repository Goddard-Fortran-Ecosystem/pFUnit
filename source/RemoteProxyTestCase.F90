!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: RemoteProxyTestCase
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC 
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
module RemoteProxyTestCase_mod
   use UnixProcess_mod
   use Exception_mod
   use Assert_mod
   use TestCase_mod
   implicit none
   private

   public :: RemoteProxyTestCase

   type, extends(TestCase) :: RemoteProxyTestCase
      private
      type (UnixProcess), pointer :: process
      integer :: clockStart
   contains
      procedure :: runMethod
      procedure :: setStartTime
   end type RemoteProxyTestCase

   interface RemoteProxyTestCase
      module procedure newRemoteProxyTestCase
   end interface RemoteProxyTestCase

   real, parameter :: MAX_TIME_TEST = 0.10 ! in seconds

contains

   function newRemoteProxyTestCase(test, process) result(proxy)
      type (RemoteProxyTestCase) :: proxy
      class (TestCase), intent(in) :: test
      type (UnixProcess), target :: process

      call proxy%setName(test%getName())
      proxy%process => process
      
   end function newRemoteProxyTestCase

   subroutine runMethod(this)
      use SourceLocation_mod
      use UnixProcess_mod
      use, intrinsic :: iso_c_binding
      class (RemoteProxyTestCase), intent(inout) :: this
      character(len=:), allocatable :: line

      character(len=:), allocatable :: message
      character(len=:), allocatable :: fileName

      character(len=80) :: timeCommand
      type (UnixProcess) :: timerProcess
      integer :: numExceptions, iException ,i
      integer :: lineNumber
      integer :: length
      character(len=100) :: ignore

      call this%setStartTime()

      ! Software equivalent of a ticking time bomb:
      ! Timer process sleeps for n milliseconds and then kills the remote test process.
      ! If the appropriate messages are received in time, then this timer process is 
      ! safely stopped.

      write(timeCommand,'(a, f10.3,a,i0,a)') "(sleep ",MAX_TIME_TEST,"; kill -9 ", this%process%getPid(),") > /dev/null 2>&1"
      timerProcess = UnixProcess(trim(timeCommand), runInBackground=.true.)

      do
         ! important to check status _before_ getLine()
         line = this%process%getLine()
         if (len(line) == 0) then
            if (.not. this%process%isActive()) then
               call throw('RUNTIME-ERROR: terminated before starting')
               call timerProcess%terminate()
               return
            else
               call timerProcess%terminate()
               timerProcess = UnixProcess(trim(timeCommand), runInBackground=.true.)
               cycle ! might just not be ready yet
            end if
         else
            call assertEqual('started: '//trim(this%getName()), line)
            exit
         end if

      end do

      ! Poll for exceptions or test finished
      do
         ! important to check status _before_ getLine()
         line = this%process%getLine()
         if (len(line) == 0) then
            if (this%process%isActive()) then
               call timerProcess%terminate()
               call this%process%terminate()
               call throw('RUNTIME-ERROR: active but no output?')
               return
            else ! process not active crashed or killed by child
               if (timerProcess%isActive()) then
                  call timerProcess%terminate()
                  call this%process%terminate()
                  call throw('RUNTIME-ERROR: terminated during execution')
                  return
               else ! child has completed - implies test was hung and processing terminated
                  call throw('RUNTIME-ERROR: hung')
                  return
               end if
            end if

         else ! have some output to process

            if (line == ('ended: ' // trim(this%getName()))) then

               call timerProcess%terminate()
               return

            elseif (line(1:22) == 'failed: numExceptions=') then

               read(line(23:),*) numExceptions

               do iException = 1, numExceptions
                  line = contentScan(this%process%getline())
                  read(line,*) length
                  
                  fileName = contentScan(this%process%getLine())
                  line = contentScan(this%process%getLine())
                  read(line,*) lineNumber
                  line = contentScan(this%process%getLine())
                  read(line,*) length
                  allocate(character(len=length) :: message)
                  line = this%process%getDelim(C_NULL_CHAR)
                  message = contentScan(line)
                  ! eat remaining linefeed
                  line= this%process%getLine()
                  call throw(trim(message), SourceLocation(fileName, lineNumber))
               end do
               cycle ! still need to process the end message

            else

               call timerProcess%terminate()
               call this%process%terminate()
               call throw('ERROR: unexpected message: '//trim(line))
               return

            end if
         end if
      end do

      ! no path to here

   contains

      function contentScan(string) result(valueString)
         character(len=*), intent(in) :: string
         character(len=:), allocatable :: valueString
         
         integer :: i0, i1
         i0 = scan(string,'<') + 1
         i1 = scan(string,'>',back=.true.) - 1

         valueString = string(i0:i1)
      end function contentScan

   end subroutine runMethod

   subroutine setStartTime(this)
      class (RemoteProxyTestCase), intent(inout) :: this
      call system_clock(this%clockStart)
   end subroutine setStartTime

end module RemoteProxyTestCase_mod
