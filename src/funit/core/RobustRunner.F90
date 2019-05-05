#include "unused_dummy.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: RobustRunner
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
module PF_RobustRunner
   use PF_Test
   use PF_TestCase
   use PF_BaseTestRunner
   use PF_TestListener
   use PF_UnixProcess
   use PF_UnixPipeInterfaces
   use pf_TestRunner
   use pf_File
   use pf_TestTimer
   use pf_ResultPrinter
   use iso_c_binding
   use iso_fortran_env
   implicit none
   private

   public :: RobustRunner

   type, extends(TestRunner) :: RobustRunner
      private
      real :: maxLaunchDuration
      real :: max_time_per_test
      character(len=:), allocatable :: remoteRunCommand
      type (UnixProcess) :: remoteProcess
   contains
      procedure :: runWithResult
      procedure :: launchRemoteRunner
   end type RobustRunner

   interface RobustRunner
      module procedure new_RobustRunner_default
      module procedure new_RobustRunner_unit
      module procedure new_RobustRunner_printer
   end interface RobustRunner

!!! Inject dependency through constructor...
   real, parameter :: MAX_TIME_LAUNCH = 5.00 ! in seconds
   real, parameter :: MAX_TIME_TEST   = 0.10 ! in seconds

   character(*), parameter :: REMOTE_PROCESS_PIPE = '.remote_process_pipe'
   character(*), parameter :: C_REMOTE_PROCESS_PIPE = REMOTE_PROCESS_PIPE // C_NULL_CHAR

contains

  function new_RobustRunner_default(maxLaunchDuration, max_time_per_test, remoteRunCommand) result(runner)
    use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
    type (RobustRunner) :: runner
    real, optional, intent(in) :: maxLaunchDuration
    real, optional, intent(in) :: max_time_per_test
    character(*), optional, intent(in) :: remoteRunCommand

    runner = RobustRunner(OUTPUT_UNIT, max_time_per_test, maxLaunchDuration, remoteRunCommand)

  end function new_RobustRunner_default


  function new_RobustRunner_unit(unit, maxLaunchDuration, max_time_per_test, remoteRunCommand) result(runner)
    type (RobustRunner) :: runner
    integer, intent(in) :: unit
    real, optional, intent(in) :: maxLaunchDuration
    real, optional, intent(in) :: max_time_per_test
    character(*), optional, intent(in) :: remoteRunCommand
    runner = RobustRunner(ResultPrinter(unit), maxLaunchDuration, max_time_per_test, remoteRunCommand)
  end function new_RobustRunner_unit

  
  function new_RobustRunner_printer(printer, maxLaunchDuration, max_time_per_test, remoteRunCommand) result(runner)
    type (RobustRunner) :: runner
    type(ResultPrinter), intent(in) :: printer
    real, optional, intent(in) :: maxLaunchDuration
    real, optional, intent(in) :: max_time_per_test
    character(*), optional, intent(in) :: remoteRunCommand

    ! parent
    runner%TestRunner = TestRunner(printer)

    if(present(maxLaunchDuration))then
       runner%maxLaunchDuration = maxLaunchDuration
    else
       runner%maxLaunchDuration = MAX_TIME_LAUNCH
    end if

    if (present(remoteRunCommand)) then
       runner%remoteRunCommand = trim(remoteRunCommand)
    else
       runner%remoteRunCommand = get_default_launch_command()
    end if

    if (present(max_time_per_test)) then
       runner%max_time_per_test = max_time_per_test
    else
       runner%max_time_per_test = MAX_TIME_TEST
    end if

  contains

    function get_default_launch_command() result(command)
      character(:), allocatable :: command
      integer :: n

      call get_command_argument(0, length=n)
      allocate(character(len=n) :: command)
      call get_command_argument(0, value=command)

      command = command // ' -o ' // REMOTE_PROCESS_PIPE // ' --runner RemoteRunner '
    end function get_default_launch_command

  end function new_RobustRunner_printer

   subroutine runWithResult(this, aTest, context, result)
     use pf_Posix, only: remove, mkfifo, mode_t
      use PF_Test
      use PF_ParallelContext
      use PF_TestResult
      use PF_RemoteProxyTestCase
      use PF_TestSuite
      use PF_TestVector
      use PF_ExceptionList
      use pf_File
      class (RobustRunner), target, intent(inout) :: this
      class (Test), intent(inout) :: aTest
      class (ParallelContext), intent(in) :: context
      type (TestResult), intent(inout) :: result

      type (TestVector) :: testCases
      type (RemoteProxyTestCase) :: proxy
      integer :: i
      type(mode_t) :: mode
      integer(kind=C_INT) :: status
      integer :: rc
      logical :: needs_launch
      type (File) :: f

      call result%addListener( this ) ! - monitoring

      select type (aTest)
      class is (TestSuite)
         call aTest%getTestCases(testCases)
      class is (TestCase)
         call testCases%push_back(aTest)
      class default
         stop
      end select

      needs_launch = .true.
      
      do i = 1, testCases%size()
         if (needs_launch) then
            !TODO:  What is the correct mode here?
            mode%mode_t = O'0777'
            rc = mkfifo(C_REMOTE_PROCESS_PIPE, mode)
            if (rc /= 0) ERROR STOP 'failed to make named pipe'

            call this%launchRemoteRunner(f, numSkip=i-1)
            needs_launch = .false.
         end if
            
         proxy = RemoteProxyTestCase(testCases%at(i), f, this%max_time_per_test)
         call proxy%run(result, context)

         if (proxy%encountered_errors()) then
            call execute_command_line('rm ' // REMOTE_PROCESS_PIPE)
            if (this%remoteProcess%is_active()) then
               call this%remoteProcess%terminate()
            end if
            needs_launch = .true.
         end if
      end do

      call f%close(status)
      status = remove(C_REMOTE_PROCESS_PIPE)

   end subroutine runWithResult

   subroutine launchRemoteRunner(this, f, numSkip)
      use PF_UnixProcess
      use PF_ExceptionList
      use pf_Posix, only: remove
      class (RobustRunner), intent(inout) :: this
      type(File), intent(inout) :: f
      integer, intent(in) :: numSkip

      character(len=:), allocatable :: command

      integer, parameter :: MAX_LEN=8
      integer :: status
      character(len=MAX_LEN) :: suffix

      character(:), allocatable :: buffer
      type (TestTimer) :: timer

      
      write(suffix,'(i0)') numSkip
      command = trim(this%remoteRunCommand) // ' --skip ' // trim(suffix)
      this%remoteProcess = UnixProcess(command, runInBackground=.true.)
      timer = TestTimer(this%maxLaunchDuration)
      call f%open(REMOTE_PROCESS_PIPE, timer, rc=status)

      select case(status)
      case (FAILED_TO_OPEN)
         call this%remoteProcess%terminate()
         status = remove(C_REMOTE_PROCESS_PIPE)
         ERROR STOP "unknown problem connecting with remote process"
      case (TIMER_EXPIRED)
         call this%remoteProcess%terminate()
         status = remove(C_REMOTE_PROCESS_PIPE)
         ERROR STOP "remote launch has timed out"
      case (SUCCESS)
         ! yay
      end select

      call f%timed_read_line(buffer, timer, rc=status)
      select case (status)
      case (SUCCESS)
         if (buffer /= '*LAUNCHED*') then
            call this%remoteProcess%terminate()
            status = remove(C_REMOTE_PROCESS_PIPE)
            ERROR STOP "remote handshake failed"
         else
            return
         end if
      case default
         call this%remoteProcess%terminate()
         status = remove(C_REMOTE_PROCESS_PIPE)
         ERROR STOP "remote handshake failed"
      end select

   end subroutine launchRemoteRunner

   subroutine startTest(this, testName)
      class (RobustRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      
     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(testName)

   end subroutine startTest

   subroutine endTest(this, testName)
      class (RobustRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName

     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(testName)

   end subroutine endTest

   subroutine addFailure(this, testName, exceptions)
      use PF_ExceptionList
      class (RobustRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(testName)
     _UNUSED_DUMMY(exceptions)

   end subroutine addFailure

   subroutine addError(this, testName, exceptions)
      use PF_ExceptionList
      class (RobustRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(testName)
     _UNUSED_DUMMY(exceptions)

   end subroutine addError

end module PF_RobustRunner
