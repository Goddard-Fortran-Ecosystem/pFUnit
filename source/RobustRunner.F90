!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: RobustRunner
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
module RobustRunner_mod
   use Test_mod
   use TestCase_mod
   use BaseTestRunner_mod
   use UnixProcess_mod
   use AbstractPrinter_mod
   implicit none
   private

   public :: RobustRunner
#ifndef DEFERRED_LENGTH_CHARACTER
   integer, parameter :: MAX_LENGTH_COMMAND=80
#endif

   type, extends(BaseTestRunner) :: RobustRunner
      private
#ifdef DEFERRED_LENGTH_CHARACTER
      character(len=:), allocatable :: remoteRunCommand
#else
      character(len=MAX_LENGTH_COMMAND) :: remoteRunCommand
#endif
      integer :: numSkip
      type (PrinterPointer), allocatable :: printers(:)
      type (UnixProcess) :: remoteProcess
   contains
      procedure :: run
      procedure :: runWithResult
      procedure :: startTest
      procedure :: endTest
      procedure :: addFailure
      procedure :: addError
      procedure :: launchRemoteRunner
      procedure :: createTestResult
   end type RobustRunner

   interface RobustRunner
      module procedure newRobustRunner
      module procedure newRobustRunner_printers
   end interface RobustRunner

   type, extends(TestCase) :: TestCaseMonitor
      private
      type (UnixProcess), pointer :: process
   contains
      procedure :: runMethod
   end type TestCaseMonitor

   real, parameter :: MAX_TIME_LAUNCH = 5.00 ! in seconds

contains

   function newRobustRunner(remoteRunCommand) result(runner)
      type (RobustRunner) :: runner
      character(len=*), intent(in) :: remoteRunCommand
      
      runner%remoteRunCommand = trim(remoteRunCommand)
      allocate(runner%printers(0))
   end function newRobustRunner

   function newRobustRunner_printers(remoteRunCommand, printers) result(runner)
      type (RobustRunner) :: runner
      character(len=*), intent(in) :: remoteRunCommand
      type(PrinterPointer), intent(in) :: printers(:)

      allocate(runner%printers(size(printers)), source=printers)
      runner%remoteRunCommand = trim(remoteRunCommand)
   end function newRobustRunner_printers

   subroutine runMethod(this)
      class (TestCaseMonitor), intent(inout) :: this
   end subroutine runMethod

   subroutine run(this, aTest, context, returnCode)
      use Test_mod
      use TestSuite_mod
      use TestResult_mod
      use ParallelContext_mod
      class (RobustRunner), intent(inout) :: this
      class (Test), intent(inout) :: aTest
      class (ParallelContext), intent(in) :: context
      integer, intent(out) :: returnCode

      type (TestResult) :: result

      result = this%createTestResult()
      call this%runWithResult(aTest, context, result, returnCode)

   end subroutine run

   subroutine runWithResult(this, aTest, context, result, returnCode)
      use Test_mod
      use ParallelContext_mod
      use TestResult_mod
      use RemoteProxyTestCase_mod
      use TestSuite_mod
      use Exception_mod
      class (RobustRunner), intent(inout) :: this
      class (Test), intent(inout) :: aTest
      class (ParallelContext), intent(in) :: context
      type (TestResult), intent(inout) :: result
      integer, intent(out) :: returnCode

      type (TestCaseReference), allocatable :: testCases(:)
      type (RemoteProxyTestCase) :: proxy
      integer :: i
      integer :: clockStart, clockStop, clockRate
      real :: runTime

      call system_clock(clockStart)

      do i=1,size(this%printers)
         call result%addListener(this%printers(i)%pPrinter)
      end do
      call result%addListener( this ) ! - monitoring

      select type (aTest)
      class is (TestSuite)
#if defined(__INTEL_COMPILER) && (INTEL_13)
         testCases = aTest%getTestCases()
#else
         call aTest%getTestCases(testCases)
#endif

      class is (TestCase)
         testCases = [TestCaseReference(aTest)]
      class default
         stop
      end select

      do i = 1, size(testCases)
         if (.not. this%remoteProcess%isActive()) then
            call this%launchRemoteRunner(numSkip=i-1)
         end if
         proxy = RemoteProxyTestCase(testCases(i)%test,this%remoteProcess)
         call proxy%run(result, context)
      end do

      call system_clock(clockStop, clockRate)
      runTime = real(clockStop - clockStart) / clockRate

      if (context%isRootProcess())  then
         do i=1,size(this%printers)
            call this%printers(i)%pPrinter%print(result, runTime)
         end do
      end if

      returnCode = getReturnCode(result)

   end subroutine runWithResult

   subroutine launchRemoteRunner(this, numSkip)
      use UnixProcess_mod
      use Exception_mod
      use Assert_mod
      class (RobustRunner), intent(inout) :: this
      integer, intent(in) :: numSkip

      character(len=:), allocatable :: command

      integer, parameter :: MAX_LEN=8
      character(len=MAX_LEN) :: suffix

      character(len=80) :: timeCommand
      type (UnixProcess) :: timerProcess
      character(len=:), allocatable :: line

      write(suffix,'(i0)') numSkip
      command = trim(this%remoteRunCommand) // ' -skip ' // suffix
      this%remoteProcess = UnixProcess(command, runInBackground=.true.)

      ! Check for successful launch - prevents MPI launch time from counting against
      ! first test's time limit.
      write(timeCommand,'(a, f10.3,a,i0,a)') &
           & "(sleep ",MAX_TIME_LAUNCH," && kill -9 ", this%remoteProcess%getPid(),") > /dev/null 2>&1"
      timerProcess = UnixProcess(trim(timeCommand), runInBackground=.true.)

      do
         line = this%remoteProcess%getLine()
         if (len(line) == 0) then
            if (.not. this%remoteProcess%isActive()) then
               call throw('RUNTIME-ERROR: terminated before starting')
               call timerProcess%terminate()
               return
            else
!!$               call timerProcess%terminate()
!!$               timerProcess = UnixProcess(trim(timeCommand), runInBackground=.true.)
               cycle ! might just not be ready yet
            end if
         else
            call assertEqual('*LAUNCHED*', line)
            ! successfully launched
            call timerProcess%terminate()
            exit
         end if
      end do

   end subroutine launchRemoteRunner

   ! No matter what, we don't want to rerun this test, so
   ! we need to increment numSkip here.
   subroutine startTest(this, testName)
      class (RobustRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      
      this%numSkip = this%numSkip + 1

   end subroutine startTest

   subroutine endTest(this, testName)
      class (RobustRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
   end subroutine endTest

   subroutine addFailure(this, testName, exceptions)
      use Exception_mod
      class (RobustRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      type (Exception), intent(in) :: exceptions(:)

   end subroutine addFailure

   subroutine addError(this, testName, exceptions)
      use Exception_mod
      class (RobustRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      type (Exception), intent(in) :: exceptions(:)

   end subroutine addError

   function createTestResult(this) result(tstResult)
      use TestResult_mod
      class (RobustRunner), intent(inout) :: this
      type (TestResult) :: tstResult
      tstResult = newTestResult()
    end function createTestResult

end module RobustRunner_mod
