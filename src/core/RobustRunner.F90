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
   implicit none
   private

   public :: RobustRunner

   type, extends(BaseTestRunner) :: RobustRunner
      private
      character(len=:), allocatable :: remoteRunCommand
      integer :: numSkip
      type (ListenerPointer), allocatable :: extListeners(:)
      type (UnixProcess) :: remoteProcess
      real :: maxLaunchDuration
      real :: maxTimeoutDuration
   contains
      procedure :: run
      procedure :: runWithResult
      procedure :: startTest
      procedure :: endTest
      procedure :: endRun
      procedure :: addFailure
      procedure :: addError
      procedure :: launchRemoteRunner
      procedure :: createTestResult
      procedure :: addSuccess
   end type RobustRunner

   interface RobustRunner
!      module procedure newRobustRunner
      module procedure newRobustRunner_extListeners
   end interface RobustRunner

   type, extends(TestCase) :: TestCaseMonitor
      private
      type (UnixProcess), pointer :: process
   contains
      procedure :: runMethod
   end type TestCaseMonitor

!!! Inject dependency through constructor...
   real, parameter :: MAX_TIME_LAUNCH = 5.00 ! in seconds
   real, parameter :: MAX_TIME_TEST   = 0.11 ! in seconds

contains

!   function newRobustRunner(remoteRunCommand,maxLaunchDuration) result(runner)
!      type (RobustRunner) :: runner
!      character(len=*), intent(in) :: remoteRunCommand
!      real, optional, intent(in) :: maxLaunchDuration
!
!      if(.not.present(maxLaunchDuration))then
!         runner%maxLaunchDuration = MAX_TIME_LAUNCH
!      else
!         runner%maxLaunchDuration = maxLaunchDuration
!      end if
!      
!      runner%remoteRunCommand = trim(remoteRunCommand)
!      allocate(runner%extListeners(0))
!   end function newRobustRunner

   function newRobustRunner_extListeners( &
        & remoteRunCommand   &
        & ,extListeners      &
        & ,maxLaunchDuration &
        & ,maxTimeoutDuration &
        & ) result(runner)
      type (RobustRunner) :: runner
      character(len=*), intent(in) :: remoteRunCommand
      type(ListenerPointer), optional, intent(in) :: extListeners(:)
      
      real, optional, intent(in) :: maxLaunchDuration
      real, optional, intent(in) :: maxTimeoutDuration

      if(.not.present(maxLaunchDuration))then
         runner%maxLaunchDuration = MAX_TIME_LAUNCH
      else
         runner%maxLaunchDuration = maxLaunchDuration
      end if

      if(.not.present(maxTimeoutDuration))then
         runner%maxTimeoutDuration = MAX_TIME_TEST
      else
         runner%maxTimeoutDuration = maxTimeoutDuration
      end if

      if(present(extListeners))then
         allocate(runner%extListeners(size(extListeners)), source=extListeners)
      end if
      
      runner%remoteRunCommand = trim(remoteRunCommand)
      runner%numSkip = 0
      
   end function newRobustRunner_extListeners

   subroutine runMethod(this)
      class (TestCaseMonitor), intent(inout) :: this
     _UNUSED_DUMMY(this)
   end subroutine runMethod

   function run(this, aTest, context) result(result)
      use PF_Test
      use PF_TestSuite
      use PF_TestResult
      use PF_ParallelContext

      type (TestResult) :: result
      class (RobustRunner), target, intent(inout) :: this
      class (Test), intent(inout) :: aTest
      class (ParallelContext), intent(in) :: context

      result = this%createTestResult()
      call result%setName(aTest%getName())
      call this%runWithResult(aTest, context, result)

   end function run

   subroutine runWithResult(this, aTest, context, result)
      use PF_Test
      use PF_ParallelContext
      use PF_TestResult
      use PF_RemoteProxyTestCase
      use PF_TestSuite
      use PF_TestVector
      use PF_ExceptionList
      class (RobustRunner), target, intent(inout) :: this
      class (Test), intent(inout) :: aTest
      class (ParallelContext), intent(in) :: context
      type (TestResult), intent(inout) :: result

      type (TestVector) :: testCases
      type (RemoteProxyTestCase) :: proxy
      integer :: i
      integer :: clockStart, clockStop, clockRate

      call system_clock(clockStart)

      do i=1,size(this%extListeners)
         call result%addListener(this%extListeners(i)%pListener)
      end do
      call result%addListener( this ) ! - monitoring

      select type (aTest)
      class is (TestSuite)
         call aTest%getTestCases(testCases)
      class is (TestCase)
         call testCases%push_back(aTest)
      class default
         stop
      end select

! mlr q: set up named pipes or units to handle comm between remote processes
      ! mlr q: and the root... being done at ukmet?
      do i = 1, testCases%size()
         if (.not. this%remoteProcess%isActive()) then
            call this%launchRemoteRunner(numSkip=i-1)
         end if
         proxy = RemoteProxyTestCase( &
              &     testCases%at(i) &
              &     ,this%remoteProcess &
              &     ,maxTimeoutDuration=this%maxTimeoutDuration &
              &  )
         call proxy%run(result, context)
      end do

      call system_clock(clockStop, clockRate)

      call result%addRunTime(real(clockStop - clockStart) / clockRate)

      ! Maybe push this call up into parent, i.e. loop over all of the listeners there...
      if (context%isRootProcess())  then
         do i=1,size(this%extListeners)
            call this%extListeners(i)%pListener%endRun(result)
         end do
      end if

   end subroutine runWithResult

   subroutine launchRemoteRunner(this, numSkip)
      use PF_UnixProcess
      use PF_ExceptionList
      class (RobustRunner), intent(inout) :: this
      integer, intent(in) :: numSkip

      character(len=:), allocatable :: command

      integer, parameter :: MAX_LEN=8
      character(len=MAX_LEN) :: suffix

      character(len=80) :: timeCommand
      type (UnixProcess) :: timerProcess
      character(len=:), allocatable :: line
      character(len=100) :: throwMessage

      
      write(suffix,'(i0)') numSkip
      command = trim(this%remoteRunCommand) // ' --skip ' // suffix


      this%remoteProcess = UnixProcess(command, runInBackground=.true.)

      ! Check for successful launch - prevents MPI launch time from counting against
      ! first test's time limit.
      write(timeCommand,'(a, f10.3,a,i0,a)') &
           & "(sleep ",this%maxLaunchDuration," && kill -9 ", &
           & this%remoteProcess%getPid(), &
           & ") > /dev/null 2>&1"
      timerProcess = UnixProcess(trim(timeCommand), runInBackground=.true.)

      do
         line = this%remoteProcess%getLine()
         if (len(line) == 0) then
            if (.not. this%remoteProcess%isActive()) then
               write(throwMessage,'(a,f0.3,a)') &
                    & ' (max launch duration = ',this%maxLaunchDuration,')'
               call throw('RUNTIME-ERROR: terminated before starting'//trim(throwMessage))
               call timerProcess%terminate()
               return
            else
!!$               call timerProcess%terminate()
!!$               timerProcess = UnixProcess(trim(timeCommand), runInBackground=.true.)
               cycle ! might just not be ready yet
            end if
         else
            if ('*LAUNCHED*' /= line) then
               call throw(&
	       &    'Failure to launch in RobustRunner. ' &
	       &    //"Expected: '*LAUNCHED*' Found: '"//line//"'" )
               return
            else
               ! successfully launched
               call timerProcess%terminate()
               exit
            end if
         end if
      end do

   end subroutine launchRemoteRunner

   ! No matter what, we don't want to rerun this test, so
   ! we need to increment numSkip here.
   subroutine startTest(this, testName)
      class (RobustRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName
      
     _UNUSED_DUMMY(testName)
      this%numSkip = this%numSkip + 1

   end subroutine startTest

   subroutine endTest(this, testName)
      class (RobustRunner), intent(inout) :: this
      character(len=*), intent(in) :: testName

     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(testName)

   end subroutine endTest

   subroutine endRun(this, result)
     use PF_AbstractTestResult
     class (RobustRunner), intent(inout) :: this
     class (AbstractTestResult), intent(in) :: result

     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(result)

   end subroutine endRun

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

   function createTestResult(this) result(tstResult)
      use PF_TestResult
      class (RobustRunner), intent(inout) :: this
      type (TestResult) :: tstResult

     _UNUSED_DUMMY(this)

      tstResult = TestResult()

    end function createTestResult


   subroutine addSuccess(this, testName)
      class (RobustRunner), intent(inout) :: this
      character(*), intent(in) :: testName

     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(testName)

   end subroutine addSuccess

end module PF_RobustRunner
