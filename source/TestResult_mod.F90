!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: TestResult_mod
!
!> @brief
!! Gathers data about successes and failures for a series of runs.  At this 
!! time, it can provide a Summary() which is the total number of runs and the 
!! number of failures (unsuccessful runs).  Report() provides additional details
!! about the particulars of the failures.
!! Two distinct reporting options are available.  MODE_USE_BUFFER, the default, 
!! is essential for internal testing of funit.  MODE_USE_STDOUT sends failure
!! messages directly to stdout which might be preferred in some instances.
!! 
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 16 Oct 2006
!!
! REVISION HISTORY:
! 16 Oct 2006 - Initial Version
! 13 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module TestResult_mod
   use Assert_mod
   use Params_mod, only: MAX_LEN_MSG, MAX_LEN_NAME
   use Report_mod
   use Exception_mod
   implicit none
   private

   public :: TestResult_type
   public :: NewTestResult
   public :: numRun
   public :: numFailed
   public :: numSevere
   public :: mode

   public :: Summary
   public :: GenerateReport

   public :: SetReportMode
   public :: TestStarted
   public :: TestFailed
   public :: PushPrefix
   public :: PopPrefix
   public :: Clean
  
   public :: MODE_USE_BUFFER
   public :: MODE_USE_STDOUT
   public :: MODE_USE_LOGFILE

   integer, parameter :: MODE_USE_BUFFER = 1
   integer, parameter :: MODE_USE_STDOUT = 2
   integer, parameter :: MODE_USE_LOGFILE = 4

   integer, parameter :: MAX_ERRORS      = 100
   integer, parameter :: MAX_LEN_SUMMARY = 100
   integer, parameter :: MAX_DEPTH    = 10

   integer, parameter :: iNaN = 1-Huge(1)

   type TestResult_type
      private
      integer :: runCount  =iNaN
      integer :: errorCount=iNaN
      integer :: msgCount=iNaN
      integer :: numSevereExceptions = iNaN
      integer :: numMildExceptions   = iNaN
      integer :: mode      = MODE_USE_STDOUT
      type (Report_type) :: report
      integer :: depth =0
      character(len=MAX_LEN_NAME) :: prefix(MAX_DEPTH)
      integer :: clockStart
      integer :: logUnit
   end type TestResult_type

   interface Clean
      module procedure clean_
   end interface

   interface TestFailed
      module procedure TestFailed_string
      module procedure TestFailed_report
   end interface

   interface GenerateReport
      module procedure GenerateReporTestResult_type
   end interface
  
   interface summary
      module procedure summary_
   end interface

contains

   !---------------------------------------------------------------------------
   !> Creates the default constructor with given optional mode for the test 
   !! result.
   !!
   !! @param mode - given optional mode (default = MODE_USE_BUFFER)
   !!
   !! @return new object of test result
   !---------------------------------------------------------------------------
   function NewTestResult(mode) result(tr)
      use IO_Utilities_mod
      integer, optional, intent(In) :: mode
      type (TestResult_type) :: tr

      tr%runCount=0
      tr%errorCount=0
      tr%msgCount=0
      tr%numSevereExceptions=0
      tr%numMildExceptions=0

      if (present(mode)) then
         call setReportMode(tr, mode)
      else
         tr%mode = MODE_USE_BUFFER
      end if

      tr%report = Report()

      call system_clock(tr%clockStart)

      if (iand(tr % mode, MODE_USE_LOGFILE) /= 0) then
         tr % logUnit = openFile('.pFUnitLog', status='unknown', form='formatted')
      end if

   end function NewTestResult

   !---------------------------------------------------------------------------
   !> Checks the vailidity on the input mode and then sets the report mode to 
   !! the object.
   !!
   !! @param mode - this object of test result 
   !! @param mode - given mode 
   !!
   !! @throws RaiseException if the input mode is invalid
   !---------------------------------------------------------------------------
   subroutine SetReportMode(this, mode)
      type (TestResult_type) :: this
      integer           :: mode

      ! Check validity
      if (mode <= 0 .or. mode >= (MODE_USE_BUFFER + MODE_USE_STDOUT + MODE_USE_LOGFILE)) then
         call RaiseException('Invalid mode.')
      end if

      this%mode = mode

   end subroutine SetReportMode

   !---------------------------------------------------------------------------
   !> Gets the current mode.
   !! the object.
   !!
   !! @param mode - this object of test result 
   !!
   !! @return mode 
   !---------------------------------------------------------------------------
   integer function mode(this)
      type (TestResult_type) :: this
      mode = this%mode
   end function mode

   !---------------------------------------------------------------------------
   !> Gets the number of runs for test result
   !!
   !! @param mode - this object of test result 
   !!
   !! @return number of runs for test result 
   !---------------------------------------------------------------------------
   integer function numRun(this)
      type (TestResult_type) :: this
      numRun = this%runCount
   end function numRun

   !---------------------------------------------------------------------------
   !> Gets the number of failures from test result
   !!
   !! @param mode - this object of test result 
   !!
   !! @return number of failures from test result 
   !---------------------------------------------------------------------------
   integer function numFailed(this)
      type (TestResult_type) :: this
      numFailed = this%errorCount
   end function numFailed

   !---------------------------------------------------------------------------
   !> Gets the number of severity
   !!
   !! @param mode - this object of test result 
   !!
   !! @return number of severity
   !---------------------------------------------------------------------------
   integer function numSevere(this)
      type (TestResult_type) :: this
      numSevere = this%numSevereExceptions
   end function numSevere

   !---------------------------------------------------------------------------
   !> Starts the test by increasing the number of run and writes the log if 
   !! mode is selected.
   !!
   !! @param mode - this object of test result 
   !! @param name - given optional name of test
   !!
   !---------------------------------------------------------------------------
   subroutine TestStarted(this, name)
      type (TestResult_type) :: this
      character(len=*), optional, intent(in) :: name

      this%runCount = this%runCount + 1
      if (iand(this % mode, MODE_USE_LOGFILE) /= 0) then
         if (present(name)) write(this % logUnit, *)'started ' // trim(name)
      end if

   end subroutine TestStarted

   !---------------------------------------------------------------------------
   !> Reports the test failed by the given test name and message.
   !!
   !! @param this - this object of test result 
   !! @param testname - given test name
   !! @param message - identifying message for test failed
   !---------------------------------------------------------------------------
   subroutine TestFailed_string(this, testname, message)
      type (TestResult_type)          :: this
      character(len=*), intent(In) :: testname
      character(len=*), intent(in) :: message

      character(len=*), parameter :: fmt = '("Failure in ",2a," - ",a)'

      character(len=MAX_LEN_MSG) :: formattedMessage

      this%errorCount = this%errorCount + 1
      this%msgCount = this%msgCount + 1
      this%numSevereExceptions = this%numSevereExceptions + 1

      write(formattedMessage, fmt) trim(GetPrefix(this)), trim(testname), trim(message)

      if (iand(this % mode, MODE_USE_BUFFER) /= 0) then
         call append(this%report, formattedMessage)
      else
         write(*,*)' '
         write(*,'(a)') trim(formattedMessage)
      end if

      call logFailure(this)

   end subroutine TestFailed_string

   !---------------------------------------------------------------------------
   !> Reports the test failed by the given test name and report.
   !!
   !! @param this - this object of test result 
   !! @param testname - given test name
   !! @param message - given report for test failed
   !---------------------------------------------------------------------------
   subroutine TestFailed_report(this, testname, testReport)
      type (TestResult_type)          :: this
      character(len=*), intent(In) :: testname
      type (Report_type) :: testReport

      character(len=*), parameter :: fmt_a = '(i0," failures in ",2a,":")'
      character(len=*), parameter :: fmt_b = '(a,a)'
      character(len=MAX_LEN_MSG) :: message

      integer :: i, n

      n = numLines(testReport)

      if ( n == 1) then
         call TestFailed_string(this, testName, getLine(testReport,1))
         return
      end if

      this%msgCount = this%msgCount + (1 + n)
      this%errorCount = this%errorCount + 1
      this%numSevereExceptions = this%numSevereExceptions + n
      this%numMildExceptions   = this%numMildExceptions   + 0 

      write(message, fmt_a) n, trim(GetPrefix(this)), trim(testName)
      if (iand(this%mode, MODE_USE_BUFFER) /= 0) then
         call Append(this%report, trim(message))
      else
         write(*,*)' '
         write(*,'(a)') trim(message)
      end if

      do i = 1, n
         write(message, fmt_b) '   - ', trim(GetLine(testReport, i))
         if (iand(this%mode, MODE_USE_BUFFER) /= 0) then
            call Append(this%report, trim(message))
         else
            write(*,'(a)') trim(message)
         end if
      end do

      call logFailure(this)
               
   end subroutine TestFailed_report

   subroutine logFailure(this)
     type (TestResult_type), intent(in) :: this
     
     if (iand(this % mode, MODE_USE_LOGFILE) /= 0) then
       write(this % logUnit,*) 'failed'
     end if
     
     if (iand(mode(this),MODE_USE_STDOUT) /= 0 ) then
       write(*,'("x")',advance='no')
     end if
    end subroutine logFailure

   !---------------------------------------------------------------------------
   !> Gets the prefix of the test result object
   !!
   !! @param this - this object of test result 
   !!
   !! @return get the prefix
   !---------------------------------------------------------------------------
   function GetPrefix(this) result (prefix)
      type (TestResult_type) :: this
      character(len=(MAX_LEN_NAME+2)*MAX_DEPTH) :: prefix
      integer :: i
    
      prefix=''
      do i = 1, this%depth
         prefix=trim(prefix)//trim(this%prefix(i))//'::'
      end do
    
   end function GetPrefix
  
   !---------------------------------------------------------------------------
   !> Writes the summary of test result by the given optional timer
   !!
   !! @param this - this object of test result 
   !! @param timer - given optional timer
   !!
   !! @return log for the summary
   !---------------------------------------------------------------------------
   function Summary_(this, timer) Result(log)
      type (TestResult_type) :: this
      logical, optional, intent(in) :: timer
      character(len=MAX_LEN_SUMMARY)    :: log
      character(len=20) :: fmt

      character(len=MAX_LEN_SUMMARY) :: extra
      real :: time
      integer :: clockStop
      integer :: clockRate
      integer :: numSeconds
      integer :: hundredths

      logical :: timer_

      write(log,'(i0,a,i0,a)') this % runCount, " run, ", this % errorCount, " failed"

      if (this%errorCount > 0) then
         write(extra,'(a,i0,a)')" (", this % numSevereExceptions, " severe)"
         log = trim(log) // trim(extra)
      end if

      timer_ = .true.
      if (present(timer)) timer_ = timer
      if (timer_) then
         call system_clock(clockStop, clockRate)
         time = real(clockStop - this % clockStart) / clockRate
         numSeconds = floor(time)
         hundredths = floor((time - numSeconds) * 100)
         write(extra,'(" ",i0,".",i2.2," seconds")') numSeconds, hundredths
         log = trim(log) // trim(extra)
      end if
   end function Summary_

   !---------------------------------------------------------------------------
   !> Generates the report from the test result
   !!
   !! @param this - this object of test result 
   !!
   !! @return generated report
   !---------------------------------------------------------------------------
   function GenerateReporTestResult_type(this) result(rpt)
      use Report_mod, only: copy
      type (TestResult_type) :: this
      type (Report_type)     :: rpt

      call copy(this % report, rpt)

   end function GenerateReporTestResult_type

   !---------------------------------------------------------------------------
   !> Pushs the prefix into the test result 
   !!
   !! @param this - this object of test result 
   !! @param prefix - given prefix
   !---------------------------------------------------------------------------
   subroutine PushPrefix(this, prefix)
      type (TestResult_type) :: this
      character(len=*)    :: prefix

      call assertTrue(this%depth < MAX_DEPTH)
      this%depth=this%depth+1
      this%prefix(this%depth)=trim(prefix)
   end subroutine PushPrefix

   !---------------------------------------------------------------------------
   !> Pops up the prefix from the test result 
   !!
   !! @param this - this object of test result 
   !---------------------------------------------------------------------------
   subroutine PopPrefix(this)
      type (TestResult_type) :: this

      call assertTrue(this%depth > 0)
      this%depth = this%depth-1

   end subroutine PopPrefix

   !---------------------------------------------------------------------------
   !> Clears up the test result object
   !!
   !! @param this - this object of test result 
   !---------------------------------------------------------------------------
   subroutine Clean_(this)
      use IO_Utilities_mod
      type (TestResult_type) :: this

      call clean(this%report)
      if (iand(this % mode, MODE_USE_LOGFILE) /= 0) close(this % logUnit)

   end subroutine Clean_

end module TestResult_mod
