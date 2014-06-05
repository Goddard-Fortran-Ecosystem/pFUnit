!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: XmlPrinter
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Halvor Lund, SINTEF Energy Research
!!
!! @date
!! 30 Jan 2014
!!
!! @note <A note here.>
!! Need to improve the handling of nested quotes.
!
! REVISION HISTORY:
!
!-------------------------------------------------------------------------------
module XmlPrinter_mod
   use Exception_mod
   use TestListener_mod
   implicit none
   private

   public :: XmlPrinter
   public :: newXmlPrinter

   type, extends(TestListener) :: XmlPrinter
      integer :: unit
      integer :: privateUnit
   contains
      procedure :: addFailure
      procedure :: addError
      procedure :: startTest
      procedure :: endTest
      procedure :: endRun
      procedure :: print
      procedure :: printHeader
      procedure :: printFailure
      procedure :: printFailures
      procedure :: printExceptions
      procedure :: printSuccess
      procedure :: printSuccesses
      procedure :: printFooter
   end type XmlPrinter

contains

   function newXmlPrinter(unit)
      type (XmlPrinter) :: newXmlPrinter
      integer, intent(in) :: unit

      newXmlPrinter%unit = unit

   end function newXmlPrinter

   subroutine addFailure(this, testName, exceptions)
      use Exception_mod
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName
      type (Exception), intent(in) :: exceptions(:)

      call this%printExceptions("failure",testName,exceptions)

   end subroutine addFailure

   subroutine addError(this, testName, exceptions)
      use Exception_mod
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName
      type (Exception), intent(in) :: exceptions(:)

      call this%printExceptions("error",testName,exceptions)

   end subroutine addError

   subroutine startTest(this, testName)
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName
      
      write(this%unit,'(a,a,a)') &
           & "<test name='",trim(testName), "'>"

      flush(this%unit)

   end subroutine startTest

   subroutine endTest(this, testName)
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName

      write(this%unit,'(a,a,a)') &
           & "</test name='",trim(testName), "'>"

      flush(this%unit)

   end subroutine endTest

   subroutine endRun(this, result)
     use AbstractTestResult_mod, only : AbstractTestResult
     class (XmlPrinter), intent(inOut) :: this
     class (AbstractTestResult), intent(in) :: result

     call this%print(result)

   end subroutine endRun

   subroutine print(this, result)
      use AbstractTestResult_mod
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      call this%printHeader(result)
      call this%printSuccesses(result%getSuccesses())
      call this%printFailures('error', result%getErrors())
      call this%printFailures('failure', result%getFailures())
      call this%printFooter(result)

   end subroutine print

   subroutine printHeader(this, result)
      use AbstractTestResult_mod
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      write(this%unit,'(a,i0,a,i0,a,i0,a)') &
           "<testsuite errors='", result%errorCount(),&
           "' failures='", result%failureCount(),&
           "' tests='", result%runCount(),"'>"

      flush(this%unit)

   end subroutine printHeader

   subroutine printFailure(this, label, aFailedTest)
      use TestFailure_mod
      use SourceLocation_mod
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailure), intent(in) :: aFailedTest

      integer :: i, j
      character(len=80) :: locationString

      call this%printExceptions(label,aFailedTest%testName,aFailedTest%exceptions)

    end subroutine printFailure

   subroutine printExceptions(this, label, testName, exceptions)
      use TestFailure_mod
      use SourceLocation_mod
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      character(len=*), intent(in) :: testName
      type(Exception), intent(in) :: exceptions(:)

      integer :: j
      character(len=80) :: locationString

!mlr testcase should likely be testname or testmethod or maybe test
!mlr Q?  What does JUnit do?
!mlr  Ask Halvor -- good for 3.0
      write(this%unit,'(a,a,a)') "<testcase name='", trim(testName), "'>"
      do j= 1, size(exceptions)
         locationString = toString(exceptions(j)%location)
         
         write(this%unit,'(a,a,a)',advance='no') '<', label, " message='"
         
         write(this%unit,'(a,a,a)',advance='no') &
              'Location: ', trim(locationString), ', '
         write(this%unit,'(a)',advance='no') &
              trim(exceptions(j)%getMessage())
         write(this%unit,*) "'/>"
      end do
      write(this%unit,'(a)') '</testcase>'
      
      flush(this%unit)

   contains

      function toString(location) result(string)
         type (SourceLocation), intent(in) :: location
         character(len=80) :: string

         if (location%fileName == UNKNOWN_FILE_NAME) then
            if (location%lineNumber == UNKNOWN_LINE_NUMBER) then
               string = '<unknown location>'
            else
               write(string,'(a,":",i0)') trim(UNKNOWN_FILE_NAME), location%lineNumber
            end if
         else
            if (location%lineNumber == UNKNOWN_LINE_NUMBER) then
               string = trim(location%fileName)
            else
               write(string,'(a,":",i0)') trim(location%fileName), location%lineNumber
            end if
         end if

         string = '[' // trim(string) // ']'

      end function toString

   end subroutine printExceptions


!mlr old version
   subroutine printFailure1(this, label, aFailedTest)
      use TestFailure_mod
      use SourceLocation_mod
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailure), intent(in) :: aFailedTest

      integer :: i, j
      character(len=80) :: locationString

!mlr testcase should likely be testname or testmethod or maybe test
!mlr Q?  What does JUnit do?
!mlr  Ask Halvor -- good for 3.0
      write(this%unit,'(a,a,a)') "<testcase name='", trim(aFailedTest%testName), "'>"
      do j= 1, size(aFailedTest%exceptions)
         locationString = toString(aFailedTest%exceptions(j)%location)
         
         write(this%unit,'(a,a,a)',advance='no') '<', label, " message='"
         
         write(this%unit,'(a,a,a)',advance='no') &
              'Location: ', trim(locationString), ', '
         write(this%unit,'(a)',advance='no') &
              trim(aFailedTest%exceptions(j)%getMessage())
         write(this%unit,*) "'/>"
      end do
      write(this%unit,'(a)') '</testcase>'
      
      flush(this%unit)

   contains

      function toString(location) result(string)
         type (SourceLocation), intent(in) :: location
         character(len=80) :: string

         if (location%fileName == UNKNOWN_FILE_NAME) then
            if (location%lineNumber == UNKNOWN_LINE_NUMBER) then
               string = '<unknown location>'
            else
               write(string,'(a,":",i0)') trim(UNKNOWN_FILE_NAME), location%lineNumber
            end if
         else
            if (location%lineNumber == UNKNOWN_LINE_NUMBER) then
               string = trim(location%fileName)
            else
               write(string,'(a,":",i0)') trim(location%fileName), location%lineNumber
            end if
         end if

         string = '[' // trim(string) // ']'

      end function toString

   end subroutine printFailure1

   subroutine printFailures(this, label, failures)
      use TestFailure_mod
      use SourceLocation_mod
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailure), intent(in) :: failures(:)

      integer :: i
      character(len=80) :: locationString

      do i = 1, size(failures)

         call this%printFailure(label,failures(i))

      end do

   end subroutine printFailures

   subroutine printTestName(this, testName)
      use TestFailure_mod
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: testName

      write(this%unit,'(a,a,a)') "<testcase name='", trim(testName), "'/>"

      flush(this%unit)

    end subroutine printTestName

   subroutine printSuccess(this, aSuccessTest)
      use TestFailure_mod
      class (XmlPrinter), intent(in) :: this
      type (TestFailure) :: aSuccessTest

      integer :: i
!      character(len=80) :: locationString

      write(this%unit,'(a,a,a)') "<testcase name='", trim(aSuccessTest%testName), "'/>"

      flush(this%unit)

   end subroutine printSuccess

   subroutine printSuccesses(this, successes)
      use TestFailure_mod
      class (XmlPrinter), intent(in) :: this
      type (TestFailure), intent(in) :: successes(:)

      integer :: i
!      character(len=80) :: locationString

      do i = 1, size(successes)
         call this%printSuccess(successes(i))
      end do

   end subroutine printSuccesses

   subroutine printFooter(this, result)
      use AbstractTestResult_mod
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      write(this%unit,'(a)') '</testsuite>'

      flush(this%unit)

   end subroutine printFooter

end module XmlPrinter_mod
