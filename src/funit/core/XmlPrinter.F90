#include "unused_dummy.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
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
! 2014 June 4 ML Rilee
!    Added intermediate status output. Refactored prints to handle both single
!    and arrays of Failure and Success.  Exceptions can be printed too. Quotes 
!    are not handled well: need to consider going to "&quot;" and "&apos;".
!    May need to separate status reports from the end-of-run summary
!
!-------------------------------------------------------------------------------
module PF_XmlPrinter
   use PF_Exception
   use PF_TestListener
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
      procedure :: addSuccess
   end type XmlPrinter

contains

   function newXmlPrinter(unit)
      type (XmlPrinter) :: newXmlPrinter
      integer, intent(in) :: unit

      newXmlPrinter%unit = unit

   end function newXmlPrinter

   subroutine addFailure(this, testName, exceptions)
      use PF_ExceptionList
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
      _UNUSED_DUMMY(exceptions)
   end subroutine addFailure

   subroutine addError(this, testName, exceptions)
      use PF_ExceptionList
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
      _UNUSED_DUMMY(exceptions)
   end subroutine addError

   subroutine startTest(this, testName)
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
   end subroutine startTest

   subroutine endTest(this, testName)
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
   end subroutine endTest

   subroutine endRun(this, result)
     use PF_AbstractTestResult, only : AbstractTestResult
     class (XmlPrinter), intent(inOut) :: this
     class (AbstractTestResult), intent(in) :: result

     call this%print(result)
   end subroutine endRun

   subroutine print(this, result)
      use PF_AbstractTestResult, only : AbstractTestResult
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      call this%printHeader(result)
      call this%printSuccesses(result%getSuccesses())
      call this%printFailures('error', result%getErrors())
      call this%printFailures('failure', result%getFailures())
      call this%printFooter(result)

   end subroutine print

   subroutine printHeader(this, result)
      use PF_AbstractTestResult, only : AbstractTestResult
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      write(this%unit,'(a,a,a,i0,a,i0,a,i0,a,f0.4,a)') &
           '<testsuite name="', cleanXml(trim(result%getName())), &
           '" errors="', result%errorCount(),&
           '" failures="', result%failureCount(),&
           '" tests="', result%runCount(),&
           '" time="', result%getRunTime(), '">'

      flush(this%unit)

   end subroutine printHeader

   subroutine printFailure(this, label, aFailedTest)
      use PF_TestFailure
      use PF_SourceLocation
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailure), intent(in) :: aFailedTest

      call this%printExceptions(label,aFailedTest%testName,&
           aFailedTest%exceptions)

   end subroutine printFailure

   subroutine printExceptions(this, label, testName, exceptions)
      use PF_TestFailure
      use PF_SourceLocation
      use PF_ExceptionList
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      character(len=*), intent(in) :: testName
      type(ExceptionList), intent(in) :: exceptions

      class(Exception), pointer  :: pException

      integer :: j
      character(len=80) :: locationString

!mlr testcase should likely be testname or testmethod or maybe test
!mlr Q?  What does JUnit do?
!mlr  Ask Halvor -- good for 3.0
      write(this%unit,'(a,a,a)') '<testcase name="', &
           cleanXml(trim(testName)), '">'
      do j= 1, exceptions%size()
         pException => exceptions%at(j)
         locationString = pException%location%toString()

         write(this%unit,'(a,a,a)',advance='no') '<', cleanXml(label),&
              ' message="'
         write(this%unit,'(a,a,a)',advance='no') &
              'Location: ', cleanXml(trim(locationString)), ', '
         write(this%unit,'(a)',advance='no') &
              cleanXml(trim(pException%getMessage()))
         write(this%unit,*) '"/>'
      end do
      write(this%unit,'(a)') '</testcase>'

      flush(this%unit)

   end subroutine printExceptions


!mlr old version
   subroutine printFailure1(this, label, aFailedTest)
      use PF_TestFailure
      use PF_SourceLocation
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailure), intent(in) :: aFailedTest
      class (Exception), pointer :: pException

      integer :: j
      character(len=80) :: locationString

!mlr testcase should likely be testname or testmethod or maybe test
!mlr Q?  What does JUnit do?
!mlr  Ask Halvor -- good for 3.0
      write(this%unit,'(a,a,a)') '<testcase name="', &
           cleanXml(trim(aFailedTest%testName)), '">'
      do j= 1, aFailedTest%exceptions%size()
        pException => aFailedTest%exceptions%at(j)
        locationString = pException%location%toString()

        write(this%unit,'(a,a,a)',advance='no') &
             '<', cleanXml(label), ' message="'
        write(this%unit,'(a,a,a)',advance='no') &
             'Location: ', cleanXml(trim(locationString)), ', '
        write(this%unit,'(a)',advance='no') &
             cleanXml(trim(pException%getMessage()))
        write(this%unit,*) '"/>'
      end do
      write(this%unit,'(a)') '</testcase>'

      flush(this%unit)

   end subroutine printFailure1

   subroutine printFailures(this, label, failures)
      use PF_TestFailure
      use PF_TestFailureVector
      use PF_SourceLocation
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailureVector), intent(in) :: failures

      integer :: i

      do i = 1, failures%size()
         call this%printFailure(label,failures%at(i))
      end do

   end subroutine printFailures

   subroutine printTestName(this, testName)
      use PF_TestFailure
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: testName

      write(this%unit,'(a,a,a)') '<testcase name="',&
           cleanXml(trim(testName)), '"/>'

      flush(this%unit)

    end subroutine printTestName

   subroutine printSuccess(this, aSuccessTest)
      use PF_TestFailure
      class (XmlPrinter), intent(in) :: this
      type (TestFailure) :: aSuccessTest

!      character(len=80) :: locationString

      write(this%unit,'(a,a,a)') '<testcase name="',&
           cleanXml(trim(aSuccessTest%testName)), '"/>'

      flush(this%unit)

   end subroutine printSuccess

   subroutine printSuccesses(this, successes)
      use PF_TestFailure
      use PF_TestFailurevector
      class (XmlPrinter), intent(in) :: this
      type (TestFailureVector), intent(in) :: successes

      integer :: i

      do i = 1, successes%size()
         call this%printSuccess(successes%at(i))
      end do

   end subroutine printSuccesses

   subroutine printFooter(this, result)
      use PF_AbstractTestResult
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      _UNUSED_DUMMY(result)

      write(this%unit,'(a)') '</testsuite>'
      flush(this%unit)

   end subroutine printFooter

   function cleanXml(string_in) result(out)
      character(len=*), intent(in) :: string_in
      character(:), allocatable :: out

      out = string_in
      out = replaceAll(out, '<', '[')
      out = replaceAll(out, '>', ']')
      out = replaceAll(out, '"', "'")
   end function cleanXml

   function replaceAll(string_in, search, replace) result(out)
      character(len=*), intent(in) :: string_in
      character, intent(in) :: search, replace
      character(:), allocatable :: out
      integer :: i
      out = string_in
      i = index(out, search)
      do while(i /= 0)
         out = out(:i-1) // replace // out(i+1:)
         i = index(out, search)
      end do
   end function replaceAll

   subroutine addSuccess(this, testName)
      class (XmlPrinter), intent(inout) :: this
      character(*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)

   end subroutine addSuccess

end module PF_XmlPrinter
