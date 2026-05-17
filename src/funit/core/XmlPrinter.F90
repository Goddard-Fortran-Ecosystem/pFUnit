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
   use PF_AbstractPrinter
   implicit none
   private

   public :: XmlPrinter

   type :: SuiteInfo
      character(len=80) :: name = ''
      integer :: numTests = 0
      integer :: numErrors = 0
      integer :: numFailures = 0
      real :: totalTime = 0.0
   end type SuiteInfo

   type, extends(AbstractPrinter) :: XmlPrinter
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
      procedure :: printTestsuiteHeader
      procedure :: printFailure
      procedure :: printFailures
      procedure :: printExceptions
      procedure :: printSuccess
      procedure :: printSuccesses
      procedure :: printFooter
      procedure :: addSuccess
      procedure :: buildSuiteInfo
      procedure :: printOneSuite
   end type XmlPrinter

   interface XmlPrinter
      module procedure new_XmlPrinter_unit
   end interface

contains

   function new_XmlPrinter_unit(unit) result(printer)
      type (XmlPrinter) :: printer
      integer, intent(in) :: unit

      printer%unit = unit

    end function new_XmlPrinter_unit

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

   subroutine endRun(this, result, elapsed_time)
     use PF_AbstractTestResult, only : AbstractTestResult
     class (XmlPrinter), intent(inOut) :: this
     class (AbstractTestResult), intent(in) :: result
      real, intent(in) :: elapsed_time

     call this%print(result, elapsed_time)

   end subroutine endRun

   subroutine print(this, result, elapsed_time)
      use PF_AbstractTestResult, only : AbstractTestResult
      use PF_TestFailureVector
      use PF_TestFailure
      class(XmlPrinter), intent(in) :: this
      class(AbstractTestResult), intent(in) :: result
      real, intent(in) :: elapsed_time

      type(TestFailureVector) :: successes, errors, failures
      type(SuiteInfo), dimension(:), allocatable :: suites
      integer :: numSuites, i

      _UNUSED_DUMMY(elapsed_time)

      successes = result%getSuccesses()
      errors = result%getErrors()
      failures = result%getFailures()

      call this%buildSuiteInfo(successes, errors, failures, suites, numSuites)

      call this%printHeader(result)
      do i = 1, numSuites
         call this%printOneSuite(suites(i), successes, errors, failures)
      end do
      call this%printFooter(result)
      flush(this%unit)

   end subroutine print

   subroutine printHeader(this, result)
      use PF_AbstractTestResult, only : AbstractTestResult
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      _UNUSED_DUMMY(result)

      write(this%unit,'(a)') '<?xml version="1.0" encoding="UTF-8"?>'
      write(this%unit,'(a)') '<testsuites>'

   end subroutine printHeader

   subroutine printTestsuiteHeader(this, suiteName, numTests, numErrors, numFailures, totalTime)
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: suiteName
      integer, intent(in) :: numTests, numErrors, numFailures
      real, intent(in) :: totalTime

      write(this%unit,'(a,a,a,i0,a,i0,a,i0,a,f0.4,a)') &
           '<testsuite name="', cleanXml(suiteName), &
           '" errors="', numErrors, &
           '" failures="', numFailures, &
           '" tests="', numTests, &
           '" time="', totalTime, '">'

   end subroutine printTestsuiteHeader

   subroutine printFailure(this, label, aFailedTest)
      use PF_TestFailure
      use PF_SourceLocation
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailure), intent(in) :: aFailedTest

      call this%printExceptions(label,aFailedTest%testName,&
           aFailedTest%exceptions, aFailedTest%time)

   end subroutine printFailure

   subroutine printExceptions(this, label, testName, exceptions, test_time)
      use PF_TestFailure
      use PF_SourceLocation
      use PF_ExceptionList
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      character(len=*), intent(in) :: testName
      type(ExceptionList), intent(in) :: exceptions
      real, intent(in) :: test_time

      class(Exception), pointer  :: pException
      character(:), allocatable :: methodName

      integer :: j
      character(len=80) :: locationString

      methodName = getTestName(testName)

      ! Write testcase opening tag
      write(this%unit,'(a,a,a,f0.4,a)') '<testcase name="', &
           cleanXml(trim(methodName)), '" time="', test_time, '">'

      ! Write failure/error elements
      do j= 1, exceptions%size()
         pException => exceptions%at(j)
         locationString = pException%location%toString()

         write(this%unit,'(a,a,a)',advance='no') '<', cleanXml(label), ' message="'
         write(this%unit,'(a,a,a)',advance='no') &
              'Location: ', cleanXml(trim(locationString)), ', '
         write(this%unit,'(a)',advance='no') &
              cleanXml(trim(pException%getMessage()))
         write(this%unit,*) '"/>'
      end do
      write(this%unit,'(a)') '</testcase>'

   end subroutine printExceptions

   subroutine printFailures(this, label, failures, suiteName)
      use PF_TestFailure
      use PF_TestFailureVector
      use PF_SourceLocation
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailureVector), intent(in) :: failures
      character(len=*), intent(in), optional :: suiteName

      integer :: i
      type(TestFailure) :: aTest

      do i = 1, failures%size()
         aTest = failures%at(i)
         if (present(suiteName)) then
            if (trim(getSuiteName(aTest%testName)) == trim(suiteName)) then
               call this%printFailure(label, aTest)
            end if
         else
            call this%printFailure(label, aTest)
         end if
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
      character(:), allocatable :: methodName

      methodName = getTestName(aSuccessTest%testName)

      write(this%unit,'(a,a,a,f0.4,a)') '<testcase name="',&
           cleanXml(trim(methodName)), '" time="', aSuccessTest%time, '"/>'

   end subroutine printSuccess

   subroutine printSuccesses(this, successes, suiteName)
      use PF_TestFailure
      use PF_TestFailurevector
      class (XmlPrinter), intent(in) :: this
      type (TestFailureVector), intent(in) :: successes
      character(len=*), intent(in), optional :: suiteName

      integer :: i
      type(TestFailure) :: aTest

      do i = 1, successes%size()
         aTest = successes%at(i)
         if (present(suiteName)) then
            if (trim(getSuiteName(aTest%testName)) == trim(suiteName)) then
               call this%printSuccess(aTest)
            end if
         else
            call this%printSuccess(aTest)
         end if
      end do

   end subroutine printSuccesses

   subroutine printFooter(this, result)
      use PF_AbstractTestResult
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      _UNUSED_DUMMY(result)

      write(this%unit,'(a)') '</testsuites>'

   end subroutine printFooter

   !> Collects per-suite information from all test result vectors.
   !! Suite names are extracted from the portion of the test name preceding the
   !! final dot ("suiteName.testName").
   !!
   !! @param[in]  successes  TestFailureVector of passing test results
   !! @param[in]  errors     TestFailureVector of errored test results
   !! @param[in]  failures   TestFailureVector of failed test results
   !! @param[out] suites     Array of SuiteInfo, one entry per unique suite
   !! @param[out] numSuites  Number of unique suites found
   subroutine buildSuiteInfo(this, successes, errors, failures, suites, numSuites)
      use PF_TestFailureVector
      use PF_TestFailure
      class(XmlPrinter), intent(in) :: this
      type(TestFailureVector), intent(in) :: successes, errors, failures
      type(SuiteInfo), dimension(:), allocatable, intent(out) :: suites
      integer, intent(out) :: numSuites

      integer :: totalTests, i, idx
      type(TestFailure) :: aTest
      character(len=80) :: sName

      _UNUSED_DUMMY(this)

      totalTests = successes%size() + errors%size() + failures%size()
      allocate(suites(max(totalTests, 1)))
      numSuites = 0

      if (totalTests == 0) return

      do i = 1, successes%size()
         aTest = successes%at(i)
         sName = getSuiteName(aTest%testName)
         idx = findOrAddSuite(suites, numSuites, sName)
         suites(idx)%numTests = suites(idx)%numTests + 1
         suites(idx)%totalTime = suites(idx)%totalTime + aTest%time
      end do

      do i = 1, errors%size()
         aTest = errors%at(i)
         sName = getSuiteName(aTest%testName)
         idx = findOrAddSuite(suites, numSuites, sName)
         suites(idx)%numTests = suites(idx)%numTests + 1
         suites(idx)%numErrors = suites(idx)%numErrors + 1
         suites(idx)%totalTime = suites(idx)%totalTime + aTest%time
      end do

      do i = 1, failures%size()
         aTest = failures%at(i)
         sName = getSuiteName(aTest%testName)
         idx = findOrAddSuite(suites, numSuites, sName)
         suites(idx)%numTests = suites(idx)%numTests + 1
         suites(idx)%numFailures = suites(idx)%numFailures + 1
         suites(idx)%totalTime = suites(idx)%totalTime + aTest%time
      end do

   contains

      function findOrAddSuite(suites, numSuites, name) result(idx)
         type(SuiteInfo), dimension(:), intent(inout) :: suites
         integer, intent(inout) :: numSuites
         character(len=*), intent(in) :: name
         integer :: idx, j

         do j = 1, numSuites
            if (trim(suites(j)%name) == trim(name)) then
               idx = j
               return
            end if
         end do

         numSuites = numSuites + 1
         idx = numSuites
         suites(idx)%name = name
      end function findOrAddSuite

   end subroutine buildSuiteInfo

   !> Writes a single `<testsuite>` XML element and its child `<testcase>` elements,
   !! filtering the relevant tests by suite name from the full result vectors.
   !!
   !! @param[in] suite     SuiteInfo containing pre-computed suite name and info
   !! @param[in] successes TestFailureVector of passing test results
   !! @param[in] errors    TestFailureVector of errored test results
   !! @param[in] failures  TestFailureVector of failed test results
   subroutine printOneSuite(this, suite, successes, errors, failures)
      use PF_TestFailureVector
      use PF_TestFailure
      class(XmlPrinter), intent(in) :: this
      type(SuiteInfo), intent(in) :: suite
      type(TestFailureVector), intent(in) :: successes, errors, failures

      call this%printTestsuiteHeader(trim(suite%name), suite%numTests, &
           suite%numErrors, suite%numFailures, suite%totalTime)

      call this%printSuccesses(successes, trim(suite%name))
      call this%printFailures('error', errors, trim(suite%name))
      call this%printFailures('failure', failures, trim(suite%name))

      write(this%unit,'(a)') '</testsuite>'

   end subroutine printOneSuite

   !> Extracts the suite name from a dot-qualified test name.
   !!
   !! @param[in] testName  Full test name in "suiteName.testName" format
   !! @return              Suite name, or empty string if no dot is present
   function getSuiteName(testName) result(name)
      character(len=*), intent(in) :: testName
      character(:), allocatable :: name
      integer :: dot_pos

      dot_pos = index(testName, '.', back=.true.)
      if (dot_pos > 0) then
         name = testName(1:dot_pos-1)
      else
         name = ''
      end if
   end function getSuiteName

   !> Extracts the test name from a dot-qualified test name.
   !!
   !! @param[in] testName  Full test name in "suiteName.testName" format
   !! @return              Test name, or the full input if no dot is present
   function getTestName(testName) result(name)
      character(len=*), intent(in) :: testName
      character(:), allocatable :: name
      integer :: dot_pos

      dot_pos = index(testName, '.', back=.true.)
      if (dot_pos > 0) then
         name = testName(dot_pos+1:)
      else
         name = testName
      end if
   end function getTestName

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
