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
!! <Or starting here...>
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
      procedure :: printFailures
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

   end subroutine addFailure

   subroutine addError(this, testName, exceptions)
      use Exception_mod
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName
      type (Exception), intent(in) :: exceptions(:)

   end subroutine addError

   subroutine startTest(this, testName)
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName

   end subroutine startTest

   subroutine endTest(this, testName)
      class (XmlPrinter), intent(inOut) :: this
      character(len=*), intent(in) :: testName

   end subroutine endTest

   subroutine endRun(this, name, result)
     use AbstractTestResult_mod, only : AbstractTestResult
     class (XmlPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: name
     class (AbstractTestResult), intent(in) :: result
     call this%print(name, result)
   end subroutine endRun

   subroutine print(this, name, result)
      use AbstractTestResult_mod, only : AbstractTestResult
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: name
      class (AbstractTestResult), intent(in) :: result

      call this%printHeader(name, result)
      call this%printSuccesses(result%getSuccesses())
      call this%printFailures('error', result%getErrors())
      call this%printFailures('failure', result%getFailures())
      call this%printFooter(result)

   end subroutine print

   subroutine printHeader(this, name, result)
      use AbstractTestResult_mod, only : AbstractTestResult
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: name
      class (AbstractTestResult), intent(in) :: result

      write(this%unit,'(a,a,a,i0,a,i0,a,i0,a,f8.4,a)') &
           '<testsuite name="', name, &
           '" errors="', result%errorCount(),&
           '" failures="', result%failureCount(),&
           '" tests="', result%runCount(),&
           '" time="', result%getRunTime(), '">'

   end subroutine printHeader

   subroutine printFailures(this, label, failures)
      use AbstractTestResult_mod
      use TestFailure_mod
      use SourceLocation_mod
      class (XmlPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailure), intent(in) :: failures(:)

      type (TestFailure) :: aFailedTest
      integer :: i, j
      character(len=80) :: locationString

      do i = 1, size(failures)
         aFailedTest = failures(i)

!mlr testcase should likely be testname or testmethod or maybe test
!mlr Q?  What does JUnit do?
!mlr  Ask Halvor -- good for 3.0
         write(this%unit,'(a,a,a)') '<testcase name="', trim(aFailedTest%testName), '">'
         do j= 1, size(aFailedTest%exceptions)
            locationString = toString(aFailedTest%exceptions(j)%location)

            write(this%unit,'(a,a,a)',advance='no') '<', label, ' message="'

            write(this%unit,'(a,a,a)',advance='no') &
                 'Location: ', trim(locationString), ', '
            write(this%unit,'(a)',advance='no') &
                 removeTags(trim(aFailedTest%exceptions(j)%getMessage()))
            write(this%unit,*) '"/>'
         end do
         write(this%unit,'(a)') '</testcase>'
      end do

   contains

      function toString(location) result(string)
         type (SourceLocation), intent(in) :: location
         character(len=80) :: string

         if (location%fileName == UNKNOWN_FILE_NAME) then
            if (location%lineNumber == UNKNOWN_LINE_NUMBER) then
               string = '[unknown location]'
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

   end subroutine printFailures

   subroutine printSuccesses(this, successes)
      use TestFailure_mod
      class (XmlPrinter), intent(in) :: this
      type (TestFailure), intent(in) :: successes(:)

      type (TestFailure) :: aSuccessTest
      integer :: i
!      character(len=80) :: locationString

      do i = 1, size(successes)
         aSuccessTest = successes(i)

         write(this%unit,'(a,a,a)') '<testcase name="', trim(aSuccessTest%testName), '"/>'
      end do
   end subroutine printSuccesses

   subroutine printFooter(this, result)
      use AbstractTestResult_mod
      class (XmlPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      write(this%unit,'(a)') '</testsuite>'

   end subroutine printFooter

   function removeTags(string_in) result(out)
      character(len=*), intent(in) :: string_in
      character(:), allocatable :: out
      integer :: i
      out = string_in
      out = replaceAll(out, '<', '[')
      out = replaceAll(out, '>', ']')
      out = replaceAll(out, '"', "'")
   end function removeTags

   function replaceAll(string_in, from, to) result(out)
      character(len=*), intent(in) :: string_in
      character, intent(in) :: from, to
      character(:), allocatable :: out
      integer :: i
      out = string_in
      i = index(out, from)
      do while(i /= 0)
         out = out(:i-1) // to // out(i+1:)
         i = index(out, from)
      end do
   end function replaceAll
end module XmlPrinter_mod
