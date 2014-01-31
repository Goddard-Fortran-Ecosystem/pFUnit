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
      procedure :: print
      procedure :: printHeader
      procedure :: printFailures
      procedure :: printFooter
   end type XmlPrinter

   integer, parameter :: MAX_COLUMN = 40
   logical, parameter :: DEBUG = .false.
!!$   logical, parameter :: DEBUG = .true.

contains

!<testsuite tests="3">
!    <testcase classname="foo" name="ASuccessfulTest"/>
!    <testcase classname="foo" name="AnotherSuccessfulTest"/>
!    <testcase classname="foo" name="AFailingTest">
!        <failure type="NotEnoughFoo"> details about failure </failure>
!    </testcase>
!</testsuite>

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

!     write(this%unit,'("F")', advance='no')

  end subroutine addFailure

  subroutine addError(this, testName, exceptions)
     use Exception_mod
     class (XmlPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (Exception), intent(in) :: exceptions(:)

!     write(this%unit,'("E")', advance='no')

  end subroutine addError

  subroutine startTest(this, testName)
     class (XmlPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName

!     write(this%unit,'(".")', advance='no')

     if (DEBUG) then
        write(this%unit,*)trim(testName)
        call flush(this%unit)
     end if

   end subroutine startTest

  subroutine endTest(this, testName)
     class (XmlPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     if (DEBUG) then
        write(this%unit,*)trim(testName)
        call flush(this%unit)
     end if

   end subroutine endTest

   subroutine print(this, result, runTime)
      use TestResult_mod
      class (XmlPrinter), intent(in) :: this
      type (TestResult), intent(in) :: result
      real, intent(in) :: runTime

      call this%printHeader(result)
      call this%printFailures('Error', result%getErrors())
      call this%printFailures('Failure', result%getFailures())
      call this%printFooter(result)

   end subroutine print

   subroutine printHeader(this, result)
      use TestResult_mod
      class (XmlPrinter), intent(in) :: this
      type(TestResult), intent(in) :: result
      

      write(this%unit,'(a,i4,a,i4,a,i4,a)') &
           '<testsuite errors="', result%errorCount(),&
           '" failures="', result%failureCount(),&
           '" tests="', result%runCount(),'">'

   end subroutine printHeader

   subroutine printFailures(this, label, failures)
      use TestResult_mod
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

         do j= 1, size(aFailedTest%exceptions)
            locationString = toString(aFailedTest%exceptions(j)%location)
            
            write(this%unit,*) label,' in: ', trim(aFailedTest%testName)
            write(this%unit,*) '  Location: ', trim(locationString)
            write(this%unit,'(a,1x,a)') aFailedTest%exceptions(j)%getMessage()
            write(this%unit,*)' '
         end do
      end do

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
      
   end subroutine printFailures

   subroutine printFooter(this, result)
      use TestResult_mod
      class (XmlPrinter), intent(in) :: this
      type (TestResult), intent(in) :: result

      if (result%wasSuccessful()) then
         write(this%unit,*)"OK"
         write(this%unit,'(a,i0,a)',advance='no')" (", result%runCount(), " test"
         if (result%runCount() > 1) then
            write(this%unit,'(a)')"s)"
         else
            write(this%unit,'(a)')")"
         end if
      else
         write(this%unit,*)"FAILURES!!!"
         write(this%unit,'(a,i0,a,i0,a,i0)')"Tests run: ", result%runCount(), &
              & ", Failures: ",result%failureCount(), &
              & ", Errors: ",result%errorCount()

      end if

   end subroutine printFooter

   integer function newunit(unit)
     ! This is a simple function to search for an available unit.
     ! LUN_MIN and LUN_MAX define the range of possible LUNs to check.
     ! The UNIT value is returned by the function, and also by the optional
     ! argument. This allows the function to be used directly in an OPEN
     ! statement, and optionally save the result in a local variable.
     ! If no units are available, -1 is returned.
     ! Copied from http://fortranwiki.org/fortran/show/newunit
     integer, intent(out), optional :: unit
     ! local
     integer, parameter :: LUN_MIN=10, LUN_MAX=1000
     logical :: opened
     integer :: lun
     ! begin
     newunit=-1
     do lun=LUN_MIN,LUN_MAX
       inquire(unit=lun,opened=opened)
       if (.not. opened) then
         newunit=lun
         exit
       end if
     end do
     if (present(unit)) unit=newunit
   end function newunit

end module XmlPrinter_mod
