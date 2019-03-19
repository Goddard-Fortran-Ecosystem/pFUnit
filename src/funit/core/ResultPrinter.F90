#include "unused_dummy.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: ResultPrinter
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
module PF_ResultPrinter
   use PF_Exception
   use PF_TestListener, only : TestListener
   implicit none
   private

   public :: ResultPrinter

   type, extends(TestListener) :: ResultPrinter
      integer :: unit
      integer :: column
   contains
      procedure :: addFailure
      procedure :: addError
      procedure :: startTest
      procedure :: disableTest
      procedure :: endTest
      procedure :: print
      procedure :: printHeader
      procedure :: printFailures
      procedure :: printFooter
      procedure :: incrementColumn
      procedure :: addSuccess
   end type ResultPrinter

   integer, parameter :: MAX_COLUMN = 80
   logical, parameter :: DEBUG = .false.
!!$   logical, parameter :: DEBUG = .true.

   interface ResultPrinter
      module procedure new_ResultPrinter
   end interface ResultPrinter

contains

  function new_ResultPrinter(unit)
     type (ResultPrinter) :: new_ResultPrinter
     integer, intent(in) :: unit

     new_ResultPrinter%unit = unit
     new_ResultPrinter%column = 0

  end function new_ResultPrinter

  subroutine addFailure(this, testName, exceptions)
     use PF_ExceptionList
     class (ResultPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (ExceptionList), intent(in) :: exceptions

     _UNUSED_DUMMY(testName)
     _UNUSED_DUMMY(exceptions)
     write(this%unit,'("F")', advance='no')
     call this%incrementColumn()

  end subroutine addFailure

  subroutine addError(this, testName, exceptions)
     use PF_ExceptionList
     class (ResultPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (ExceptionList), intent(in) :: exceptions

     _UNUSED_DUMMY(testName)
     _UNUSED_DUMMY(exceptions)
     write(this%unit,'("E")', advance='no')
     call this%incrementColumn()

  end subroutine addError

  subroutine startTest(this, testName)
     class (ResultPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(this%unit,'(".")', advance='no')
     call this%incrementColumn()

     if (DEBUG) then
        write(this%unit,*)trim(testName)
        call flush(this%unit)
     end if

   end subroutine startTest

  subroutine disableTest(this, testName)
     class (ResultPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(this%unit,'("I")', advance='no')
     call this%incrementColumn()

     if (DEBUG) then
        write(this%unit,*)trim(testName)
        call flush(this%unit)
     end if

  end subroutine disableTest

  subroutine endTest(this, testName)
     class (ResultPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     if (DEBUG) then
        write(this%unit,*)trim(testName)
        call flush(this%unit)
     end if

   end subroutine endTest

   subroutine print(this, result, elapsed_time)
      use PF_AbstractTestResult, only : AbstractTestResult
      use PF_TestFailureVector
      class (ResultPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result
      real, intent(in) :: elapsed_time

      call this%printHeader(elapsed_time)
      call this%printFailures('Error', result%getErrors())
      call this%printFailures('Failure', result%getFailures())
      call this%printFooter(result)

   end subroutine print

   subroutine printHeader(this, runTime)
      class (ResultPrinter), intent(in) :: this
      real, intent(in) :: runTime

      write(this%unit,*)
      write(this%unit,'(a,1x,f12.3,1x,a)') 'Time: ', runTime, 'seconds'
      write(this%unit,*)" "

   end subroutine printHeader

   subroutine printFailures(this, label, failures)
      use PF_TestFailure
      use PF_TestFailureVector
      use PF_SourceLocation
      class (ResultPrinter), intent(in) :: this
      character(len=*), intent(in) :: label
      type (TestFailureVector), intent(in) :: failures

      type (TestFailure), pointer :: aFailedTest
      integer :: i, j
      character(len=:), allocatable :: locationString

      class (Exception), pointer :: e

      do i = 1, failures%size()
         aFailedTest => failures%at(i)

         do j= 1, aFailedTest%exceptions%size()
            e => aFailedTest%exceptions%at(j)
            locationString = e%location%toString()

            write(this%unit,'(a)') label,' in: ', trim(aFailedTest%testName)
            write(this%unit,'(a)') '  Location: ', trim(locationString)
            write(this%unit,'(a,1x,a)') e%getMessage()
            write(this%unit,*)' '
         end do
      end do

   end subroutine printFailures

   subroutine printFooter(this, result)
      use PF_AbstractTestResult
      class (ResultPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      if (result%wasSuccessful()) then
         write(this%unit,*)"OK"
         write(this%unit,'(a,i0,a)',advance='no')" (", result%runCount(), " test"
         if (result%runCount() > 1) then
            write(this%unit,'(a)',advance='no')"s"
         end if
         if (result%disableCount() > 0) then
            write(this%unit,'(a,i0,a)',advance='no')", ", result%disableCount(), " disabled"
         end if
         write(this%unit,'(a)')")"
      else
         write(this%unit,*)"FAILURES!!!"
         write(this%unit,'(a,i0,a,i0,a,i0)')"Tests run: ", result%runCount(), &
              & ", Failures: ",result%failureCount(), &
              & ", Errors: ",result%errorCount(), &
              & ", Disabled: ",result%disableCount()

      end if

   end subroutine printFooter

     subroutine incrementColumn(this)
        class (ResultPrinter), intent(inout) :: this

        this%column = this%column + 1

        if (this%column >= MAX_COLUMN) then
           write(this%unit,*) ! newline
           this%column = 0
        end if

     end subroutine incrementColumn

   subroutine addSuccess(this, testName)
      class (ResultPrinter), intent(inout) :: this
      character(*), intent(in) :: testName

     _UNUSED_DUMMY(this)
     _UNUSED_DUMMY(testName)

   end subroutine addSuccess
     
end module PF_ResultPrinter
