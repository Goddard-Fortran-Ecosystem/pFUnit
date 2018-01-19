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
module PF_ResultPrinter_mod
   use PF_Exception_mod
   use PF_TestListener_mod, only : TestListener
   implicit none
   private

   public :: ResultPrinter
   public :: newResultPrinter

   type, extends(TestListener) :: ResultPrinter
      integer :: unit
      integer :: column
   contains
      procedure :: addFailure
      procedure :: addError
      procedure :: startTest
      procedure :: endTest
      procedure :: endRun
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

contains

  function newResultPrinter(unit)
     type (ResultPrinter) :: newResultPrinter
     integer, intent(in) :: unit

     newResultPrinter%unit = unit
     newResultPrinter%column = 0

  end function newResultPrinter

  subroutine addFailure(this, testName, exceptions)
     use PF_ExceptionList_mod
     class (ResultPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (ExceptionList), intent(in) :: exceptions

     write(this%unit,'("F")', advance='no')
     call this%incrementColumn()

  end subroutine addFailure

  subroutine addError(this, testName, exceptions)
     use PF_ExceptionList_mod
     class (ResultPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (ExceptionList), intent(in) :: exceptions

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

  subroutine endTest(this, testName)
     class (ResultPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     if (DEBUG) then
        write(this%unit,*)trim(testName)
        call flush(this%unit)
     end if

   end subroutine endTest

   subroutine endRun(this, result)
      use PF_AbstractTestResult_mod, only : AbstractTestResult
      class (ResultPrinter), intent(inout) :: this
      class (AbstractTestResult), intent(in) :: result

      call this%print(result)

    end subroutine endRun

   subroutine print(this, result)
      use PF_AbstractTestResult_mod, only : AbstractTestResult
      use PF_TestFailureVector_mod
      class (ResultPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

      call this%printHeader(result%getRunTime())
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
      use PF_TestFailure_mod
      use PF_TestFailureVector_mod
      use PF_SourceLocation_mod
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
      use PF_AbstractTestResult_mod
      class (ResultPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result

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
   end subroutine addSuccess
     
end module PF_ResultPrinter_mod
