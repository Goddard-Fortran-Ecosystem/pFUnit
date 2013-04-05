module ResultPrinter_mod
   use Exception_mod
   use TestListener_mod
   implicit none
   private

   public :: ResultPrinter
   public :: newResultPrinter

   type, extends(TestListener) :: ResultPrinter
      integer :: unit
      integer :: column
      type (ExceptionList) :: exceptions
   contains
      procedure :: addFailure
      procedure :: startTest
      procedure :: endTest
      procedure :: print
      procedure :: printHeader
      procedure :: printFailures
      procedure :: printFooter
   end type ResultPrinter

   integer, parameter :: MAX_COLUMN = 40
   logical, parameter :: DEBUG = .false.
!!$   logical, parameter :: DEBUG = .true.

contains

  function newResultPrinter(unit)
     type (ResultPrinter) :: newResultPrinter
     integer, intent(in) :: unit

     newResultPrinter%unit = unit
     newResultPrinter%column = 0
     newResultPrinter%exceptions = newExceptionList()

  end function newResultPrinter

  subroutine addFailure(this, testName, anException)
     use Exception_mod
     class (ResultPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (Exception), intent(in) :: anException

     write(this%unit,'("F")', advance='no')
     this%column = this%column + 1
     if (this%column >= MAX_COLUMN) then
        write(this%unit,*) ! newline
        this%column = 0
     end if

     call this%exceptions%throw(anException)

  end subroutine addFailure

  subroutine startTest(this, testName)
     class (ResultPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     write(this%unit,'(".")', advance='no')

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

   subroutine print(this, result, runTime)
      use TestResult_mod
      class (ResultPrinter), intent(in) :: this
      type (TestResult), intent(in) :: result
      real, intent(in) :: runTime

      call this%printHeader(runTime)
      call this%printFailures(result)
      call this%printFooter(result)

   end subroutine print

   subroutine printHeader(this, runTime)
      class (ResultPrinter), intent(in) :: this
      real, intent(in) :: runTime

      write(this%unit,*)
      write(this%unit,'(a,1x,f12.3,1x,a)') 'Time: ', runTime, 'seconds'

   end subroutine printHeader

   subroutine printFailures(this, result)
      use TestResult_mod
      use TestFailure_mod
      use SourceLocation_mod
      class (ResultPrinter), intent(in) :: this
      type (TestResult), intent(in) :: result

      type (TestFailure) :: aFailedTest
      integer :: i
      character(len=80) :: locationString

      do i = 1, size(result%failures)
         aFailedTest = result%failures(i)
         write(this%unit,*) 'Failure in: ', trim(aFailedTest%testName)

         locationString = toString(aFailedTest%exception%location)
         write(this%unit,'(a,1x,a)') aFailedTest%exception%getMessage(), trim(locationString)
      end do

   contains

      function toString(location) result(string)
         type (SourceLocation), intent(in) :: location
         character(len=80) :: string

         if (location%fileName == UNKNOWN_FILE_NAME) then
            if (location%lineNumber == UNKNOWN_LINE_NUMBER) then
               string = '<unknown location>'
            else
               write(string,'(a,"::",i0)') trim(UNKNOWN_FILE_NAME), location%lineNumber
            end if
         else
            if (location%lineNumber == UNKNOWN_LINE_NUMBER) then
               string = trim(location%fileName)
            else
               write(string,'(a,"::",i0)') trim(location%fileName), location%lineNumber
            end if
         end if

         string = '[' // trim(string) // ']'

      end function toString
      
   end subroutine printFailures

   subroutine printFooter(this, result)
      use TestResult_mod
      class (ResultPrinter), intent(in) :: this
      type (TestResult), intent(in) :: result

      write(this%unit,*)" "
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
         write(this%unit,*)"Tests run: ", result%runCount(), &
              & ", Failures: ",result%failureCount()
      end if

   end subroutine printFooter

end module ResultPrinter_mod
