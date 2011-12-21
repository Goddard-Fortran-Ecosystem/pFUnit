!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: Report_mod
!
!> @brief
!! Implements the report object for handling the test report. 
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 16 Oct 2006
!!
! REVISION HISTORY:
! 16 Oct 2006 - Initial Version
! 12 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module Report_mod
   use Params_mod, only: MAX_LEN_MSG
   implicit none
   private

   public :: Report_type
   public :: report
   public :: append
   public :: print
   public :: numLines
   public :: getLine
   public :: AssertEqual
   public :: generateExceptionReport
   public :: copy
   public :: clean

   type Report_type
      private
      character(len=MAX_LEN_MSG), Pointer :: messages(:) => null()
   end type Report_type

   interface report
      module procedure report_empty
      module procedure report_str
      module procedure report_strs
   end interface

   interface clean
      module procedure clean_report
   end interface

   interface getLine
      module procedure getLine_report
   end interface

   interface copy
      module procedure deepCopy
   end interface
   integer :: ier

   Interface AssertEqual
      Module Procedure AssertEqual_reports
   End Interface

contains

   !---------------------------------------------------------------------------
   !> Creates the default constructor with empty for the report.
   !!
   !! @return the empty object of report 
   !---------------------------------------------------------------------------
   function Report_empty() result(rprt)
     type (Report_type) :: rprt

     allocate(rprt%messages(0), STAT=ier)
     if (ier /= 0) print*,__LINE__,__FILE__,ier

   end function Report_empty

   !---------------------------------------------------------------------------
   !> Creates the constructor with the given string for the report.
   !!
   !! @param str - given string for appending it to the report object
   !!
   !! @return report object with string
   !---------------------------------------------------------------------------
   function Report_str(str) result(rprt)
     type (Report_type) :: rprt
     character(len=*), intent(in) :: str
    
     rprt = Report()
     call append(rprt, str)

   end function Report_str

   !---------------------------------------------------------------------------
   !> Creates the constructor with the given string array for the report.
   !!
   !! @param str - given string array for appending it to the report object
   !!
   !! @return report object with string array
   !---------------------------------------------------------------------------
   function Report_strs(strs) result(rprt)
     type (Report_type) :: rprt
     character(len=*), intent(in) :: strs(:)
     integer :: i

     rprt = Report()

     do i= 1 , size(strs)
        call append(rprt, trim(strs(i)))
     end do

   end function Report_strs

   !---------------------------------------------------------------------------
   !> Appends the given message to the report object
   !!
   !! @param this - this report object
   !! @param str - given message
   !!
   !! @return updated report object after the message is appended
   !---------------------------------------------------------------------------
   subroutine Append(this, message)
      type (Report_type)              :: this
      character(len=*), intent(in) :: message

      integer :: n_messages
      character(len=MAX_LEN_MSG), pointer :: messages_new(:) => null()

      n_messages = numLines(this)
      allocate(messages_new(n_messages + 1), STAT=ier)
      if (ier /= 0) print*,__LINE__,__FILE__,ier

      messages_new(1:n_messages) = this%messages
      messages_new(n_messages + 1) = trim(message)

      deallocate(this%messages, stat=ier)
      if (ier /= 0) print*,__LINE__,__FILE__,ier

      this%messages => messages_new

   end subroutine Append

   !---------------------------------------------------------------------------
   !> Prints the informaton of the existing report object.
   !!
   !! @param this - this report object
   !---------------------------------------------------------------------------
   subroutine print(this)
      type (Report_type) :: this

      integer :: n

      do n = 1, size(this%messages)
         print*,'Report:',n
         print*,trim(this%messages(n))
      end do

   end subroutine print

   !---------------------------------------------------------------------------
   !> Returns the size of the report's messages.
   !!
   !! @param this - this report object
   !! 
   !! @return number of lines for the this report object's messages
   !---------------------------------------------------------------------------
   integer function numLines(this)
      type (Report_type) :: this

      if (associated(this%messages)) then
         numLines = size(this%messages)
      else
         numLines = 0
      end if

   end function numLines

   !---------------------------------------------------------------------------
   !> Returns the line of the report in string
   !!
   !! @param this - this report object
   !! @param line - given line number 
   !! 
   !! @return buffer in string 
   !---------------------------------------------------------------------------
   function getLine_report(this, line) result(buffer)
      type (Report_type), intent(in) :: this
      integer,         intent(in) :: line
      character(len=MAX_LEN_MSG)  :: buffer

      buffer = trim(this%messages(line))

   end function getLine_report

   !---------------------------------------------------------------------------
   !> Clears up the report object
   !!
   !! @param this - this report object
   !---------------------------------------------------------------------------
   subroutine clean_report(this)
      type (Report_type) :: this

      deallocate(this%messages, STAT=ier)
      if (ier /= 0) print*,__LINE__,__FILE__,ier

   end subroutine clean_report

   !---------------------------------------------------------------------------
   !> Makes a copy of allocated report object into another report object
   !!
   !! @param a - this report object with allocation
   !! @param b - new report object being copied from allocated report object
   !---------------------------------------------------------------------------
   subroutine deepCopy(a, b)
      type (Report_type), intent(in) :: a
      type (Report_type), intent(out) :: b

      integer :: i

      allocate(b%messages(numLines(a)))
      do i= 1, numLines(a)
         b % messages(i) = a % messages(i)
      end do

   end subroutine deepCopy

   !---------------------------------------------------------------------------
   !> Generates the exception for the report 
   !!
   !! @return report object generating its exception
   !---------------------------------------------------------------------------
   function generateExceptionReport() result(rprt)
      use pFUnitException_mod
      type (Report_type) :: rprt
      type (Exception_type) :: nextException
      integer :: n

      rprt = Report()
      do
         nextException = catchAny()
         if (nextException == NO_EXCEPTION) exit
         call append(rprt, getMessage(nextException))
      end do

   end function generateExceptionReport



   !---------------------------------------------------------------------------
   !> Asserts that two reports or objects are equal.
   !!
   !! @param expected - report with expected value
   !! @param actual -  report with actual value
   !!
   !! @return .true. only if the two BaseAddresses are the same.
   !---------------------------------------------------------------------------
   subroutine AssertEqual_reports(expected, actual)
      use AssertString_mod
      use AssertInteger_mod
      type (Report_type), intent(in) :: expected
      type (Report_type), intent(in) :: actual

      integer :: i

      Call AssertEqual(numLines(expected), numLines(actual),'reports differ')

      Do i = 1, Min(numLines(expected), numLines(actual))
         Call AssertEqual(trim(getLine(expected,i)), trim(getLine(actual,i)), ignoreWhiteSpace=.true.)
      End Do

   end subroutine AssertEqual_reports

end module Report_mod
