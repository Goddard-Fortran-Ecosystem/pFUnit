module AssertionLocation_mod
   use Exception_mod
   implicit none
   private

   public :: AssertionLocation
   public :: LineAssertionLocation
   public :: LineAndFileAssertionLocation
   public :: newAssertionLocation

   integer, parameter :: MAXLEN_FILE_NAME = 40

   type AssertionLocation
      private
   contains
      procedure :: print => printNoLocation
   end type AssertionLocation

   type, extends(AssertionLocation) :: LineAssertionLocation
      private
      integer :: lineNumber
   contains
      procedure :: print => printLineNumber
   end type LineAssertionLocation

   type, extends(LineAssertionLocation) :: LineAndFileAssertionLocation
      private
      character(len=MAXLEN_FILE_NAME) :: fileName
   contains
      procedure :: print => printLineNumberAndFile
   end type LineAndFileAssertionLocation

   interface newAssertionLocation
      module procedure newLineAssertionLocation
      module procedure newLineAndFileAssertionLocation
   end interface

contains

   function newLineAssertionLocation(lineNumber)
      type (LineAssertionLocation), pointer :: newLineAssertionLocation
      integer, intent(in) :: lineNumber
      allocate(newLineAssertionLocation)
      newLineAssertionLocation%lineNumber = lineNumber
   end function newLineAssertionLocation

   function newLineAndFileAssertionLocation(lineNumber, fileName)
      type (LineAndFileAssertionLocation), pointer :: newLineAndFileAssertionLocation
      integer, intent(in) :: lineNumber
      character(len=*), intent(in) :: fileName
      allocate(newLineAndFileAssertionLocation)
      newLineAndFileAssertionLocation%lineNumber = lineNumber
      newLineAndFileAssertionLocation%fileName = fileName

   end function NewLineAndFileAssertionLocation

   function printNoLocation(this) result(message)
      class (AssertionLocation), intent(in) :: this
      character(len=MAXLEN_MESSAGE) :: message
      message = ' '
   end function printNoLocation

   function printLineNumber(this) result(message)
      class (LineAssertionLocation), intent(in) :: this
      character(len=MAXLEN_MESSAGE) :: message
      message = 'at line <5>'
   end function printLineNumber

   function printLineNumberAndFile(this) result(message)
      class (LineAndFileAssertionLocation), intent(in) :: this
      character(len=MAXLEN_MESSAGE) :: message
      write(message,'("in ",a," at line <",i0,">")') trim(this%fileName), this%lineNumber
   end function printLineNumberAndFile

end module AssertionLocation_mod
