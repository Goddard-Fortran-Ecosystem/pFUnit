! This module just provides a data type - not a class.
! Meant to be shared for easy access.

module SourceLocation_mod
   implicit none
   private

   public :: SourceLocation
   public :: newSourceLocation
   public :: UNKNOWN_SOURCE_LOCATION
   public :: UNKNOWN_FILE_NAME
   public :: UNKNOWN_LINE_NUMBER

   integer, parameter :: MAXLEN_FILE_NAME = 40
   character(len=MAXLEN_FILE_NAME), parameter :: UNKNOWN_FILE_NAME= '<unknown file>'
   integer, parameter :: UNKNOWN_LINE_NUMBER = -1

   type SourceLocation
      character(len=MAXLEN_FILE_NAME) :: fileName = UNKNOWN_FILE_NAME
      integer :: lineNumber = UNKNOWN_LINE_NUMBER
   end type SourceLocation

   type (SourceLocation), parameter :: UNKNOWN_SOURCE_LOCATION = &
        & SourceLocation(UNKNOWN_FILE_NAME, UNKNOWN_LINE_NUMBER)

   interface newSourceLocation
      module procedure SourceLocation_fileAndLine
      module procedure SourceLocation_fileName
      module procedure SourceLocation_lineNumber
   end interface newSourceLocation

contains

   function SourceLocation_fileAndLine(fileName, lineNumber) result(location)
      type (SourceLocation) :: location
      character(len=*), intent(in) :: fileName
      integer, intent(in) :: lineNumber

      location%fileName = fileName
      location%lineNumber = lineNumber

   end function SourceLocation_fileAndLine

   function SourceLocation_fileName(fileName) result(location)
      type (SourceLocation) :: location
      character(len=*), intent(in) :: fileName

      location%fileName = fileName
      location%lineNumber = UNKNOWN_LINE_NUMBER

   end function SourceLocation_fileName

   function SourceLocation_lineNumber(lineNumber) result(location)
      type (SourceLocation) :: location
      integer, intent(in) :: lineNumber

      location%fileName = UNKNOWN_FILE_NAME
      location%lineNumber = lineNumber

   end function SourceLocation_lineNumber

end module SourceLocation_mod
