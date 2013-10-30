! This module just provides a data type - not a class.
! Meant to be shared for easy access.

module SourceLocation_mod
   implicit none
   private

   public :: SourceLocation
   public :: UNKNOWN_SOURCE_LOCATION
   public :: UNKNOWN_FILE_NAME
   public :: UNKNOWN_LINE_NUMBER

   integer, parameter :: MAXLEN_FILE_NAME = 100
   character(len=MAXLEN_FILE_NAME), parameter :: UNKNOWN_FILE_NAME= '<unknown file>'
   integer, parameter :: UNKNOWN_LINE_NUMBER = -1

   type SourceLocation
      character(len=MAXLEN_FILE_NAME) :: fileName = UNKNOWN_FILE_NAME
      integer :: lineNumber = UNKNOWN_LINE_NUMBER
   end type SourceLocation

   type (SourceLocation), parameter :: UNKNOWN_SOURCE_LOCATION = &
        & SourceLocation()

end module SourceLocation_mod
