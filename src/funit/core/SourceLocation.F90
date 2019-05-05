!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: SourceLocation
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
! This module just provides a data type - not a class.
! Meant to be shared for easy access.

module PF_SourceLocation
   implicit none
   private

   public :: SourceLocation
   public :: UNKNOWN_SOURCE_LOCATION
   public :: UNKNOWN_FILE_NAME
   public :: UNKNOWN_LINE_NUMBER

   integer, parameter :: MAXLEN_FILE_NAME = 255
   character(len=MAXLEN_FILE_NAME), parameter :: UNKNOWN_FILE_NAME= '<unknown file>'
   integer, parameter :: UNKNOWN_LINE_NUMBER = -1

   type :: SourceLocation
      character(len=MAXLEN_FILE_NAME) :: fileName = UNKNOWN_FILE_NAME
      integer :: lineNumber = UNKNOWN_LINE_NUMBER
   contains
      procedure :: serialize
      procedure, nopass :: deserialize
      procedure :: toString
   end type SourceLocation

   type (SourceLocation), parameter :: UNKNOWN_SOURCE_LOCATION = &
        & SourceLocation()

contains

   function toString(this) result(string)
      class (SourceLocation), intent(inout) :: this
      character(len=300) :: string
      integer :: status

      if (this%fileName == UNKNOWN_FILE_NAME) then
         if (this%lineNumber == UNKNOWN_LINE_NUMBER) then
            string = '<unknown location>'
         else
            write(string,'(a,":",i0)', iostat=status) trim(UNKNOWN_FILE_NAME), this%lineNumber
         end if
      else
         if (this%lineNumber == UNKNOWN_LINE_NUMBER) then
            string = trim(this%fileName)
         else
            write(string,'(a,":",i0)', iostat=status) trim(this%fileName), this%lineNumber
         end if
      end if

      string = '[' // trim(string) // ']'

   end function toString


   function serialize(this) result(buffer)
      integer, allocatable :: buffer(:)
      class (SourceLocation), intent(in) :: this

      buffer = serialize_string(trim(this%filename))
      buffer = [ buffer, this%lineNumber]

   contains

      function serialize_string(string) result(buffer)
         integer, allocatable :: buffer(:)
         character(*), intent(in) :: string

         buffer = [0, len(string), transfer(string,[1])]
         buffer(1) = size(buffer)
      end function serialize_string

   end function serialize

   function deserialize(buffer) result(loc)
      type (SourceLocation) :: loc
      integer, intent(in) :: buffer(:)

      integer :: n
      character(:), allocatable :: str

      call deserialize_string(buffer, str)
      loc%fileName = str

      n = buffer(1)
      loc%lineNumber = buffer(n+1)

   contains

      subroutine deserialize_string(buffer, str)
         integer, intent(in) :: buffer(:)
         character(len=:), allocatable :: str
         
         integer :: buf_size, str_len

         buf_size = buffer(1)
         str_len = buffer(2)
         allocate(character(str_len) :: str)
         str = transfer(buffer(3:buf_size), str)

      end subroutine deserialize_string

   end function deserialize

end module PF_SourceLocation
