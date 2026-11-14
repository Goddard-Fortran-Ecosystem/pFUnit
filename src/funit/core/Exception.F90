!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: PF_PrivateException
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
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
module PF_PrivateException
   use PF_SourceLocation
   implicit none
   private

   public :: Exception

   public :: NULL_MESSAGE
   public :: UNKNOWN_LINE_NUMBER
   public :: UNKNOWN_FILE_NAME

   character(len=*), parameter :: NULL_MESSAGE = ''

   type Exception
      character(len=:), allocatable :: message
      type (SourceLocation) :: location = UNKNOWN_SOURCE_LOCATION
      ! Locations the exception propagated through, innermost first.
      ! Kept last so that positional structure constructors are unaffected.
      type (SourceLocation), allocatable :: traceback(:)
   contains
      procedure :: getMessage
      procedure :: getLineNumber
      procedure :: getFileName
      procedure :: isNull
      procedure :: addTraceback
      procedure :: getTraceback
      procedure :: serialize
      procedure, nopass :: deserialize
   end type Exception

   interface Exception
      module procedure new_Exception
   end interface Exception


contains

   type(Exception) function new_Exception(message, location)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      if (present(message)) then
         new_Exception%message = trim(message)
      else
         new_Exception%message = NULL_MESSAGE
      end if

      if (present(location)) then
         new_Exception%location = location
      else
         new_Exception%location = UNKNOWN_SOURCE_LOCATION
      end if

   end function new_Exception

   function getMessage(this) result(message)
      class (Exception), intent(in) :: this
      character(:), allocatable :: message
      message = trim(this%message)
   end function getMessage

   integer function getLineNumber(this)
      class (Exception), intent(in) :: this
      getLineNumber = this%location%lineNumber
   end function getLineNumber

   function getFileName(this) result(name)
      character(len=:), allocatable :: name
      class (Exception), intent(in) :: this
      name = trim(this%location%fileName)
   end function getFileName


   logical function isNull(this)
      class (Exception), intent(in) :: this
      isNull = .not. allocated(this%message)
   end function isNull

   subroutine addTraceback(this, location)
      class (Exception), intent(inout) :: this
      type (SourceLocation), intent(in) :: location

      if (allocated(this%traceback)) then
         this%traceback = [this%traceback, location]
      else
         this%traceback = [location]
      end if

   end subroutine addTraceback

   function getTraceback(this) result(traceback)
      class (Exception), intent(in) :: this
      type (SourceLocation), allocatable :: traceback(:)

      if (allocated(this%traceback)) then
         traceback = this%traceback
      else
         allocate(traceback(0))
      end if

   end function getTraceback

   ! Layout: message, location, number of traceback entries, then
   ! each traceback location.
   function serialize(this) result(buffer)
      class (Exception), intent(in) :: this

      integer, allocatable :: buffer(:)
      type (SourceLocation), allocatable :: traceback(:)
      integer :: i

      buffer = serialize_string(this%message)
      buffer = [buffer, this%location%serialize()]

      traceback = this%getTraceback()
      buffer = [buffer, size(traceback)]
      do i = 1, size(traceback)
         buffer = [buffer, traceback(i)%serialize()]
      end do

   contains

      function serialize_string(string) result(buffer)
         integer, allocatable :: buffer(:)
         character(*), intent(in) :: string

         buffer = [0, len(string), transfer(string,[1])]
         buffer(1) = size(buffer)
      end function serialize_string
      
   end function serialize

   function deserialize(buffer) result(e)
      type (Exception) :: e
      integer, intent(in) :: buffer(:)
      integer :: n, i, num_traceback
      type (SourceLocation) :: sloc

      call deserialize_string(buffer, e%message)

      n = buffer(1)
      e%location = sloc%deserialize(buffer(n+1:))
      n = n + location_size(buffer(n+1:))

      num_traceback = buffer(n+1)
      n = n + 1
      allocate(e%traceback(num_traceback))
      do i = 1, num_traceback
         e%traceback(i) = sloc%deserialize(buffer(n+1:))
         n = n + location_size(buffer(n+1:))
      end do

   contains

      ! A serialized SourceLocation is a string block followed by the line number.
      integer function location_size(buffer)
         integer, intent(in) :: buffer(:)
         location_size = buffer(1) + 1
      end function location_size

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

end module PF_PrivateException

module PF_Exception
   use PF_SourceLocation
   use PF_PrivateException
   implicit none
   private

   public :: Exception

   public :: NULL_MESSAGE
   public :: UNKNOWN_LINE_NUMBER
   public :: UNKNOWN_FILE_NAME

end module PF_Exception
