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
module PF_PrivateException_mod
   use PF_SourceLocation_mod
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
   contains
      procedure :: getMessage
      procedure :: getLineNumber
      procedure :: getFileName
      procedure :: isNull
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

   function serialize(this) result(buffer)
      class (Exception), intent(in) :: this

      integer, allocatable :: buffer(:)

      buffer = [len(this%message)]
      buffer = [buffer, transfer(this%message,[1])]
      buffer = [buffer, this%location%serialize()]
   end function serialize

   function deserialize(buffer) result(e)
      type (Exception) :: e
      integer, intent(in) :: buffer(:)
      integer :: n

      type (SourceLocation) :: sloc

      n = buffer(1)
      e%message = transfer(buffer(2:n+1),'c')
      e%location = sloc%deserialize(buffer(n+2:))
      
   end function deserialize

end module PF_PrivateException_mod

module PF_Exception_mod
   use PF_SourceLocation_mod
   use PF_PrivateException_mod
   implicit none
   private

   public :: Exception

   public :: NULL_MESSAGE
   public :: UNKNOWN_LINE_NUMBER
   public :: UNKNOWN_FILE_NAME

end module PF_Exception_mod
