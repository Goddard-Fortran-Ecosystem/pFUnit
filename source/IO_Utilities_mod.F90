!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: IO_Utilities
!
!> @brief
!! Handles input and output utilities including file and pipe.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 12 Mar 2007
!!
! REVISION HISTORY:
! 12 Mar 2007 - Initial Version
! 12 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module IO_Utilities_mod
   use pFUnitException_mod
   implicit none

   private

   public :: getUnit
   public :: openFile
   public :: createNamedPipe
   public :: deleteFile
   
   public :: isOpen
   public :: isConnected

   interface getUnit
      module procedure getUnit_new
      module procedure getUnit_fromFileName
   end interface

   interface isOpen
      module procedure isOpen_unit
      module procedure isOpen_fileName
   end interface

   interface deleteFile
      module procedure deleteFile_basic
      module procedure deleteFile_ignore
   end interface

contains

   !---------------------------------------------------------------------------
   !> Gets a logical unit number for its I/O as default
   !! 
   !! @return getUnit_new new logical unit number
   !!
   !! @throw exception for not available unit number
   !!
   !! @note
   !! The Fortran standard apparently does not dictate the legitimate range
   !! for logical unit numbers used for I/O aside from the requirement that 
   !! they not be negative.  The following limits are generally safe for known
   !!  compilers.  If an application needs more than 90 files to be opened 
   !! simultaneously (shudder), then MAX_UNIT_NUMBER could be increased on a 
   !! per-compiler basis.
   !---------------------------------------------------------------------------
   integer function getUnit_new() 
      ! throws NO_AVAILABLE_UNITS
      integer :: unit
      logical :: isOpen

      integer, parameter :: MIN_UNIT_NUMBER = 10
      integer, parameter :: MAX_UNIT_NUMBER = 99

      do unit = MIN_UNIT_NUMBER, MAX_UNIT_NUMBER
         inquire(unit = unit, opened = isOpen)
         if (.not. isOpen) then
            getUnit_new = unit
            return
         end if
      end do

      getUnit_new = -1 ! failed
      call throw(NO_AVAILABLE_UNITS)

   end function getUnit_new

   !---------------------------------------------------------------------------
   !> Gets a logical unit number from the file for its I/O
   !!
   !! @param fileName - given file name
   !! 
   !! @return getUnit_fromFileName logical unit number of the file
   !!
   !! @throw exception when file is not associated.
   !---------------------------------------------------------------------------
   integer function getUnit_fromFileName(fileName)
      ! throws FILE_NOT_ASSOCIATED
      character(len=*), intent(in) :: fileName
      integer :: unit

      inquire(file = fileName, number = unit)
      if (unit == -1) then
         call throw(FILE_NOT_ASSOCIATED)
      end if

      getUnit_fromFileName = unit

   end function getUnit_fromFileName

   !---------------------------------------------------------------------------
   !> Gets a logical unit number from the file for its I/O
   !!
   !! @param fileName - given file name
   !! @param status - given status of the file
   !! @param form - given format file
   !! 
   !! @return unit logical unit number from the file
   !!
   !! @throw exception when it occurs due to a failure of handling the file. 
   !---------------------------------------------------------------------------
   integer function openFile(fileName, status, form) result(unit)
      ! throws FILE_IS_OPEN
      ! throws FILE_EXISTS
      ! throws FILE_DOES_NOT_EXIST
      ! throw NO_AVAILABLE_UNITS
      character(len=*), intent(in) :: fileName
      character(len=*), intent(in) :: status
      character(len=*), intent(in) :: form

      logical :: exists
      logical :: isOpen

      inquire(file=trim(fileName), exist = exists, opened = isOpen)

      if (isOpen) then
         call throw(FILE_IS_OPEN)
         unit = -1
         return
      end if

      if (exists .and. any(trim(status) == (/ 'new', 'NEW', 'New' /))) then
         call throw(FILE_EXISTS)
         unit = -1
         return
      end if

      if (.not. exists .and. any(trim(status) == (/ 'old', 'OLD', 'Old' /))) then
         call throw(FILE_DOES_NOT_EXIST)
         unit = -1
         return
      end if

      unit = getUnit()
      if (catch(NO_AVAILABLE_UNITS)) then
         call throw(NO_AVAILABLE_UNITS)
         return
      end if

      ! Cleared all exceptional cases - can now open the file
      open(unit,file=fileName, status=status, form=form)

   end function openFile

   !---------------------------------------------------------------------------
   !> Determines the connection between file and its unit number
   !!
   !! @param unit - given unit number of file
   !! @param fileName - given file name
   !!
   !! @return isConnected .true. if it is connected.   Otherwise, it is .false.
   !---------------------------------------------------------------------------
   logical function isConnected(unit, fileName)
      integer, intent(in) :: unit
      character(len=*), intent(in) :: fileName

      logical :: isOpen
      integer :: foundUnit

      inquire(file=fileName, number=foundUnit, opened = isOpen)
      isConnected = (isOpen .and. (foundUnit == unit))

   end function isConnected

   !---------------------------------------------------------------------------
   !> Determines if the file with its unit number is opened.
   !!
   !! @param unit - given unit number of file
   !!
   !! @return isOpen_unit .true. if the file is opened.   Otherwise, it is 
   !! .false.
   !---------------------------------------------------------------------------
   logical function isOpen_unit(unit)
      integer, intent(in) :: unit

      integer :: foundUnit

      inquire(unit=unit, opened = isOpen_unit)

   end function isOpen_unit

   !---------------------------------------------------------------------------
   !> Determines if the file with the given filename is opened.
   !!
   !! @param fileName - given file name
   !!
   !! @return isOpen_fileName .true. if the file is opened.   
   !! Otherwise, it is .false.
   !---------------------------------------------------------------------------
   logical function isOpen_fileName(fileName)
      character(len=*), intent(in) :: fileName

      integer :: foundUnit

      inquire(file = fileName, opened = isOpen_fileName)

   end function isOpen_fileName

   !---------------------------------------------------------------------------
   !> Deletes the file based on the given file name
   !!
   !! @param fileName - given file name
   !!
   !! @throw exception when it fails to delete the file due to the 
   !! non-existing file or unavailable unit number
   !---------------------------------------------------------------------------
   subroutine deleteFile_basic(fileName)
      ! throws FILE_IS_OPEN
      ! throws NO_AVAILABLE_UNITS
      character(len=*), intent(in) :: fileName

      call deleteFile_ignore(fileName, ignore=.false.)
   end subroutine deleteFile_basic

   !---------------------------------------------------------------------------
   !> Deletes the file based on the given file name with ignore option
   !!
   !! @param fileName - given file name
   !! @param ignore - given ignore 
   !!
   !! @throw exception when it fails to delete the file due to some reasons 
   !! including 1) opening file, 2) non-existing file, or 
   !! 3) unavailable unit number
   !---------------------------------------------------------------------------
   subroutine deleteFile_ignore(fileName, ignore)
      ! throws FILE_IS_OPEN
      ! throws NO_AVAILABLE_UNITS
      character(len=*), intent(in) :: fileName
      logical, intent(in) :: ignore
      integer :: unit
      logical :: isOpen
      logical :: exists

      inquire(file=fileName, opened=isOpen, exist = exists)

      if (.not. exists) then 
         if (.not. ignore) call throw(FILE_DOES_NOT_EXIST)
         return
      end if

      if (isOpen) then
         call throw(FILE_IS_OPEN)
         return
      end if

      unit = getUnit()
      if (catch(NO_AVAILABLE_UNITS)) then
         call throw(NO_AVAILABLE_UNITS)
         return
      end if

      open(unit, file=fileName, status='old')
      close(unit,status='delete')

   end subroutine deleteFile_ignore

   !---------------------------------------------------------------------------
   !> Creates new pipe name. 
   !!
   !! @param pipeName - given pipe name 
   !---------------------------------------------------------------------------
   subroutine createNamedPipe(pipeName)
#ifdef NAG
      use f90_unix_proc
      use f90_unix
#endif
      character(len=*), intent(in) :: pipeName
      call system('mkfifo '//pipeName)
   end subroutine createNamedPipe


end module IO_Utilities_mod
