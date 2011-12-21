!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: InternalFile_mod
!
!> @brief
!! Handles the internal file such as writing and reading.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 16 Oct 2006
!!
!! @todo 
!! It needs improvement since it is very inefficient implemenation.
!!
!
! REVISION HISTORY:
! 16 Oct 2006 - Initial Version
! 12 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module InternalFile_mod
   implicit none
   private

   public :: InternalFile_type
   public :: InternalFile ! constructor
   public :: Clean        ! destructor
   public :: GetLine
   public :: NumLines
   public :: AppendLine
   public :: LoadFile
   public :: WriteFile

   integer, parameter :: MAX_LINE_LENGTH=132
 
   type InternalFile_type
      private
      character(len=MAX_LINE_LENGTH), pointer :: lines(:) => null()
   end type InternalFile_type

   interface InternalFile
      module procedure new_empty
      module procedure new_strings
   end interface

   interface Clean
      module procedure clean_
   end interface

contains

   !---------------------------------------------------------------------------
   !> Creates empty to the file for its initialized allocation
   !!
   !! @return file - file object for setting up the empty file
   !---------------------------------------------------------------------------
   function new_empty() result(file)
      type (InternalFile_type) :: file
      allocate(file%lines(1:0)) ! 0 sized array
   end function new_empty

   !---------------------------------------------------------------------------
   !> Gets the new strings in lines from the file 
   !!
   !! @param file - given file object
   !!
   !! @return file - updated file object for getting new strings in its lines
   !---------------------------------------------------------------------------
   function new_strings(lines) result(file)
      type (InternalFile_type) :: file
      character(len=*), intent(in) :: lines(:)

      integer :: n_lines

      !! @todo shouldn't leave commented-out code below
!    ASSERT(len(lines) <= MAX_LINE_LENGTH)

      n_lines=Size(lines)
      allocate(file%lines(n_lines))
      file%lines = lines

   end function new_strings

   !---------------------------------------------------------------------------
   !> Gets the number of lines for its length of the file
   !!
   !! @param file - given file object
   !!
   !! @return n - the number of lines that file has
   !---------------------------------------------------------------------------
   function NumLines(file) result(n)
      integer :: n
      type (InternalFile_type) :: file

      !! @todo shouldn't leave commented-out code below
!    ASSERT(associated(file%lines))
      n = size(file%lines)

   end function NumLines

   !---------------------------------------------------------------------------
   !> Gets the specific line from the given specific line of the file.
   !!
   !! @param file - given file object
   !! @param ith - given the specific line number for receiving 
   !!
   !! @return line - line in string from the specific line number of the file
   !---------------------------------------------------------------------------
   function GetLine(file, ith) result(line)
      character(len=MAX_LINE_LENGTH) :: line
      type (InternalFile_type), intent(in) :: file
      integer,               intent(in) :: ith

      !! @todo shouldn't leave commented-out code below
!    ASSERT(associated(file%lines))
!    ASSERT(ith > 0)
!    ASSERT(ith <= NumLines(file))

      line = file%lines(ith)

   end function GetLine

   !---------------------------------------------------------------------------
   !> Appends the given line into the existing file
   !!
   !! @param file - given file object
   !! @param line - given line in string
   !!
   !! @return file - updated file object
   !---------------------------------------------------------------------------
   subroutine AppendLine(file, line)
      type (InternalFile_type), intent(inout) :: file
      character(len=*),      intent(in)    :: line

      character(len=MAX_LINE_LENGTH), pointer :: newlines(:)
      integer :: n_lines

      !! @todo shouldn't leave commented-out code below
!    ASSERT(len(line) <= MAX_LINE_LENGTH)
!    ASSERT(associated(file%lines))

      n_lines = size(file%lines)

      allocate(newlines(n_lines+1))
      newlines(:n_lines) = file%lines(:)
    
      if (associated(file%lines)) deallocate(file%lines)

      file%lines=> newlines
      file%lines(n_lines+1)=trim(line)

   end subroutine AppendLine
  

   !---------------------------------------------------------------------------
   !> Cleans up the file for deallocating it.
   !!
   !! @param file - given file object
   !---------------------------------------------------------------------------
   subroutine clean_(file)
      type (InternalFile_type) :: file

      !! @todo shouldn't leave commented-out code below
!    ASSERT(associated(file%lines))

      deallocate(file%lines)
   end subroutine clean_

   !---------------------------------------------------------------------------
   !> Loads the input file by the given file name
   !!
   !! @param fname - given file name 
   !! 
   !! @return file - file object 
   !!
   !! @note it will exit if failure of reading the file
   !---------------------------------------------------------------------------
   function LoadFile(fname) result(file)
      type (InternalFile_type) :: file
      character(len=*), intent(in) :: fname

      character(len=MAX_LINE_LENGTH) :: line
      integer :: unit, stat

      file=InternalFile()

      unit = GetUnit()
      open(unit=unit, file=trim(fname), form='FORMATTED')

      do
         read(unit,'(a)',iostat=stat) line
         if (stat < 0) exit ! eof?
         call AppendLine(file, trim(line))
      end do

      close(unit)
    
   end function LoadFile

   !---------------------------------------------------------------------------
   !> Writes the file of the given file name
   !!
   !! @param file - given file object 
   !! @param fname - given file name 
   !!
   !---------------------------------------------------------------------------
   subroutine WriteFile(file, fname)
      type (InternalFile_type) :: file
      character(len=*), intent(in) :: fname

      integer :: unit
      integer :: ith

      !! @todo shouldn't leave commented-out code below
!    ASSERT(associated(file%lines))

      unit = GetUnit()
      open(unit=unit, file=trim(fname), form='FORMATTED')

      do ith = 1, size(file%lines)
         write(unit,'(a)') file%lines(ith)
      end do

      close(unit)
    
   end subroutine WriteFile

   !---------------------------------------------------------------------------
   !> Gets the logical unit number for its input/output utility
   !!
   !! @return getunit get unit number 
   !---------------------------------------------------------------------------
   integer function getunit()
      integer :: unit
    
      integer, parameter :: MIN_UNIT=10, MAX_UNIT=99
      logical :: opened
    
      do unit = MIN_UNIT, MAX_UNIT
         inquire(unit=unit, opened=opened)
         if (.not. opened) exit
      end do

      !! @todo shouldn't leave commented-out code below
!    ASSERT(unit<= MAX_UNIT)

      getunit = unit
   end function getunit

end module InternalFile_mod
