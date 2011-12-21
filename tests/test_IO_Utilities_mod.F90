module test_IO_Utilities_mod
   use pfunit, only: assertEqual
   use pfunit, only: assertTrue
   use pfunit, only: assertFalse
   use pfunit, only: raiseException
   use IO_Utilities_mod
   use pFUnitException_mod
   implicit none
   private

   public :: test_getUnit
   public :: test_getUnit2
   public :: test_openNew
   public :: test_open2Files
   public :: test_openNewFailExists
   public :: test_openFailIsOpen
   public :: test_openOld
   public :: test_openOldFail
   public :: test_deleteFile
   public :: test_deleteFileIsOpen
   public :: test_deleteFileDoesNotExist
   public :: test_deleteFileDoesNotExistIgnore
   public :: test_openUnformatted
   public :: test_openFormatted

   ! accessors
   public :: test_isConnected
   public :: test_isOpenUnit
   public :: test_isOpenFileName
   public :: test_getUnitFromName
   public :: test_getUnitFromNameFail

   public :: test_createNamedPipe

contains

   subroutine test_getUnit()

      integer :: unitNumber
      logical :: isOpen

      unitNumber = getUnit()

      if (catch(NO_AVAILABLE_UNITS)) then
         call raiseException(getMessage(NO_AVAILABLE_UNITS))
         return
      end if

      inquire(unit=unitNumber, opened = isOpen)
      call assertFalse(isOpen)

   end subroutine test_getUnit

   subroutine test_getUnit2()

      integer :: unitNumber1
      integer :: unitNumber2

      unitNumber1 = getUnit()
      open(file='temporaryFile',unit=unitNumber1)

      unitNumber2 = getUnit()
      call assertFalse(unitNumber2 == unitNumber1)
      close(unitNumber1, status='delete')

   end subroutine test_getUnit2

   subroutine test_openNew()
      character(len=*), parameter :: fileName = 'tmpNewFile'
      integer :: unit
      logical :: exists

      ! make a file
      unit = openFile(fileName, status='new', form='unformatted')

      if (catch(FILE_EXISTS)) then
         call raiseException(getMessage(FILE_EXISTS))
         return
      end if

      inquire(file=fileName, exist=exists)
      call assertTrue(exists)

      close(unit,status='delete')

   end subroutine test_openNew

   subroutine test_openNewFailExists()
      character(len=*), parameter :: fileName = 'tmpNewFail'
      integer :: unit, status

      ! make a file
      unit = openFile(fileName, status='new', form='unformatted')
      close(unit)

      ! open again - but fail
      unit = openFile(fileName, status='new', form='unformatted')
      if (.not. catch(FILE_EXISTS)) then
         call raiseException('failed to detect file already exists')
      end if

      unit = getUnit()
      open(unit, file=fileName, status='unknown')
      close(unit, status='delete')

   end subroutine test_openNewFailExists

   subroutine test_openFailIsOpen()
      character(len=*), parameter :: fileName = 'tmpFailIsOpen'
      integer :: unit1, unit2

      ! make a file
      unit1 = openFile(fileName, status='new', form='unformatted')

      ! open again - but fail
      unit2 = openFile(fileName, status='unknown', form='unformatted')
      if (.not. catch(FILE_IS_OPEN)) then
         call raiseException('failed to detect file is already open')
         close(unit2, status='delete') ! is this line a good idea?
      end if

      close(unit1, status='delete')

   end subroutine test_openFailIsOpen

   subroutine test_openOld()
      character(len=*), parameter :: fileName = 'tests/testData/OldFile'
      logical :: isOpen
      integer :: unit, whichUnit

      ! make a file
      unit = openFile(fileName, status='old', form='unformatted')

      if (catch(FILE_EXISTS)) then
         call raiseException('The file _should_ exist - there should not be an exception.')
         return
      end if

      inquire(file=fileName, number=whichUnit, opened=isOpen)
      call assertEqual(unit, whichUnit)
      if (isOpen) then
         close(unit)
      else
         call raiseException('File was apparently not opened.')
      end if

   end subroutine test_openOld

   subroutine test_openOldFail()
      character(len=*), parameter :: fileName = 'oldFileFail'
      logical :: isOpen
      integer :: unit

      ! make a file
      unit = openFile(fileName, status='old', form='unformatted')

      if (.not. catch(FILE_DOES_NOT_EXIST)) then
         call raiseException('This file should not have already existed.')
         return
      end if

      inquire(file=fileName, opened=isOpen)
      call assertFalse(isOpen)

      if (isOpen) then
         close(unit,status='delete')
      end if

   end subroutine test_openOldFail

   subroutine test_open2Files()
      character(len=*), parameter :: file1='tmpfile1'
      character(len=*), parameter :: file2='tmpfile2'
      integer :: unitNumber1
      integer :: unitNumber2
      logical :: isOpen
      integer :: whichUnit

      unitNumber1 = OpenFile(file1,status='new',form='unformatted')
      unitNumber2 = OpenFile(file2,status='new',form='unformatted')

      inquire(file=file1, number=whichUnit, opened=isOpen)
      call assertTrue(isOpen,'not open 1')
      call assertEqual(whichUnit, unitNumber1,'not unitnumber 1')

      inquire(file=file2, number=whichUnit, opened=isOpen)
      call assertTrue(isOpen,'not open 2')
      call assertEqual(whichUnit, unitNumber2,'not unitnumber 2')

      call assertFalse(unitNumber1 == unitNumber2)

      close(unitNumber1, status='delete')
      close(unitNumber2, status='delete')

   end subroutine test_open2Files

   subroutine test_isConnected()
      character(len=*), parameter :: file1='tmpfile1'
      character(len=*), parameter :: file2='tmpfile2'
      integer :: unitNumber1
      integer :: unitNumber2
      logical :: isOpen
      integer :: whichUnit

      unitNumber1 = OpenFile(file1,status='new',form='unformatted')
      unitNumber2 = OpenFile(file2,status='new',form='unformatted')

      inquire(file=file1, number=whichUnit, opened=isOpen)
      call assertTrue(isOpen,'not open 1')
      call assertEqual(whichUnit, unitNumber1,'not unitnumber 1')

      inquire(file=file2, number=whichUnit, opened=isOpen)
      call assertTrue(isOpen,'not open 2')
      call assertEqual(whichUnit, unitNumber2,'not unitnumber 2')

      call assertFalse(unitNumber1 == unitNumber2)

      call assertTrue(isConnected(unitNumber1, file1))
      call assertTrue(isConnected(unitNumber2, file2))
      call assertFalse(isConnected(unitNumber1,file2))
      call assertFalse(isConnected(unitNumber2,file1))

      close(unitNumber1, status='delete')
      close(unitNumber2, status='delete')

   end subroutine test_isConnected

   subroutine test_deleteFile()
      character(len=*), parameter :: fileName='deleteThis'
      integer :: unit
      logical :: exists

      unit = openFile(fileName, status='new',form='unformatted')
      close(unit)

      inquire(file=fileName, exist=exists)
      call assertTrue(exists,'file not created')
      
      call deleteFile(fileName)
      
      inquire(file=fileName, exist=exists)
      call assertFalse(exists,'file not deleted')

   end subroutine test_deleteFile

   subroutine test_deleteFileIsOpen()
      character(len=*), parameter :: fileName='cannotDeleteThis'
      integer :: unit
      logical :: exists

      unit = openFile(fileName, status='new',form='unformatted')
      
      call deleteFile(fileName)
      
      if (.not. catch(FILE_IS_OPEN)) then
         call raiseException('Should not delete files that are open.')
      end if

      close(unit,status='delete')

      inquire(file=fileName, exist=exists)
      call assertFalse(exists,'file not deleted')

   end subroutine test_deleteFileIsOpen

   subroutine test_deleteFileDoesNotExist()
      character(len=*), parameter :: fileName='doesNotExist'
      logical :: exists

      inquire(file=fileName, exist=exists)
      call assertFalse(exists)

      call deleteFile(fileName)
      
      if (.not. catch(FILE_DOES_NOT_EXIST)) then
         call raiseException('Failed to warn about delete of file that did not exist.')
      end if

   end subroutine test_deleteFileDoesNotExist

   subroutine test_deleteFileDoesNotExistIgnore()
      character(len=*), parameter :: fileName='doesNotExist'
      logical :: exists

      inquire(file=fileName, exist=exists)
      call assertFalse(exists)

      call deleteFile(fileName,ignore=.true.)
      
      if (catch(FILE_DOES_NOT_EXIST)) then
         call raiseException('Failed to ignore problem that file that did not exist.')
      end if

   end subroutine test_deleteFileDoesNotExistIgnore

   subroutine test_isOpenUnit()
      integer :: unit

      unit = getUnit()
      call assertFalse(isOpen(unit))
      open(unit,file='tmpFile')
      call assertTrue(isOpen(unit))
      close(unit,status='delete')

   end subroutine test_isOpenUnit

   subroutine test_isOpenFileName()
      integer :: unit
      character(len=*), parameter :: fileName='tmpIsOpenFileName'

      call assertFalse(isOpen(fileName))
      unit = getUnit()
      open(unit, file=fileName)
      call assertTrue(isOpen(fileName))
      close(unit,status='delete')

   end subroutine test_isOpenFileName

   subroutine test_GetUnitFromName()
      character(len=*), parameter :: fileName='tmpGetUnitFromName'
      integer :: unit

      unit = getUnit()
      open(unit, file=fileName)
      call assertEqual(unit, getUnit(fileName))
      close(unit, status='delete')
      
   end subroutine test_GetUnitFromName

   subroutine test_GetUnitFromNameFail()
      character(len=*), parameter :: fileName='tmpGetUnitFromNameFail'
      integer :: unit

      unit = getUnit(fileName)
      if (.not. catch(FILE_NOT_ASSOCIATED)) then
         call raiseException('Failed to trap improper call to getUnit().')
      end if
      
   end subroutine test_GetUnitFromNameFail

   subroutine test_openUnformatted()
      character(len=*), parameter :: fileName = 'tmpNewFile'
      character(len=80) :: whichFormat
      integer :: unit
      logical :: exists

      ! make a file
      unit = openFile(fileName, status='new', form='unformatted')

      if (catch(FILE_EXISTS)) then
         call raiseException(getMessage(FILE_EXISTS))
         return
      end if

      inquire(file=fileName, exist=exists,form=whichFormat)

      call assertTrue(any((/ 'unformatted', 'UNFORMATTED', 'Unformatted' /) ==  trim(whichFormat)))

      close(unit,status='delete')
   end subroutine test_openUnformatted

   subroutine test_openFormatted()
      character(len=*), parameter :: fileName = 'tmpNewFile'
      character(len=80) :: whichFormat
      integer :: unit
      logical :: exists

      ! make a file
      unit = openFile(fileName, status='new', form='formatted')

      if (catch(FILE_EXISTS)) then
         call raiseException(getMessage(FILE_EXISTS))
         return
      end if

      inquire(file=fileName, exist=exists,form=whichFormat)

      call assertTrue(any((/ 'formatted', 'FORMATTED', 'Formatted' /) ==  trim(whichFormat)))

      close(unit,status='delete')

   end subroutine test_openFormatted

   subroutine test_createNamedPipe()
      use IO_Utilities_mod
      character(len=*), parameter :: pipeName = 'test_namedPipe'
      logical :: exists

      call createNamedPipe(pipeName)
      inquire(file = pipeName, exist = exists)
      call assertTrue(exists)
      call deleteFile(pipeName)

   end subroutine test_createNamedPipe

end module test_IO_Utilities_mod
