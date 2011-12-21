!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: SharedObjLibUtilities
!
!> @brief
!! Adds support for interfacing with shared object libraries.   This 
!! significantly improves the user interface and obviates the need to
!! create wrappers for each test.
!
!! @author
!! Tom Clune, NASA/GSFC SIVO
!!
!! @date
!! 12 Mar 2007
!
! REVISION HISTORY:
! 12 Mar 2007 - Initial Version
! 14 Jul 2010 - Added prologue for Doxygen 
!-------------------------------------------------------------------------------
module SharedObjLibUtilities_mod
#if defined( NAG )
   use f90_unix_proc
#endif
   use CString_mod
   use BaseAddress_mod, only: KIND_POINTER
   implicit none

   private

   public :: SharedObjLibUtilities_type
   public :: openLibrary
   public :: closeLibrary
   public :: name
   public :: getProcedureHandle
   public :: externalNames

   public :: MAX_SYMBOL_LENGTH

   integer, parameter :: MAXLEN=128
   integer, parameter :: MAX_SYMBOL_LENGTH=80

   type SharedObjLibUtilities_type
      private
      integer(kind=KIND_POINTER) :: handle
      character(len=MAXLEN) :: libraryName
   end type SharedObjLibUtilities_type

   integer, parameter :: MAX_LEN_ERROR_MESSAGE = 200

   character(len=*), parameter :: NO_SYMBOL='-----'

contains

   !---------------------------------------------------------------------------
   !> Open the library with the given name. 
   !!
   !! @param libraryName - given name of the library
   !!
   !! @return new object of shared object library
   !---------------------------------------------------------------------------
   function openLibrary(libraryName) result(this)
      character (len=*), intent(in):: libraryName
      type(SharedObjLibUtilities_type) :: this
      external cdlOpen
      character(len=MAX_LEN_ERROR_MESSAGE) :: message
      integer :: returnCode
      logical :: ok

      this % libraryName = trim(libraryName)
      call cdlOpen (this % handle, cString(libraryName), message)
      ok = checkMessage(message)

   end function openLibrary

   !---------------------------------------------------------------------------
   !> Gets the name of shared object library
   !!
   !! @param this - this object of the shared object library
   !!
   !! @return name of shared object library
   !---------------------------------------------------------------------------
   function name(this)

      character(len=MAXLEN) :: name
      type(SharedObjLibUtilities_type), intent(in) :: this

      name = trim(this % libraryName)

   end function name

   !---------------------------------------------------------------------------
   !> Gets the object of procedure pointer for handling the procedure
   !!
   !! @param this - this object of the shared object library
   !! @param name - given name 
   !! @param mangle - given optional mangle 
   !!
   !! @return name of shared object library
   !---------------------------------------------------------------------------
   function getProcedureHandle(this, name, mangle) result(procPointer)
      use ProcedurePointer_mod
      use BaseAddress_mod
      use FortranNameMangle_mod
      type(SharedObjLibUtilities_type) :: this
      character(len=*), intent(in) :: name
      logical, optional, intent(in) :: mangle
      type(ProcedurePointer_type) :: procPointer

      type(BaseAddress_type) :: addr
      external cdlsym

      character(len=MAX_LEN_ERROR_MESSAGE) :: message
      logical :: mangle_

      mangle_ = .false.
      if (present(mangle)) mangle_ = mangle
      if (mangle_) then
         call cdlSym(addr, this%handle, cString(fortranNameMangle(name)), message)
      else
         call cdlSym(addr, this%handle, cString(name), message)
      end if

      if (checkMessage(message)) &
           & procPointer = ProcedurePointer(address = addr, name=trim(name))

   end function getProcedureHandle

   !---------------------------------------------------------------------------
   !> Closes the shared object library
   !!
   !! @param this - this object of the shared object library
   !---------------------------------------------------------------------------
   subroutine closeLibrary(this)
      type (SharedObjLibUtilities_type) :: this
      character(len=MAX_LEN_ERROR_MESSAGE) :: message
      logical :: ok
      external cdlClose

      call cdlClose(this%handle, message)
      ok = (checkMessage(message))

   end subroutine closeLibrary

   !---------------------------------------------------------------------------
   !> Checks if message has any null.
   !!
   !! @param message - given message 
   !!
   !! @throw Exception if the message is out of range 
   !!
   !! @return .true. if message is in the first index, .false. otherwise.
   !---------------------------------------------------------------------------
   logical function checkMessage(message)
      use pFUnitException_mod
      character(len=*), intent(in) :: message
      integer :: idx

      idx = scan(message,NULL)
      if (idx /= 1) call throw(Exception(message(:idx-1)))
      checkMessage = (idx == 1)

   end function checkMessage

   !---------------------------------------------------------------------------
   !> Handles the external procedure names 
   !!
   !! @param this - this object fo shared object library
   !!
   !! @return the name of external procedure 
   !---------------------------------------------------------------------------
   function externalNames(this) result(names)
      use IO_Utilities_mod
      use pFUnitException_mod
      type (SharedObjLibUtilities_type), intent(in) :: this
      character(len=MAX_SYMBOL_LENGTH), pointer :: names(:)
      character(len=*), parameter :: tmpFile = 'tmpOutputFromNM'
      integer :: unit
      integer :: numRecords

      call system('nm -g ' // trim(this % libraryName) // ' > ' // tmpFile)

      numRecords = readFile(tmpFile)
      allocate(names(numRecords))
      numRecords = readFile(tmpFile, names)
      call deleteFile(tmpFile)

   contains

      !------------------------------------------------------------------------
      !> Reads the file with given name and optional symbols 
      !!
      !! @param fileName - given name of the file
      !! @param symbols - optional symbols 
      !!
      !! @return the number of lines from the file 
      !------------------------------------------------------------------------
      integer function readFile(fileName, symbols) result(numLines)
         character(len=*), intent(in) :: fileName
         character(len=*), optional, intent(out) :: symbols(:)
         character(len=MAX_SYMBOL_LENGTH) :: symbolName

         integer :: unit
         integer :: n
         integer :: length

         unit = openFile(tmpFile,status='old',form='formatted')
         n = 0
         do
            call readLine(unit, symbolName)
            if (catch(END_OF_FILE)) exit
            if (trim(symbolName) /= NO_SYMBOL) then
               n = n + 1
               if (present(symbols)) symbols(n) = trim(symbolName)
            end if
         end do
         close(unit)

         numLines = n
      end function readFile

      !------------------------------------------------------------------------
      !> Reads line of the file with the given unit number and name of the 
      !! symbol.
      !! 
      !! @param unit - given unit number
      !! @param symbol - name of the symbol
      !------------------------------------------------------------------------
      subroutine readLine(unit, symbolName)
         integer, intent(in) :: unit
         character(len=*), intent(out) :: symbolName

         character(len=8) :: address
         character(len=1) :: symbolType
         integer :: status
         integer :: numIgnore
         character(len=100) :: buffer

         character(len=20) :: fmt

         read(unit,'(a100)',iostat=status) buffer

         if (status /= 0) then
            call throw(END_OF_FILE)
            return
         end if

         if (isEmptyLine(buffer)) then
            symbolName = NO_SYMBOL
            return
         end if

         if (isObjectFile(buffer)) then
            symbolName = NO_SYMBOL
            return
         end if

         if (.not. isTypeText(buffer)) then
            symbolName = NO_SYMBOL
            return
         end if

         ! skip leading underscore for symbolName - at least on OSX
#ifdef LINUX
         numIgnore = 0
#else
         numIgnore = 1
#endif

#ifdef LONG_PTR
         write(fmt,'("(a", i2, ",1x,a1,",i1,"x,a)")') 16, 1 + numIgnore
#else
         write(fmt,'("(a", i2, ",1x,a1,",i1,"x,a)")') 8, 1 + numIgnore
#endif
         read(buffer,trim(fmt)) address, symbolType, symbolName

#ifdef LINUX
         if (symbolName(1:1) == "_") then
            symbolName = NO_SYMBOL
            return
         end if
#endif

         if (isModuleSymbol(symbolName)) then
            symbolName = NO_SYMBOL
            return
         end if

      end subroutine readLine

      !------------------------------------------------------------------------
      !> Checks if the given buffer in the line is blank.
      !! 
      !! @param buffer - given buffer 
      !!
      !! @return .true. if line is empty, .false. otherwise.
      !------------------------------------------------------------------------
      logical function isEmptyLine(buffer)
         character(len=*), intent(in) :: buffer
         isEmptyLine = (len_trim(buffer) == 0)
      end function isEmptyLine

      !------------------------------------------------------------------------
      !> Checks if the given buffer is used for the file object.
      !! 
      !! @param buffer - given buffer 
      !!
      !! @return .true. if input is for the file object, .false. otherwise.
      !------------------------------------------------------------------------
      logical function isObjectFile(buffer)
         character(len=*), intent(in) :: buffer
         isObjectFile = (scan(buffer,':') /= 0)
      end function isObjectFile

      !------------------------------------------------------------------------
      !> Checks if the given buffer is used for the text type.
      !! 
      !! @param buffer - given buffer 
      !!
      !! @return .true. if buffer is the text type, .false. otherwise.
      !------------------------------------------------------------------------
      logical function isTypeText(buffer)
         character(len=*), intent(in) :: buffer
#ifdef LONG_PTR
         isTypeText = (buffer(18:18) == 'T')
!!$$         isTypeText = isTypeText .and. (buffer(20:20) /= "_")
         isTypeText = isTypeText .and. (index(buffer,'.') == 0)
#else
         isTypeText = (buffer(10:10) == 'T')
#endif

      end function isTypeText

      !------------------------------------------------------------------------
      !> Checks if the given buffer is used for the module symbol.
      !! 
      !! @param buffer - given buffer 
      !!
      !! @return .true. if buffer is used for the module, .false. otherwise.
      !------------------------------------------------------------------------
      logical function isModuleSymbol(buffer)
         character(len=*), intent(in) :: buffer

         ! IBM puts a text symbol associated with module name containing &&
         isModuleSymbol = (index(buffer, '&&') > 0)
         ! On OSX Intel puts a text symbol associated with module name containing "."
         isModuleSymbol = isModuleSymbol .or. (index(buffer,'.') > 0)
         

      end function isModuleSymbol

   end function externalNames

end module SharedObjLibUtilities_mod
