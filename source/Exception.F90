module PrivateException_mod
   implicit none
   private

   public :: Exception
   public :: newException
   public :: ExceptionList
   public :: newExceptionList

   public :: MAXLEN_MESSAGE
   public :: MAXLEN_FILE_NAME
   public :: NO_MESSAGE
   public :: UNKNOWN_LINE_NUMBER
   public :: UNKNOWN_FILE_NAME

   integer, parameter :: MAXLEN_MESSAGE = 80*10
   integer, parameter :: MAXLEN_FILE_NAME = 80
   integer, parameter :: UNKNOWN_LINE_NUMBER = -1
   character(len=*), parameter :: NO_MESSAGE = ''
   character(len=*), parameter :: UNKNOWN_FILE_NAME = "<unknown file>"

   type Exception
      character(len=MAXLEN_MESSAGE) :: message = NO_MESSAGE
      character(len=MAXLEN_FILE_NAME) :: fileName = UNKNOWN_FILE_NAME
      integer :: lineNumber = UNKNOWN_LINE_NUMBER
      logical :: nullFlag = .true.
   contains
      procedure :: getMessage
      procedure :: getLineNumber
      procedure :: getFileName
      procedure :: isNull
   end type Exception

   type (Exception), parameter :: NULL_EXCEPTION = Exception('NULL EXCEPTION', UNKNOWN_FILE_NAME, UNKNOWN_LINE_NUMBER, .true.)

   type ExceptionList
      type (Exception), allocatable :: exceptions(:)
   contains
      
      procedure :: getNumExceptions

      procedure :: catchAny
      procedure :: gather
      procedure :: catch
      procedure :: noExceptions
      procedure :: clearAll
      procedure, private :: deleteIthException

      generic :: throw => throwMessage
      generic :: throw => throwMessageWithLineNumber
      generic :: throw => throwMessageWithLineAndFile
      generic :: throw => throwException

      procedure :: throwMessage
      procedure :: throwMessageWithLineNumber
      procedure :: throwMessageWithLineAndFile
      procedure :: throwException

      final :: delete
   end type ExceptionList

   interface newException
      module procedure Exception_
      module procedure Exception_message
      module procedure Exception_messageAndLineNumber
      module procedure Exception_messageAndLineAndFile
   end interface

contains

   type(Exception) function Exception_()
      Exception_%message = ''
      Exception_%nullFlag = .false.
   end function Exception_

   type(Exception) function Exception_message(message)
      character(len=*), intent(in) :: message
      Exception_message%message = trim(message)
      Exception_message%nullFlag = .false.
   end function Exception_message

   type(Exception) function Exception_messageAndLineNumber(message, lineNumber)
      character(len=*), intent(in) :: message
      integer, intent(in) :: lineNumber
      Exception_messageAndLineNumber%message = message
      Exception_messageAndLineNumber%lineNumber =  lineNumber
      Exception_messageAndLineNumber%nullFlag = .false.
   end function Exception_messageAndLineNumber

   type(Exception) function Exception_messageAndLineAndFile(message, lineNumber, fileName)
      character(len=*), intent(in) :: message
      integer, intent(in) :: lineNumber
      character(len=*), intent(in) :: fileName
      Exception_messageAndLineAndFile%message = message
      Exception_messageAndLineAndFile%lineNumber =  lineNumber
      Exception_messageAndLineAndFile%fileName =  fileName
      Exception_messageAndLineAndFile%nullFlag = .false.
   end function Exception_messageAndLineAndFile

   function getMessage(this) result(message)
      class (Exception), intent(in) :: this
      character(len=len_trim(this%message)) :: message
      message = trim(this%message)
   end function getMessage

   integer function getLineNumber(this) 
      class (Exception), intent(in) :: this
      getLineNumber = this%lineNumber
   end function getLineNumber

   character(len=MAXLEN_FILE_NAME) function getFileName(this) 
      class (Exception), intent(in) :: this
      getFileName = trim(this%fileName)
   end function getFileName

   logical function isNull(this) 
      class (Exception), intent(in) :: this
      isNull = this%nullFlag
   end function isNull

   function newExceptionList() result(list)
      type (ExceptionList) :: list
      allocate(list%exceptions(0))
   end function newExceptionList

   integer function getNumExceptions(this)
      class (ExceptionList), intent(in) :: this
      getNumExceptions = size(this%exceptions)
   end function getNumExceptions

   subroutine throwMessage(this, message)
      class (ExceptionList), intent(inOut) :: this
      character(len=*), intent(in) :: message

      call this%throw(newException(message))

   end subroutine throwMessage

   subroutine throwMessageWithLineNumber(this, message, lineNumber)
      class (ExceptionList), intent(inOut) :: this
      character(len=*), intent(in) :: message
      integer, intent(in) :: lineNumber

      call this%throw(newException(message, lineNumber))

   end subroutine throwMessageWithLineNumber

   subroutine throwMessageWithLineAndFile(this, message, lineNumber, fileName)
      class (ExceptionList), intent(inOut) :: this
      character(len=*), intent(in) :: message
      integer, intent(in) :: lineNumber
      character(len=*), intent(in) :: fileName

      call this%throw(Exception(message, fileName, lineNumber, .false.))

   end subroutine throwMessageWithLineAndFile

   subroutine throwException(this, anException)
      class (ExceptionList), intent(inOut) :: this
      type (Exception), intent(in) :: anException

      type (Exception), allocatable :: tmp(:)
      integer :: n

      n = size(this%exceptions)
      allocate(tmp(n+1))
      tmp(1:n) = this%exceptions
      tmp(n+1) = anException
      deallocate(this%exceptions)
      allocate(this%exceptions(n+1))
      this%exceptions = tmp
      deallocate(tmp)

   end subroutine throwException

   function catchAny(this, preserve) result(anException)
      class (ExceptionList), intent(inOut) :: this
      logical, optional, intent(in) :: preserve
      type (Exception) :: anException
      if (size(this%exceptions) > 0) then
         anException = this%exceptions(1)
         call this%deleteIthException(1, preserve)
      else
         anException = NULL_EXCEPTION
      end if

   end function catchAny

   subroutine gather(this, context)
      use ParallelContext_mod
      class (ExceptionList), intent(inOut) :: this
      class (ParallelContext), intent(in) :: context

      type (ExceptionList) :: list
      integer :: globalExceptionCount
      character(len=MAXLEN_MESSAGE) :: msg
      integer :: i

      globalExceptionCount = context%sum(size(this%exceptions))

      if (globalExceptionCount > 0) then

         allocate(list%exceptions(globalExceptionCount))
         do i = 1, this%getNumExceptions()
            write(msg,'(a," (process ",i0," of ",i0,")")') trim(this%exceptions(i)%message), &
                 & context%processRank(), context%getNumProcesses()
            this%exceptions(i)%message = msg
         end do
         call context%gather(this%exceptions(:)%message, list%exceptions(:)%message)
         call context%gather(this%exceptions(:)%fileName, list%exceptions(:)%fileName)
         call context%gather(this%exceptions(:)%lineNumber, list%exceptions(:)%lineNumber)
         call context%gather(this%exceptions(:)%nullFlag, list%exceptions(:)%nullFlag)

         deallocate(this%exceptions)
         allocate(this%exceptions(globalExceptionCount))
         this%exceptions(:) = list%exceptions
         call clearAll(list)

      end if

   end subroutine gather

   logical function noExceptions(this)
      class (ExceptionList), intent(inOut) :: this

      noExceptions = (this%getNumExceptions() == 0)

   end function noExceptions

   ! Fortran does not require "short-circuit" so be careful with 
   ! evaluation of optional arguments.
   logical function preserveMessage(preserve)
      logical, optional, intent(in) :: preserve

      preserveMessage = .false. ! default
      if (present(preserve)) preserveMessage = preserve

   end function preserveMessage

   subroutine deleteIthException(this, i, preserve)
      class (ExceptionList), intent(inOut) :: this
      integer, intent(in) :: i
      logical, optional, intent(in) :: preserve

      type (Exception), allocatable :: tmp(:)
      integer :: n

      if (preserveMessage(preserve)) return

      n = this%getNumExceptions()
      if (n == 0) return ! cannot throw exceptions here, alas
      allocate(tmp(n-1))
      tmp(1:i-1) = this%exceptions(1:i-1)
      tmp(i:n-1) = this%exceptions(i+1:n)
      deallocate(this%exceptions)
      allocate(this%exceptions(n-1))
      this%exceptions = tmp
      deallocate(tmp)

   end subroutine deleteIthException

   logical function catch(this, message, preserve) 
      class (ExceptionList), intent(inOut) :: this
      character(len=*), intent(in) :: message
      logical, optional, intent(in) :: preserve

      integer :: i, n
      logical :: preserve_ ! for default value

      n = this%getNumExceptions()

      do i = 1, n
         if (trim(message) == this%exceptions(i)%getMessage()) then
            catch =.true.
            preserve_ = .false.
            if (present(preserve)) preserve_ = preserve
            if (.not. preserve_) call this%deleteIthException(i, preserve)
            return
         end if
      end do
      catch =.false.

   end function catch

   subroutine clearAll(this)
      class (ExceptionList), intent(inOut) :: this
      deallocate(this%exceptions)
      allocate(this%exceptions(0))
   end subroutine clearAll

   subroutine delete(this)
      type (ExceptionList), intent(inOut) :: this
      if (allocated(this%exceptions)) deallocate(this%exceptions)
   end subroutine delete

end module PrivateException_mod

module Exception_mod
   use PrivateException_mod
   implicit none
   private

   public :: Exception
   public :: ExceptionList
   public :: newExceptionList

   public :: MAXLEN_MESSAGE
   public :: NO_MESSAGE
   public :: UNKNOWN_LINE_NUMBER
   public :: UNKNOWN_FILE_NAME

   public :: getNumExceptions
   public :: throw
   public :: gatherExceptions
   public :: catchAny
   public :: catch
   public :: noExceptions
   public :: clearAll

   public :: initializeGlobalExceptionList
   public :: initializeNewLine
   public :: NEWLINE

   type (ExceptionList) :: globalExceptionList ! private
   character(len=1), protected :: NEWLINE

  interface throw
    module procedure throw_message
    module procedure throw_messageWithLineNumber
    module procedure throw_messageWithLineAndFile
  end interface

contains

   subroutine initializeGlobalExceptionList()
      globalExceptionList = newExceptionList()
   end subroutine initializeGlobalExceptionList

   subroutine initializeNewLine()
      NEWLINE = new_line('a')
   end subroutine initializeNewLine

   integer function getNumExceptions()
      getNumExceptions = globalExceptionList%getNumExceptions()
   end function getNumExceptions

   subroutine throw_message(message)
      character(len=*), intent(in) :: message
      call globalExceptionList%throw(message)
   end subroutine throw_message

   subroutine throw_messageWithLineNumber(message, lineNumber)
      character(len=*), intent(in) :: message
      integer, intent(in) :: lineNumber
      call globalExceptionList%throw(message, lineNumber)
   end subroutine throw_messageWithLineNumber

   subroutine throw_messageWithLineAndFile(message, lineNumber, fileName)
      character(len=*), intent(in) :: message
      integer, intent(in) :: lineNumber
      character(len=*), intent(in) :: fileName
      call globalExceptionList%throw(message, lineNumber, fileName)
   end subroutine throw_messageWithLineAndFile

   function catchAny(preserve) result(anException)
      logical, optional, intent(in) :: preserve
      type (Exception) :: anException

      anException = globalExceptionList%catchAny(preserve)
   end function catchAny

   logical function catch(message, preserve)
      character(len=*), intent(in) :: message
      logical, optional, intent(in) :: preserve

      catch = globalExceptionList%catch(message, preserve)
   end function catch

   logical function noExceptions()
      noExceptions = globalExceptionList%noExceptions()
   end function noExceptions

   subroutine gatherExceptions(context)
      use ParallelContext_mod
      class (ParallelContext), intent(in) :: context
      call globalExceptionList%gather(context)
   end subroutine gatherExceptions

   subroutine clearAll()
      call globalExceptionList%clearAll()
   end subroutine clearAll

end module Exception_mod
