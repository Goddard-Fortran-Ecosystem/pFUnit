!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: PrivateException
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
   public :: newException
   public :: ExceptionList
   public :: newExceptionList

   public :: NULL_MESSAGE
   public :: NULL_EXCEPTION
   public :: UNKNOWN_LINE_NUMBER
   public :: UNKNOWN_FILE_NAME

   character(len=*), parameter :: NULL_MESSAGE = ''

   type Exception
      character(len=:), allocatable :: message
      type (SourceLocation) :: location = UNKNOWN_SOURCE_LOCATION
      logical :: nullFlag = .true.
   contains
      procedure :: getMessage
      procedure :: getLineNumber
      procedure :: getFileName
      procedure :: isNull
   end type Exception

   type (Exception), parameter :: NULL_EXCEPTION = Exception(null(), UNKNOWN_SOURCE_LOCATION, .true.)

   type ExceptionList
      type (Exception), allocatable :: exceptions(:)
   contains

      procedure :: getNumExceptions

      procedure :: catch_any
      procedure :: catchNext
      procedure :: gather
      procedure :: catch_message
      generic :: catch => catch_any
      generic :: catch => catch_message
      procedure :: getExceptions
      procedure :: noExceptions
      procedure :: anyExceptions
      procedure :: clearAll
      procedure, private :: deleteIthException

      generic :: throw => throwMessage
      generic :: throw => throwException

      procedure :: throwMessage
      procedure :: throwException
!TODO - NAG does not yet support FINAL keyword
!!$$      final :: delete
   end type ExceptionList

   interface newException
      module procedure Exception_
   end interface

contains

   type(Exception) function Exception_(message, location)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      if (present(message)) then
         Exception_%message = trim(message)
      else
         Exception_%message = NULL_MESSAGE
      end if

      if (present(location)) then
         Exception_%location = location
      else
         Exception_%location = UNKNOWN_SOURCE_LOCATION
      end if

      Exception_%nullFlag = .false.

    end function Exception_

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
      isNull = this%nullFlag
   end function isNull

   function newExceptionList() result(list)
      type (ExceptionList) :: list
      if (allocated(list%exceptions)) then
         deallocate(list%exceptions)
      end if
      allocate(list%exceptions(0))
   end function newExceptionList

   integer function getNumExceptions(this)
      class (ExceptionList), intent(in) :: this

      getNumExceptions = size(this%exceptions)

   end function getNumExceptions

   subroutine throwMessage(this, message, location)
      class (ExceptionList), intent(inOut) :: this
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call this%throw(newException(message, location))

   end subroutine throwMessage

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

   function catchNext(this, preserve) result(anException)
      class (ExceptionList), intent(inOut) :: this
      logical, optional, intent(in) :: preserve
      type (Exception) :: anException
      if (size(this%exceptions) > 0) then
         anException = this%exceptions(1)
         call this%deleteIthException(1, preserve)
      else
         anException = NULL_EXCEPTION
      end if

   end function catchNext

   subroutine gather(this, context)
      use PF_ParallelContext_mod
      class (ExceptionList), intent(inOut) :: this
      class (ParallelContext), intent(in) :: context

      type (ExceptionList) :: list
      integer :: n_global_exceptions, n_local_exceptions
      integer :: i
      integer :: max_len
      character(len=:), allocatable :: global_messages(:)
      character(len=:), allocatable :: local_messages(:)

      n_local_exceptions = this%getNumExceptions()
      n_global_exceptions = context%sum(size(this%exceptions))

      if (n_global_exceptions > 0) then

         allocate(list%exceptions(n_global_exceptions))

         do i = 1, n_local_exceptions
            print*,__FILE__,__LINE__, i, 'before: ', trim(this%exceptions(i)%message)
            call context%labelProcess(this%exceptions(i)%message)
            print*,__FILE__,__LINE__, i, 'after: ', trim(this%exceptions(i)%message)
         end do
         call context%gather(this%exceptions(:)%nullFlag, list%exceptions(:)%nullFlag)
         call context%gather(this%exceptions(:)%location%fileName, list%exceptions(:)%location%fileName)
         call context%gather(this%exceptions(:)%location%lineNumber, list%exceptions(:)%location%lineNumber)

         ! variable length strings create some complexity
         max_len = maxval([(len(this%exceptions(i)%message),i=1,n_local_exceptions)])
         max_len = context%maximum(max_len)
         
         allocate(character(len=max_len) :: local_messages(n_local_exceptions))
         do i = 1, n_local_exceptions
            local_messages(i) = this%exceptions(i)%message
         end do

         call clearAll(this)

         allocate(character(len=max_len) :: global_messages(n_global_exceptions))
         call context%gather(local_messages, global_messages)

         if (context%isRootProcess()) then
            do i = 1, n_global_exceptions
               list%exceptions(i)%message = trim(global_messages(i))
            end do

            deallocate(this%exceptions)
            allocate(this%exceptions(n_global_exceptions))
            this%exceptions(:) = list%exceptions
         end if


         call clearAll(list)

      end if

   end subroutine gather

   logical function noExceptions(this)
      class (ExceptionList), intent(inOut) :: this

      noExceptions = .not. this%anyExceptions()

   end function noExceptions

   logical function anyExceptions(this)
      class (ExceptionList), intent(inOut) :: this

      anyExceptions = (this%getNumExceptions() > 0)

   end function anyExceptions

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

   logical function catch_any(this, preserve)
      class (ExceptionList), intent(inOut) :: this
      logical, optional, intent(in) :: preserve

      integer :: n
      logical :: preserve_ ! for default value

      n = this%getNumExceptions()

      if (n >= 1) then
         catch_any =.true.
         preserve_ = .false.
         if (present(preserve)) preserve_ = preserve
         if (.not. preserve_) call this%deleteIthException(n, preserve)
         return
      end if

      catch_any =.false.

   end function catch_any

   logical function catch_message(this, message, preserve)
      class (ExceptionList), intent(inOut) :: this
      character(len=*), intent(in) :: message
      logical, optional, intent(in) :: preserve

      integer :: i, n
      logical :: preserve_ ! for default value

      n = this%getNumExceptions()

      do i = 1, n
         if (trim(message) == this%exceptions(i)%getMessage()) then
            catch_message =.true.
            preserve_ = .false.
            if (present(preserve)) preserve_ = preserve
            if (.not. preserve_) call this%deleteIthException(i, preserve)
            return
         end if
      end do
      catch_message =.false.

   end function catch_message

   function getExceptions(this) result(exceptions)
      type (Exception), allocatable :: exceptions(:)
      class (ExceptionList), intent(inOut) :: this

      call move_alloc(from=this%exceptions, to=exceptions)
      allocate(this%exceptions(0))

   end function getExceptions

   subroutine clearAll(this)
      class (ExceptionList), intent(inOut) :: this
      deallocate(this%exceptions)
      allocate(this%exceptions(0))
   end subroutine clearAll

   subroutine delete(this)
      type (ExceptionList), intent(inOut) :: this
      if (allocated(this%exceptions)) deallocate(this%exceptions)
   end subroutine delete

end module PF_PrivateException_mod

module PF_Exception_mod
   use PF_SourceLocation_mod
   use PF_PrivateException_mod
   implicit none
   private

   public :: Exception
   public :: newException
   public :: ExceptionList
   public :: newExceptionList

   public :: NULL_MESSAGE
   public :: UNKNOWN_LINE_NUMBER
   public :: UNKNOWN_FILE_NAME

   public :: getNumExceptions
   public :: throw
   public :: gatherExceptions
   public :: catchNext
   public :: catch
   public :: getExceptions
   public :: noExceptions
   public :: anyExceptions
   public :: anyErrors
   public :: clearAll

   public :: initializeGlobalExceptionList

   type (ExceptionList), save :: globalExceptionList ! private
   logical, save :: init = .false. ! private


  interface throw
    module procedure throw_message
  end interface

  interface catch
     module procedure catch_any
     module procedure catch_message
  end interface catch

  interface anyExceptions
     module procedure anyExceptions_local
     module procedure anyExceptions_context
  end interface anyExceptions

  interface getNumExceptions
     module procedure getNumExceptions_local
     module procedure getNumExceptions_context
  end interface getNumExceptions

contains

   subroutine initializeGlobalExceptionList()
      globalExceptionList = newExceptionList()
   end subroutine initializeGlobalExceptionList


   integer function getNumExceptions_local() result(numExceptions)

      if (.not. init) then
         call initializeGlobalExceptionList()
         init = .true.
      end if

      numExceptions = globalExceptionList%getNumExceptions()
   end function getNumExceptions_local

   integer function getNumExceptions_context(context) result(numExceptions)
      use PF_ParallelContext_mod, only: ParallelContext
      class (ParallelContext), intent(in) :: context

      numExceptions = context%sum(getNumExceptions())
      
   end function getNumExceptions_context


   subroutine throw_message(message, location)
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      !$omp critical
      if (.not. init) then
         init = .true.
         call initializeGlobalExceptionList()
         !$omp flush(init)
      end if

      call globalExceptionList%throw(message, location)
      !$omp end critical

   end subroutine throw_message

   function catchNext(preserve) result(anException)
      logical, optional, intent(in) :: preserve
      type (Exception) :: anException

      if (.not. allocated(globalExceptionList%exceptions)) then
         call initializeGlobalExceptionList()
      end if

      anException = globalExceptionList%catchNext(preserve)
   end function catchNext

   logical function catch_any(preserve)
      logical, optional, intent(in) :: preserve

      if (.not. allocated(globalExceptionList%exceptions)) then
         call initializeGlobalExceptionList()
      end if

      catch_any = globalExceptionList%catch(preserve)
   end function catch_any

   logical function catch_message(message, preserve)
      character(len=*), intent(in) :: message
      logical, optional, intent(in) :: preserve

      if (.not. allocated(globalExceptionList%exceptions)) then
         call initializeGlobalExceptionList()
      end if

      catch_message = globalExceptionList%catch(message, preserve)
   end function catch_message

   function getExceptions() result(exceptions)
      type (Exception), allocatable :: exceptions(:)

      if (.not. allocated(globalExceptionList%exceptions)) then
         call initializeGlobalExceptionList()
      end if
#ifdef INTEL_16
      call move_alloc(from=globalExceptionList%exceptions, to=exceptions)
#else
      exceptions = globalExceptionList%getExceptions()
#endif
         
   end function getExceptions

   logical function noExceptions()

      if (.not. allocated(globalExceptionList%exceptions)) then
         call initializeGlobalExceptionList()
      end if

      noExceptions = globalExceptionList%noExceptions()
   end function noExceptions

   logical function anyExceptions_local() result(any)

      if (.not. allocated(globalExceptionList%exceptions)) then
         call initializeGlobalExceptionList()
      end if

      any = globalExceptionList%anyExceptions()

   end function anyExceptions_local


   logical function anyExceptions_context(context) result(any)
      use PF_ParallelContext_mod, only: ParallelContext
      class (ParallelContext), intent(in) :: context

      any = context%allReduce(anyExceptions())

   end function anyExceptions_context


   logical function anyErrors()
      integer :: i
      integer :: n

      do i = 1, globalExceptionList%getNumExceptions()
         n = min(14,len(globalExceptionList%exceptions(i)%message))
         if (globalExceptionList%exceptions(i)%message(1:n) == 'RUNTIME-ERROR:') then
            anyErrors = .true.
            return
         end if
      end do
      anyErrors = .false.
   end function anyErrors

   subroutine gatherExceptions(context)
      use PF_ParallelContext_mod
      class (ParallelContext), intent(in) :: context
      call globalExceptionList%gather(context)
   end subroutine gatherExceptions


   subroutine clearAll()
      call globalExceptionList%clearAll()
   end subroutine clearAll

end module PF_Exception_mod
