!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: pFUnitException
!
!> @brief
!! Provides a global mechanism for signaling exceptions/failures between 
!! different software elements without aborting.  Fortran does not provide any 
!! mechanism analogous to Try/Catch in other languages, so this module is 
!! intended to act as a supplement in that direction.  Lack of direct support 
!! in the language does limit the capabilities somewhat.
!!
!> @par Limitations
!! No suitable mechanism exists for throw() to jump to the catch handler.
!! The user of this layer must implement an appropriate sequence of 
!! returns that accomplishes this, perhaps by supplementing with a strict style for
!! such contsructs.  
!
!> @par Implementation
!! Exceptions are implemented as an opaque data type containing a string.
!!
!! A global, private list of objects of type "Exception" is maintained by this module.
!! Calls are provided to create new Exceptions, push/pop entries in the list, and manipulate the
!! list entries via queries.
!!
!! A global private \b stack of indices is used to track which exceptions are within which Try block.
!! 
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 29 Aug 2006
!
! REVISION HISTORY:
! 29 Aug 2006 - Initial Version
! 13 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module pFUnitException_mod
   implicit none
   private
   
   public :: Exception_type ! derived type
   public :: Exception      ! constructor

   ! Public interfaces
   public :: getMessage
   public :: try
   public :: endTry
   public :: throw
   public :: catch
   public :: catchAny
   public :: clearAll
   public :: operator(==)
   public :: numExceptions
#ifdef USE_MPI
   public :: gatherExceptions
#endif

   ! Public parameters (constants)
   public :: NO_EXCEPTION
   public :: NO_AVAILABLE_UNITS
   public :: FILE_EXISTS
   public :: FILE_DOES_NOT_EXIST
   public :: FILE_IS_OPEN
   public :: FILE_NOT_ASSOCIATED
   public :: END_OF_FILE

   public :: NULL_PROCEDURE_POINTER
   public :: INCORRECT_NUMBER_OF_ARGUMENTS

   integer, parameter :: MAXLEN=996

   type Exception_type
      private
      integer :: uniqueID = -1
      character(len=MAXLEN) :: message
   end type Exception_type

   interface throw
     module procedure throwException
     module procedure throwString
   end interface

   interface catch
      module procedure catchGeneric
      module procedure catchSpecific
      module procedure catchMessage
   end interface

   ! singleton list for managing exceptions
   type (Exception_type), allocatable, save :: exceptionStack(:)
   integer, allocatable, save :: tryStack(:)

   interface operator(==)
      module procedure equals
   end interface

   ! Predefined exceptions
   ! Negative ID's are used for built in exceptions for easy extension.
   type (Exception_type), parameter :: NO_EXCEPTION = &
        & Exception_type(-1,'none')
   type (Exception_type), parameter :: NO_AVAILABLE_UNITS = &
        & Exception_type(-2,'No units are available for fortran I/O.')
   type (Exception_type), parameter :: FILE_EXISTS = &
        & Exception_type(-3,'File already exists.')
   type (Exception_type), parameter :: FILE_DOES_NOT_EXIST = &
        & Exception_type(-4,'File does not already exist.')
   type (Exception_type), parameter :: FILE_IS_OPEN = &
        & Exception_type(-5,'File is already open.')
   type (Exception_type), parameter :: FILE_NOT_ASSOCIATED = &
        & Exception_type(-6,'File is not associated with any unit.')
   type (Exception_type), parameter :: END_OF_FILE = &
        & Exception_type(-7,'End of file reached.')

   type (Exception_type), parameter :: NULL_PROCEDURE_POINTER = &
        & Exception_type(-8,'NULL or uninitialized procedure pointer passed to ProcedurePointer::invoke().')
   type (Exception_type), parameter :: INCORRECT_NUMBER_OF_ARGUMENTS = &
        & Exception_type(-9,'Incorrect number of arguments passed to ProcedurePointer::invoke().')

   integer, save :: globalExceptionCount = 0

contains

   !---------------------------------------------------------------------------
   !> Construct a new Exception object.
   !!
   !! @param message - identifying message for Exception
   !!
   !! @return new object of Exception
   !---------------------------------------------------------------------------
   function Exception(message) result (this)
      implicit none
      character(len=*), intent(in) :: message
      type (Exception_type) :: this

      globalExceptionCount = globalExceptionCount + 1
      this%uniqueID = globalExceptionCount
      this%message = trim(message)

   end function Exception

   !---------------------------------------------------------------------------
   !> "throwException" an exception object. Adds the specified exception object to the 
   !! stack.   
   !!
   !! @param this - this Exception object
   !!
   !---------------------------------------------------------------------------
   subroutine throwException(this)
      implicit none
      type (Exception_type), intent(in) :: this

      call extendExceptionStack()
      call put(this)

    end subroutine throwException

   !---------------------------------------------------------------------------
   !> "throwString" a string. Creates an exception and throws it.
   !!
   !! @param string - text of exception
   !!
   !---------------------------------------------------------------------------
   subroutine throwString(string)
      implicit none
      character(len=*), intent(in) :: string

      call throw(Exception(string))

    end subroutine throwString

   !---------------------------------------------------------------------------
   !> Catch (and delete) arbitrary exception from the list.
   !! If the list is not empty, this function returns an exception,
   !! otherwise NO_EXCEPTION is returned.  The user should not assume any
   !! implied order for which exception is accessed.
   !!
   !! @return object of Exception whether it is found or not.
   !---------------------------------------------------------------------------
   function catchAny() result(found)
      implicit none
      type (Exception_type) :: found
      integer :: sz

      sz = numExceptions()

      if (sz > 0) then
         found = exceptionStack(1)
         call deleteIthEntry(1)
         return
      end if
      
      found = NO_EXCEPTION

   end function catchAny

   !---------------------------------------------------------------------------
   !> Returns the size of the exception list. If the list is not allocated, 0 
   !! is returned. 
   !!
   !! @return the exception's size
   !---------------------------------------------------------------------------
   integer function numExceptions() result(sz)

      if (allocated(exceptionStack)) then
         sz = size(exceptionStack)
      else
         sz = 0
      end if

   end function numExceptions

   !---------------------------------------------------------------------------
   !> Catch (and delete) exception from exception list.  User should 
   !! not assume any implied order for which exception is accessed.
   !!
   !! @param preserve - optional preserve
   !!
   !! @return .true. if list is not empty, .false. otherwise.
   !---------------------------------------------------------------------------
   logical function catchGeneric(preserve)
      implicit none

      logical, optional, intent(in) :: preserve

      logical :: preserve_

      if (.not. isEmpty()) then
         catchGeneric = .true.
         preserve_ = .false.
         if (present(preserve)) preserve_ = preserve
         if (.not. preserve_) call deleteIthEntry(numExceptions())
      else
         catchGeneric = .false.
      end if

   end function catchGeneric

   !---------------------------------------------------------------------------
   !> Returns if exception is empty.
   !!
   !! @returns .true. if exception is empty, .false. otherwise.
   !---------------------------------------------------------------------------
   logical function isEmpty()
      isEmpty = (numExceptions() == 0)
   end function isEmpty

   !---------------------------------------------------------------------------
   !> Catch a specific exception from the list.
   !!
   !! @param this - this exception object
   !! @param rethrow - flag indicator for rethrow as optional
   !!
   !! @returns .true. if exception is found in the global list,
   !! .false. otherwise.  The optional parameter "rethrow" can be used to specify
   !! that the exceptions should remain in the list for later handling.
   !---------------------------------------------------------------------------
   logical function catchSpecific(this, rethrow)
      implicit none
      type (Exception_type), intent(in) :: this
      logical, optional, intent(in) :: rethrow

      catchSpecific = isMember(this, keep = rethrow)

   end function catchSpecific

   !---------------------------------------------------------------------------
   !> Catch a message exception from the list.
   !!
   !! @param message - specific message for catching specific type of exception
   !!
   !! @returns .true. if type of exception is message, otherwise .false.
   !---------------------------------------------------------------------------
   logical function catchMessage(message)
      implicit none
      character(len=*) :: message

      catchMessage = isMemberMsg(message)

   end function catchMessage

   !---------------------------------------------------------------------------
   !> Determine if two exceptions are the same.
   !!
   !! @param a - expected exception to be equal
   !! @param b - found exception to be equal
   !!
   !! @return .true. if they are equal.   Otherwise, .false.
   !---------------------------------------------------------------------------
   logical function equals(a, b)
      implicit none
      type (Exception_type), intent(in) :: a
      type (Exception_type), intent(in) :: b


      equals = (a%message == b%message) .and. (a%uniqueID == b%uniqueID)

   end function equals

   !---------------------------------------------------------------------------
   !> Internal procedure to increase the size of the list by 1.
   !! Currently allocatable arrays are used, but pointers would
   !! probably be more efficient.
   !---------------------------------------------------------------------------
   subroutine extendExceptionStack()
      integer :: sz
      type (Exception_type), allocatable :: tmpStack(:)

      sz = numExceptions()
      if (sz > 0) then
         allocate(tmpStack(sz))
         tmpStack = exceptionStack
         deallocate(exceptionStack)
      end if

      allocate(exceptionStack(sz+1))
      if (sz > 0) then
         exceptionStack(1:sz) = tmpStack
         deallocate(tmpStack)
      end if

   end subroutine extendExceptionStack

   !---------------------------------------------------------------------------
   !> Helper function to insert an exception at the end of the list.
   !!
   !! @param this - this exception object
   !---------------------------------------------------------------------------
   subroutine put(this)
      implicit none
      type (Exception_type), intent(in) :: this

      exceptionStack(size(exceptionStack)) = this

   end subroutine put

   !---------------------------------------------------------------------------
   !> Returns true if global private list contains the specified exception.
   !! Default behavior is to delete the exception, but this can be overridden with
   !! the optional "keep" argument.
   !!
   !! @param anException - exception object
   !! @param keep - optional keep
   !!
   !! @return .true. if an exception is a member, .false. otherwise.
   !---------------------------------------------------------------------------
   logical function isMember(anException, keep)
      implicit none
      type (Exception_type), intent(in) :: anException
      logical, optional, intent(in) :: keep
      integer :: i
      integer :: iStart
      integer :: n
      logical :: keep_

      keep_ = .false.
      if (present(keep)) keep_ = keep

      iStart = topIndex(tryStack)

      do i = iStart, numExceptions()
         if (exceptionStack(i) == anException) then
            isMember = .true.
            if (.not. keep_) call deleteIthEntry(i)
            return
         end if
      end do

      isMember = .false.

   end function isMember

   !---------------------------------------------------------------------------
   !> Returns true if global private list contains the specified message.
   !!
   !! @param message - given messge
   !!
   !! @return .true. if a message is member, .false. otherwise.
   !---------------------------------------------------------------------------
   logical function isMemberMsg(message)
      implicit none
      character(len=*) :: message
      integer :: i
      integer :: iStart
      integer :: n
      logical :: keep_

      keep_ = .false.
      iStart = topIndex(tryStack)

      do i = iStart, numExceptions()
         if (trim(getMessage(exceptionStack(i))) == trim(message)) then
            isMemberMsg = .true.
            call deleteIthEntry(i)
            return
         end if
      end do

      isMemberMsg = .false.

   end function isMemberMsg

   !---------------------------------------------------------------------------
   !> Returns the top index of the exception by the given allocated stack in 
   !! array. 
   !!
   !! @param stack - given allocated stack
   !!
   !! @return the top index of stack
   !---------------------------------------------------------------------------
   integer function topIndex(stack)
      integer, intent(in), allocatable :: stack(:)

      topIndex = 1 ! unless

      if (allocated(stack)) then
         if (size(stack) > 0) topIndex = stack(1)
      end if

   end function topIndex

   !---------------------------------------------------------------------------
   !> Clear all exceptions from the list.
   !---------------------------------------------------------------------------
   subroutine clearAll()
      implicit none

      if (allocated(exceptionStack)) deallocate(exceptionStack)
      if (allocated(tryStack)) deallocate(tryStack)

   end subroutine clearAll

   !---------------------------------------------------------------------------
   !> Remove the ith element in the exception list.
   !---------------------------------------------------------------------------
   subroutine deleteIthEntry(i)
      implicit none
      integer, intent(in) :: i
      type (Exception_type), allocatable :: tmpStack(:)
      integer :: n
      
      n = size(exceptionStack)
      if (n > 1) then
         allocate(tmpStack(n-1))
         tmpStack(1:i-1) = exceptionStack(1:i-1)
         tmpStack(i:) = exceptionStack(i+1:)
      end if

      deallocate(exceptionStack)
      if (n > 1) then
         allocate(exceptionStack(n-1))
         exceptionStack = tmpStack
         deallocate(tmpStack)
      end if

   end subroutine deleteIthEntry

   !---------------------------------------------------------------------------
   !> Introduce a new level of nesting for the management of throw/catch.
   !! Weakly analagous to "try" from other languages.  (Push the stack.)
   !---------------------------------------------------------------------------
   subroutine try()
      implicit none

      call pushIndex(tryStack, numExceptions()+1)

   end subroutine try

   !---------------------------------------------------------------------------
   !> Leave current "try" block.  (Pop the stack.)
   !---------------------------------------------------------------------------
   subroutine endTry()
      implicit none

      call popIndex(tryStack)

   end subroutine endTry

   !---------------------------------------------------------------------------
   !> Helper procedure to encode information about where the current try block
   !! corresponds to the list of exceptions.
   !!
   !! @param stack - given allocated stack
   !! @param n - given number for pushing it into the top of stack 
   !---------------------------------------------------------------------------
   subroutine pushIndex(stack, n)
      implicit none
      integer, allocatable :: stack(:)
      integer, intent(in) :: n

      integer, allocatable :: tmpStack(:)
      integer :: stackSize

      if (.not. allocated(stack)) then
         stackSize = 0
      else
         stackSize = size(stack)
      end if

      if (stackSize > 0) then
         allocate(tmpStack(stackSize))
         tmpStack = stack

         deallocate(stack)
         allocate(stack(stackSize+1))

         stack(2:stackSize+1) = tmpStack
         deallocate(tmpStack)
      else
         allocate(stack(1))
      end if

      stack(1) = n

   end subroutine pushIndex

   !---------------------------------------------------------------------------
   !> Helper procedure to pop entry from the "try" stack.
   !!
   !! @param stack - given allocated stack
   !---------------------------------------------------------------------------
   subroutine popIndex(stack)
      implicit none
      integer, allocatable :: stack(:)

      integer, allocatable :: tmpStack(:)
      integer :: stackSize

      stackSize = size(stack)
      allocate(tmpStack(stackSize))
      tmpStack = stack
      deallocate(stack)
      if (stackSize > 1) then
         allocate(stack(stackSize-1))
         stack(1:stackSize-1) = tmpStack(2:)
      end if
      deallocate(tmpStack)

   end subroutine popIndex

   !---------------------------------------------------------------------------
   !> Returns the message contents of the specified exception.
   !!
   !! @param this - this exception object
   !!
   !! @return message of the specific exception
   !---------------------------------------------------------------------------
   function getMessage(this) result(message)
      type (Exception_type), intent(in) :: this
      character(len=MAXLEN) :: message

      message = trim(this%message)

   end function getMessage

#ifdef USE_MPI
   !---------------------------------------------------------------------------
   !> Gathers exceptions with given MPI communicator. 
   !!
   !! @param comm - MPI communicator
   !---------------------------------------------------------------------------
   subroutine gatherExceptions(comm)
      use MpiServices_mod
      integer, intent(in) :: comm ! mpi communicator

      include 'mpif.h'
      integer :: status(MPI_STATUS_SIZE)
      integer :: rank, npes ,ier
      integer :: p, i
      integer, allocatable :: exceptionCounts(:)
      type (Exception_type) :: remoteException
      character(len=MAXLEN) :: message
      character(len=MAXLEN) :: procMessage
      integer, parameter :: TAG = 100
      integer :: initialNumExceptions


      npes = numProcesses(comm)
      rank = getRank(comm)

      allocate(exceptionCounts(0:npes-1))
      initialNumExceptions = numExceptions()
      call mpi_gather(initialNumExceptions, 1, MPI_INTEGER, exceptionCounts, 1, MPI_INTEGER, 0, comm, ier)

      if (rank == 0) then
         ! local messages must be prepended with process id tag
         p = 0
         do i = 1, exceptionCounts(p)
            write(exceptionStack(i) % message,'("(pe:",i5.1,") ",a)') p,trim(exceptionStack(i) % message)
         end do
         do p = 1, npes - 1
            do i = 1, exceptionCounts(p)
               call mpi_recv( message(1:1), MAXLEN, MPI_CHARACTER, p, TAG, comm, status, ier)
               write(procMessage,'("(pe:",i5.1,") ",a)') p,trim(message)
               call throw(Exception(trim(procMessage)))
            end do
         end do
      else ! not root
         do i = 1, initialNumExceptions
            remoteException = catchAny()
            message = getMessage(remoteException)
            call mpi_send(message(1:1), MAXLEN, MPI_CHARACTER, 0, TAG, comm, ier)
         end do
      end if
      deallocate(exceptionCounts)
      
   end subroutine gatherExceptions
#endif

end module PFUnitException_mod
