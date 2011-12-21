!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE:  newMpiTestCase_mod
!
!> @brief
!! Implements the object for test case using Message Passing Interface (MPI) for
!! handling the file for reading/writing and then running test cases.
!!
!! @author
!! Tom Clune, NASA/GSFC SIVO
!!
!! @date
!! 15 Dec 2007
!!
! REVISION HISTORY:
! 15 Dec 2007 - Initial Version
! 15 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module newMpiTestCase_mod
   use ProcedurePointer_mod
   implicit none
   private
   
   public :: newMpiTestCase_type
   public :: newMpiTestCase
   public :: clean

   public :: writeToFile
   public :: readFromFile
   public :: run

   public :: numProcs
   public :: name

   integer, parameter :: MAX_LEN_NAME = 40
   type newMpiTestCase_type
      private
      integer :: numProcs = -1
      type (ProcedurePointer_type) :: handle
      character(len=MAX_LEN_NAME)  :: name
   end type newMpiTestCase_type

   interface name
      module procedure getName
   end interface

   interface run
      module procedure run_method
      module procedure run_test
   end interface
   
   interface clean
      module procedure clean_
   end interface

contains

   !---------------------------------------------------------------------------
   !> Constructs the test case using MPI with the given name, external method,
   !! and number of processes.
   !!
   !! @param name - given name of test case
   !! @param method - given external method
   !! @param numProcesses - given number of processes
   !!
   !! @return new object of MPI test case
   !---------------------------------------------------------------------------
   function newMpiTestCase(name, method, numProcs) result(test)
      character(len=*), intent(in) :: name
      external :: method
!!$      type (ProcedurePointer_type), intent(in) :: method
      integer, intent(in) :: numProcs
      type (newMpiTestCase_type) :: test

      test % numProcs = numProcs
      test % name = trim(name)
      test % handle = ProcedurePointer(method)

   end function newMpiTestCase

   !---------------------------------------------------------------------------
   !> Writes message including the test case object to the file with the given
   !! unit number.
   !!
   !! @param this - this MPI test case object 
   !! @param unit - given unit number 
   !---------------------------------------------------------------------------
   subroutine writeToFile(this, unit)
      use ISO_C_BINDING
      type (newMpiTestCase_type), intent(in) :: this
      integer, intent(in) :: unit
      type (C_FUNPTR) :: addr

      write(unit,'("numProcs: ",i8.0)') this % numProcs
      write(unit,'("name:     ",a)') trim(this % name)
      write(unit,'("address size:  ",i10.0)') size(C_POINTER_TO_INTEGER(this % handle))
      write(unit,'("address:  ",i10.0)') C_POINTER_TO_INTEGER(this % handle)

   end subroutine writeToFile

   !---------------------------------------------------------------------------
   !> Reads the test case object from the file with the given unit number.
   !!
   !! @param this - this MPI test case object 
   !! @param unit - given unit number 
   !---------------------------------------------------------------------------
   subroutine readFromFile(this, unit)
      use ISO_C_BINDING
      use BaseAddress_mod, only: KIND_POINTER
      type (newMpiTestCase_type), intent(out) :: this
      integer, intent(in) :: unit
      type (C_FUNPTR) :: addr
      integer :: n
      integer (C_INTPTR_T),allocatable :: iAddr(:)

      read(unit,'("numProcs: ",i8.0)') this % numProcs
      read(unit,'("name:     ",a)') this % name
      read(unit,'("address size:  ",i10.0)') n
      allocate(iAddr(n))
      read(unit,'("address:  ",i10.0)') iaddr
      this % handle = ProcedurePointer(INTEGER_TO_C_POINTER(iaddr), this % name)
      deallocate(iAddr)

   end subroutine readFromFile

   !---------------------------------------------------------------------------
   !> Gets the number of processes from the MPI test case object.
   !!
   !! @param this - this MPI test case object 
   !!
   !! @return number of processes
   !---------------------------------------------------------------------------
   integer function numProcs(this)
      type (newMpiTestCase_type), intent(in) :: this
      numProcs = this % numProcs
   end function numProcs

   !---------------------------------------------------------------------------
   !> Gets the name of MPI test case object.
   !!
   !! @param this - this MPI test case object 
   !!
   !! @return name of this MPI test case object
   !---------------------------------------------------------------------------
   function getName(this) result(name)
      type (newMpiTestCase_type), intent(in) :: this
      character(len=MAX_LEN_NAME)  :: name
      name = this % name
   end function getName

   !---------------------------------------------------------------------------
   !> Performs to run the test method information.
   !!
   !! @param this - this MPI test case object 
   !---------------------------------------------------------------------------
   subroutine run_method(this) 
      use BaseAddress_mod
      use TestMethodInfo_mod
      external :: BaseAddress
      type (BaseAddress_type) :: BaseAddress
      type (newMpiTestCase_type), intent(in) :: this
      type (TestMethodInfo_type) :: info
      include 'mpif.h'
      info % comm = MPI_COMM_WORLD
      call invoke(this % handle, BaseAddress(info))

   end subroutine run_method

   !---------------------------------------------------------------------------
   !> Performs to run the test with the given result and MPI client.
   !!
   !! @param this - this MPI test case object 
   !! @param result - given test result object 
   !! @param client - given MPI client object 
   !---------------------------------------------------------------------------
   subroutine run_test(this, result, client)
      use TestResult_mod
      use MpiClient_mod
      use IO_Utilities_mod
      use pFUnitException_mod
      type (newMpiTestCase_type) :: this
      type (TestResult_type) :: result
      type (MpiClient_type) :: client
      integer :: unit
      integer, parameter :: MAX_LEN_MESSAGE = 100
      character(len=MAX_LEN_MESSAGE) :: message
      character(len=6) :: fmt
      integer :: ithMessage, numMessages

      call testStarted(result, this % name)
      call putLine(client, 'mpiTest')
      unit = getUnit(client, 'toServer')
      call writeToFile(this, unit)

      unit = getUnit(client, 'fromServer')
      read(unit,'("numMessages: ",i8.0)') numMessages
      do ithMessage = 1, numMessages
         call readMessage(unit, message)
         call testFailed(result, this % name, trim(message))
      end do

   contains

      !------------------------------------------------------------------------
      !> Reads the process and length from the input unit with given message.
      !!
      !! @param unit - given unit number
      !! @param message - given message
      !------------------------------------------------------------------------
      subroutine readMessage(unit, message)
         integer, intent(in) :: unit
         character(len=*), intent(out) :: message
         integer :: process
         integer :: length

         read(unit,'("process: ",i8.0,"  length: ",i8.0)') process, length
         write(fmt,'("(a",i3.0,")")') length
         read(unit,fmt) message(1:length)

      end subroutine readMessage
      
   end subroutine run_test

   !---------------------------------------------------------------------------
   !> Clears up the memory of the MPI test case object
   !!
   !! @param this - this MPI test case object 
   !---------------------------------------------------------------------------
   subroutine clean_(this)
      type (newMpiTestCase_type) :: this
   end subroutine clean_

   !---------------------------------------------------------------------------
   !> Converts C function pointer to array integer with the given procedure 
   !! pointer.
   !!
   !! @param funptr - given C function pointer for procedure pointer
   !!
   !! @return array integers 
   !---------------------------------------------------------------------------
   function C_POINTER_TO_INTEGER(funptr) result(arr)
     use ISO_C_BINDING
     type (ProcedurePointer_type) :: funptr
     integer(kind=C_INTPTR_T), allocatable :: arr(:)
     integer :: n

     n = size(transfer(address(funptr), arr))
     allocate(arr(n))
     arr = transfer(address(funptr),arr)
     
   end function C_POINTER_TO_INTEGER

   !---------------------------------------------------------------------------
   !> Converts array integer to C function pointer in procedure pointer.
   !!
   !! @param arr - given array integers 
   !!
   !! @return C function pointer
   !---------------------------------------------------------------------------
   function INTEGER_TO_C_POINTER(arr) result (funptr)
     use ISO_C_BINDING
     integer (C_INTPTR_T) :: arr(:)
     type (C_FUNPTR) :: funptr
     
     funptr = transfer(arr, funptr)

   end function INTEGER_TO_C_POINTER

end module newMpiTestCase_mod
