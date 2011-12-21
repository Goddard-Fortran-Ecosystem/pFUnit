!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: MpiServer_mod
!
!> @brief
!! Starts up the server using Message Passing Interface (MPI) mechanism.  
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 05 Apr 2007
!!
!! @todo - need more description above and some of subrountines/functions 
!!         below?
!!
! REVISION HISTORY:
! 05 Apr 2007 - Initial Version
! 13 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module MpiServer_mod
   use MpiServices_mod
   implicit none
   private

   public :: MpiServer_type
   public :: amMpiServer
   public :: runMpiServer
   public :: MpiServer
   public :: clean

   public :: terminate
   public :: beginPolling
   public :: getNextCommand
   public :: readCommand


   integer, parameter, public :: MAX_LEN_MESSAGE = 80
   integer, parameter, public :: MAX_LEN_NAME    = 40
   integer, parameter :: root = 0 

   character(len=*), parameter, public :: CMD_TERMINATE = 'terminate'
   character(len=*), parameter, public :: CMD_RUN_TEST  = 'run'

   character(len=*), parameter, public :: DONE            = 'MPI server terminated.'
   character(len=*), parameter, public :: UNKNOWN_COMMAND = 'unknown command'


   type MpiServer_type
      private
      integer :: unitFromClient
      integer :: unitToClient
   end type MpiServer_type

   interface readCommand
      module procedure readCommand_
   end interface

   interface clean
      module procedure clean_
   end interface

   integer, parameter :: STDIN = 5
   integer, parameter :: STDOUT = 6
   integer, parameter :: MAX_LEN_COMMAND = 80

contains

   !---------------------------------------------------------------------------
   !> Constructs an empty object of MPI server.   With the option of the input
   !! and/or output file name(s), an object of MPI server with the specific 
   !! unit is contructed.
   !!
   !! @param inFile - given optional file for input
   !! @param outFile - given optional file for output
   !!
   !! @return new object of MPI server
   !---------------------------------------------------------------------------
   function MpiServer(inFile, outFile) result(this)
      use Io_utilities_mod
      type (MpiServer_type) :: this
      character(len=*), optional, intent(in) :: inFile
      character(len=*), optional, intent(in) :: outFile

      if (present(inFile)) then
         this % unitFromClient = openFile(inFile, status = 'old', form='formatted')
      else
         this % unitFromClient = STDIN
      end if
      if (present(outFile)) then
         this % unitToClient = openFile(outFile, status = 'unknown', form='formatted')
      else
         this % unitToClient = STDOUT
      end if

   end function MpiServer

   !---------------------------------------------------------------------------
   !> Returns the status of getting the command for receiving the environment
   !! variable 'NEW_PFUNIT_MPI_SERVER'
   !!
   !! @return .true. if it is successful.  Otherwise, .false.
   !---------------------------------------------------------------------------
   logical function amMpiServer()
      integer, parameter :: MAXLEN_COMMAND = 20
      character(len=MAXLEN_COMMAND) :: command, mpiServer

      call get_command(command)
      call get_environment_variable('NEW_PFUNIT_MPI_SERVER', value = mpiServer)

      amMpiServer = (index(command, mpiServer) > 0)

   end function amMpiServer

   !---------------------------------------------------------------------------
   !> Begins to poll MPI until the terminated message.
   !!
   !! @param this - this object of MPI server 
   !---------------------------------------------------------------------------
   subroutine beginPolling(this)
      use newMpiTestCase_mod
      type (MpiServer_type), intent(inOut) :: this
      type (newMpiTestCase_type) :: test
      integer :: unit
      character(len=40) :: cmd

      do ! until terminate message
         cmd = trim(getNextCommand(this))
         select case (trim(cmd))
         case (' terminate')
            exit
         case (' mpiTest')
            call readFromFile(test, this % unitFromClient)
            call run(test)
            call return_MPI_result(this % unitToClient)
            call clean(test)
         end select
      end do

   end subroutine beginPolling

   !---------------------------------------------------------------------------
   !> Returns the MPI result for the report.
   !!
   !! @param unit - given unit for the MPI server 
   !---------------------------------------------------------------------------
   subroutine Return_MPI_Result(unit)
      use Report_mod
      use Params_mod
      use pFUnitException_mod
      character(len=MAX_LEN_MSG) :: msg
      integer :: unit
      character(len=MAX_LEN_MSG), allocatable :: globalExceptions(:)
      character(len=MAX_LEN_MSG), allocatable :: localExceptions(:)
      integer :: numExceptionsLocal, numExceptionsGlobal
      integer, allocatable :: exceptionsMap(:)
#ifdef USE_MPI
      include 'mpif.h'
      integer :: p
      integer :: dest, source, tag
      integer :: npes, ier
      integer :: status(MPI_STATUS_SIZE)
      integer :: i
      integer :: msgCount
#endif
      type (Report_type) :: rprt

      rprt = GenerateExceptionReport()
      numExceptionsLocal = numLines(rprt)

      allocate(localExceptions(numExceptionsLocal))
      call MPI_COMM_SIZE(MPI_COMM_WORLD, npes, ier)

      ! Gather results
      do msgCount = 1, numExceptionsLocal
         localExceptions(msgCount) = trim(getLine(rprt, msgCount))
      end do

#ifdef USE_MPI
      allocate(exceptionsMap(0:npes-1))
      call MPI_Gather(numExceptionsLocal, 1, MPI_INTEGER, exceptionsMap, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ier)
      numExceptionsGlobal = sum(exceptionsMap)
      if (amRoot(MPI_COMM_WORLD)) allocate(globalExceptions(numExceptionsGlobal))

      tag = 10 ! arbitrary
      ! MPI_Gather fails on many architectures for arrays of strings
      ! Implementing gather manually ...
      if (amRoot(MPI_COMM_WORLD)) then
         dest = 0
         do i = 1, numExceptionsLocal
            globalExceptions(i) = localExceptions(i)
         end do
         msgCount = numExceptionsLocal
         do p = 1, npes-1
            msgCount = msgCount + 1
            source = p
            call MPI_Recv(globalExceptions(msgCount)(1:1), MAX_LEN_MSG, MPI_CHARACTER, p, tag, MPI_COMM_WORLD, status, ier)
         end do
      else
         dest = 0
         do i = 1, numExceptionsLocal
            call MPI_Send(localExceptions(i)(1:1), MAX_LEN_MSG, MPI_CHARACTER, dest, tag, MPI_COMM_WORLD, ier)
         end do
      end if

#endif

      if (amRoot(MPI_COMM_WORLD)) then
         call write_msgs_to_file(globalExceptions, unit)
         deallocate(globalExceptions)
      end if

      deallocate(localExceptions)
      deallocate(exceptionsMap)

   contains

      !------------------------------------------------------------------------
      !> Writes the messages to the file with the given exception and unit 
      !! number.
      !!
      !! @param exceptions - given message for exception
      !! @param unit - given unit number 
      !------------------------------------------------------------------------
      subroutine write_msgs_to_file(exceptions, unit)
         use IO_Utilities_mod
         character(len=MAX_LEN_MSG) :: exceptions(:)
         integer, intent(in) :: unit

         integer :: p

         if (catch()) return
         write(unit,'("numMessages: ",i8.0)') size(exceptions)
         do p = 1, size(exceptions)
            call encode(exceptions(p))
            write(unit,'("process: ",i8.0,"  length: ",i8.0)') 0, len_trim(exceptions(p))
            write(unit,'(a)') trim(exceptions(p))
         end do
         close(unit)

      end subroutine write_msgs_to_file

   end subroutine Return_MPI_Result

   !---------------------------------------------------------------------------
   !> Terminates the object of the MPI server.
   !!
   !! @param this - this MPI server object
   !---------------------------------------------------------------------------
   subroutine terminate(this)
      type (MpiServer_type), intent(inOut) :: this
      write(this % unitToClient,'(a)') DONE
      if (STDIN /= this % unitFromClient) close(this % unitFromClient, status='delete')
      if (STDOUT /= this % unitToClient) close(this % unitToClient, status='delete')
   end subroutine terminate

   !---------------------------------------------------------------------------
   !> Gets the next command in string. 
   !!
   !! @param this - this MPI server object
   !!
   !! @return next command in string
   !---------------------------------------------------------------------------
   function getNextCommand(this) result(command)
      type (MpiServer_type), intent(inOut) :: this
      character(len=MAX_LEN_COMMAND) :: command
      read(this % unitFromClient,'(a)') command
   end function getNextCommand

   !---------------------------------------------------------------------------
   !> Reads the command via MPI.
   !!
   !! @param this - this MPI server object
   !! @param command - output of MPI command
   !---------------------------------------------------------------------------
   subroutine readCommand_(this, command)
      use MpiCommand_mod
      type (MpiServer_type), intent(in) :: this
      type (MpiCommand_type), intent(out) :: command

      call readCommand(command, this % unitFromClient)

   end subroutine readCommand_

   !---------------------------------------------------------------------------
   !> Clears up the memory of MPI server object.
   !!
   !! @param this - this MPI server object
   !---------------------------------------------------------------------------
   subroutine clean_(this)
      use IO_Utilities_mod
      type (MpiServer_type) :: this

      if (this % unitToClient /= STDOUT) then
         close(unit = this % unitToClient)
      end if

      if (this % unitFromClient /= STDIN) then
         close(unit = this % unitFromClient)
      end if

   end subroutine clean_

   !---------------------------------------------------------------------------
   !> Launches the MPI server using the MPI mechanism.
   !---------------------------------------------------------------------------
   subroutine runMpiServer()
      use IO_Utilities_mod
      include 'mpif.h'
      type (MpiServer_type) :: this
      integer :: ier

      call Mpi_Init(ier)
      print*,'ping' ! signal am alive
      this = MpiServer()
      call beginPolling(this)
      
      call terminate(this)
      call clean(this)
      
      call Mpi_Finalize(ier)
   end subroutine runMpiServer

   !---------------------------------------------------------------------------
   !> Encodes the string.
   !! 
   !! @param string - given string for encoding
   !---------------------------------------------------------------------------
   subroutine encode(string)
      character(len=*) :: string
      integer :: i

      do i = 1, len_trim(string)
         if (string(i:i) == '\n') then
            string(i:i) = '\\'
         end if
      end do
   end subroutine encode

   !---------------------------------------------------------------------------
   !> decodes the string.
   !! 
   !! @param string - given string for decoding
   !---------------------------------------------------------------------------
   subroutine unencode(string)
      character(len=*) :: string
      integer :: i

      do i = 1, len_trim(string)
         if (string(i:i) == '\\') then
            string(i:i) = '\n'
         end if
      end do
   end subroutine unencode

end module MpiServer_mod
