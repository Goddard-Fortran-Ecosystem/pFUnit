!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: MpiClient 
!
!> @brief
!! Implements the MpiClient object for communicating with MPI server.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 16 Oct 2006
!!
! REVISION HISTORY:
! 05 Apr 2007 - Initial Version
! 13 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module MpiClient_mod
   implicit none
   private

   public :: MpiClient_type
   public :: MpiClient
   public :: launchServer
   public :: getUnit
   public :: getLine
   public :: putLine
   public :: clean
   public :: terminateServer

   public :: isResponding

   integer, parameter :: MAXLEN_COMMAND = 80
   type MpiClient_type
      private
      integer :: myID
      integer :: unitToServer
      integer :: unitFromServer
      logical :: alive = .false.
      character(len=MAXLEN_COMMAND) :: server
   end type MpiClient_type

   interface clean
      module procedure clean_
   end interface

   interface getLine
      module procedure getLine_client
   end interface
   
   interface getUnit
      module procedure getUnit_
   end interface

   integer, save :: globalID = 0

contains

   !---------------------------------------------------------------------------
   !> Constructs a new MpiClent object.  If the server is present, it creates
   !! a new MpiClient object with the specific server name.
   !!
   !! @param server - optional server name
   !!
   !! @return new MPI client object
   !---------------------------------------------------------------------------
   function MpiClient(server) result(this)
      character(len=*), optional, intent(in) :: server
      type (MpiClient_type) :: this

      globalID = globalID + 1
      this % myId = globalID

      if (present(server)) then
         this % server = trim(server)
      else
         call get_environment_variable('NEW_PFUNIT_MPI_SERVER', this % server)
      end if

   end function MpiClient

   !---------------------------------------------------------------------------
   !> Launches the server with the specific maximum number of processes.
   !!
   !! @param this - this MPI client object 
   !! @param maxProcesses - maximum number of processes
   !---------------------------------------------------------------------------
   subroutine launchServer(this, maxProcesses)
      use IO_Utilities_mod
      type (MpiClient_type) :: this
      integer, intent(in) :: maxProcesses
      character(len=MAXLEN_COMMAND) :: command
      character(len=MAXLEN_COMMAND) :: check

      call createNamedPipe('toServer')
      call createNamedPipe('fromServer')

      command = launchCommand(this % server, maxProcesses, 'toServer', 'fromServer')
      this % unitToServer = openFile('toServer', form='formatted', status = 'old')
      this % unitFromServer = openFile('fromServer', form='formatted', status = 'old')

      call system(trim(command) // '&' ) ! run in background

      check = getLine(this)
      this % alive = ('ping' == trim(check))

   end subroutine launchServer

   !---------------------------------------------------------------------------
   !> Gets the line used in string from the MPI server. 
   !!
   !! @param this - this MPI client object 
   !!
   !! @return buffer from the server
   !---------------------------------------------------------------------------
   function getLine_client(this) result(buffer)
      type (MpiClient_type) :: this
      character(len=80) :: buffer

      read(this % unitFromServer,'(a80)') buffer

   end function getLine_client

   !---------------------------------------------------------------------------
   !> Sends the buffer to the MPI server 
   !!
   !! @param this - this MPI client object 
   !! @param buffer - given buffer
   !---------------------------------------------------------------------------
   subroutine putLine(this, buffer)
      type (MpiClient_type) :: this
      character(len=*),intent(in) :: buffer

      write(this % unitToServer,*) trim(buffer)

   end subroutine putLine

   !---------------------------------------------------------------------------
   !> Executes the MPI command to launch for communicating within MPI servers.
   !!
   !! @param server - server name
   !! @param maxProcesses - maximum number of processes
   !! @param toServer - server name for sending to
   !! @param fromServer - server name for receiving from
   !!
   !! @return buffer of the command
   !---------------------------------------------------------------------------
   function launchCommand(server, maxProcesses, toServer, fromServer) result(command)
      character(len=*), intent(in) :: server
      integer,          intent(in) :: maxProcesses
      character(len=*), intent(in) :: toServer
      character(len=*), intent(in) :: fromServer
      character(len=MAXLEN_COMMAND) :: command

      write(command,'("mpirun -np ",i8," ",a," < ",a," > ",a)') maxProcesses, &
           trim(server), trim(toServer), trim(fromServer)

   end function launchCommand

   !---------------------------------------------------------------------------
   !> Checks if the connection is responding.
   !!
   !! @param this - this MPI client object
   !!
   !! @return .true. if responded.  Otherwise, it is not responding.
   !---------------------------------------------------------------------------
   logical function isResponding(this)
      type (MpiClient_type) :: this
      isResponding = this % alive
   end function isResponding

   !---------------------------------------------------------------------------
   !> Clears up the memory of the MPI client object.
   !!
   !! @param this - this MPI client object
   !---------------------------------------------------------------------------
   subroutine clean_(this)
      type (MpiClient_type) :: this
      close(this % unitToServer, status='delete')
      close(this % unitFromServer, status='delete')
   end subroutine clean_

   !---------------------------------------------------------------------------
   !> Gets the unit number with the specific selection of the server. 
   !!
   !! @param this - this MPI client object
   !! @param selection - given selection between 'toServer' and 'fromServer'
   !!
   !! @return unit number based on the selection of the server (from/to)
   !---------------------------------------------------------------------------
   integer function getUnit_(this, selection) result(unit)
      type (MpiClient_type) :: this
      character(len=*), intent(in) :: selection
      select case (trim(selection))
      case ('toServer')
         unit = this % unitToServer
      case ('fromServer')
         unit = this % unitFromServer
      end select
   end function getUnit_

   !---------------------------------------------------------------------------
   !> Terminates the connection of the server by requesting.
   !!
   !! @param this - this MPI client object
   !---------------------------------------------------------------------------
   subroutine terminateServer(this)
      type (MpiClient_type) :: this
      call putLine(this,'terminate')
   end subroutine terminateServer

end module MpiClient_mod
