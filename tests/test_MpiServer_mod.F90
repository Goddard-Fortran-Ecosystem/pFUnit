module test_MpiServer_mod
   use pFUnit
   use MpiTestCase_mod
   use MpiServer_mod
   implicit none
   private

   public :: test_amMpiServer
   public :: test_getNextCommand

contains

   subroutine test_amMpiServer()
      ! Normally this test will be run from the client executable, 
      ! so we expect not to match 'mpi'
      call assertFalse(amMpiServer())
      
   end subroutine test_amMpiServer

   subroutine test_getNextCommand()
      use MpiCommand_mod
      use IO_Utilities_mod
      type (MpiServer_type) :: server
      type (MpiCommand_type) :: command1
      type (MpiCommand_type) :: command2
      integer :: unit

      unit = openFile(fileName='inFile', form='formatted', status='new')
      command1 = MpiCommand(numProcs=4)
      call writeCommand(command1, unit)
      close(unit)

      server = MpiServer('inFile', 'outFile')
      call readCommand(server, command2)
      call assertEqual(4, numProcs(command2))

      call clean(server)

      call deleteFile('inFile')
      call deleteFile('outFile')
      
   end subroutine test_getNextCommand

end module test_MpiServer_mod
