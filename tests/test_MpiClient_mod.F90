module test_MpiClient_mod
   use pFUnit
   use MpiTestCase_mod
   implicit none
   private

   public :: test_launchServer

contains

   subroutine test_launchServer()
      use IO_Utilities_mod
      type (MpiClient_type) :: client
      integer :: nSquared
      character(len=80) :: buffer

      client = MpiClient('tests/testData/square.x')
      call assertFalse(isResponding(client))
      call launchServer(client, maxProcesses = 3)

      call putLine(client,'3')
      buffer = getLine(client)
      read(buffer,*)nSquared

      call assertEqual(9, nSquared)

      call putLine(client,'0')
      call clean(client)

   end subroutine test_launchServer

end module test_MpiClient_mod
