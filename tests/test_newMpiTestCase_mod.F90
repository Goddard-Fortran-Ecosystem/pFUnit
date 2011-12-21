module test_newMpiTestCase_mod
   use pFunit
   use newMpiTestCase_mod
   implicit none
   private

   public :: test_readWrite
   public :: test_runOnServer

#include "reflection.h"

   character(len=*), parameter :: message = 'wasCalled'

contains

   subroutine test_readWrite()
      use TestMethodInfo_mod
      use IO_Utilities_mod
      use Test_MPI_WasRun_mod
      external :: mpiTestMethod
      integer :: unit
      type (newMpiTestCase_type) :: testCaseA
      type (newMpiTestCase_type) :: testCaseB
      character(len=len(message)) :: msg

      testCaseA = newMpiTestCase(REFLECT(mpiTestMethod), numProcs = 2)
      unit = openFile(fileName='inFile', form='formatted', status='new')
      call writeToFile(testCaseA, unit)
      close(unit)

      unit = openFile(fileName='inFile', form='formatted', status='old')
      call readFromFile(testCaseB, unit)
      close(unit)

      call assertEqual(2, numProcs(testCaseB))
      call assertEqual('mpiTestMethod', name(testCaseB))
      call run(testCaseB)
      
      unit =openFile('mpiTestMethod.dat', form='formatted', status='old')
      read(unit,*) msg
      close(unit,status='delete')
      call assertEqual(message, msg)

      call deleteFile('infile')
      
   end subroutine test_readWrite

   subroutine test_runOnServer()
      type (newMpiTestCase_type) :: test
      type (TestResult_type) :: result
      type (MpiClient_type) :: client
      external :: mpiRunOne
      external :: mpiRunOneFail

      client = MpiClient()
      call launchServer(client, maxProcesses = 1)

      test = newMpiTestCase(REFLECT(mpiRunOne), numProcs=1)
      result = newTestResult()

      call run(test, result, client)

      call assertEqual(1, numRun(result))
      call assertEqual(0, numFailed(result))
      call clean(result)
      call clean(test)

      test = newMpiTestCase(REFLECT(mpiRunOneFail), numProcs=1)
      result = newTestResult()

      call run(test, result, client)

      call assertEqual(1, numRun(result))
      call assertEqual(1, numFailed(result))
      call clean(result)
      call clean(test)

      call terminateServer(client)
      call clean(client)

   end subroutine test_runOnServer

end module test_newMpiTestCase_mod

subroutine mpiTestMethod(info)
   use TestMethodInfo_mod
   use IO_Utilities_mod
   implicit none
   type (TestMethodInfo_type), intent(inOut) :: info
   integer :: unit

   unit =openFile('mpiTestMethod.dat', form='formatted', status='new')
   write(unit,*) 'wasCalled'
   close(unit)

end subroutine mpiTestMethod

subroutine mpiRunOne(info)
   use TestMethodInfo_mod
   use pfunit
   implicit none
   type (TestMethodInfo_type), intent(inOut) :: info
   integer :: npes, ier

   call mpi_comm_size(getComm(info), npes, ier)
   call assertEqual(1, npes)

end subroutine mpiRunOne

subroutine mpiRunOneFail(info)
   use TestMethodInfo_mod
   use pfunit
   implicit none
   type (TestMethodInfo_type), intent(inOut) :: info
   integer :: npes, ier

   call mpi_comm_size(getComm(info), npes, ier)
   call assertEqual(2, npes)

end subroutine mpiRunOneFail
