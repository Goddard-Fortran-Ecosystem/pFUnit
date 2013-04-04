
#include "reflection.h"
module Test_MpiTestCase_mod
   use Test_mod
   use TestCase_mod
   use MpiTestCase_mod
   implicit none
   private

   public :: suite
   public :: newTest_MpiTestCase
   public :: Test_MpiTestCase

   type, extends(MpiTestCase) :: Test_MpiTestCase
      character(len=20), public :: runLog
      procedure(method), pointer :: testMethod => null()
   contains
      procedure :: runMethod
   end type Test_MpiTestCase

   abstract interface
      subroutine method(this)
        import Test_MpiTestCase
        class (Test_MpiTestCase), intent(inout) :: this
      end subroutine method
   end interface
   
contains

   function suite()
     use TestSuite_mod, only: TestSuite, newTestSuite
      type (TestSuite) :: suite

      suite = newTestSuite('Test_MpiTestCase')
      call suite%addTest(newTest_MpiTestCase(REFLECT(testWasRun), numProcesses=1))
      call suite%addTest(newTest_MpiTestCase(REFLECT(testRunOn2Processors), numProcesses=2))
#ifndef __INTEL_COMPILER
      call suite%addTest(newTest_MpiTestCase(REFLECT(testFailOn1), numProcesses=3))
      call suite%addTest(newTest_MpiTestCase(REFLECT(testFailOn2), numProcesses=3))
      call suite%addTest(newTest_MpiTestCase(REFLECT(testTooFewProcs), numProcesses=4))
#endif
      
   end function suite

   function newTest_MpiTestCase(name, userMethod, numProcesses) result(this)
      type(Test_MpiTestCase) :: this
      character(len=*), intent(in) :: name
      procedure(method) :: userMethod
      integer, intent(in) :: numProcesses

      this%testMethod => userMethod
      this%numProcessesRequested = numProcesses
      call this%setName(name)

    end function newTest_MpiTestCase

   subroutine testWasRun(this)
      use Assert_mod, only: assertEqual
      class (Test_MpiTestCase), intent(inout) :: this

      this%runLog = ' ' ! empty
      call wasRun(this%runLog, this%getMpiCommunicator())
      call assertEqual('was run', this%runLog)

   end subroutine testWasRun

   subroutine testRunOn2Processors(this)
      use Assert_mod, only: assertEqual
      class (Test_MpiTestCase), intent(inout) :: this

      integer :: numProcesses, ier
      call Mpi_Comm_Size(this%getMpiCommunicator(), numProcesses, ier)
      call assertEqual(2, numProcesses)

   end subroutine testRunOn2Processors

   subroutine brokenProcess1(this)
      use Exception_mod
      class (Test_MpiTestCase), intent(inout) :: this

      integer :: unit
      unit = 40 + this%context%processRank()

      if (this%context%processRank() == 1) then
         call throw('Intentional fail on process 1.')
      end if

   end subroutine brokenProcess1

   subroutine brokenOnProcess2(this)
      use Exception_mod
      class (Test_MpiTestCase), intent(inout) :: this
      if (this%context%processRank() == 1 .or. this%context%processRank() == 2) then
         call throw('Intentional fail')
      end if
   end subroutine brokenOnProcess2

   ! Test that exception thrown on non root process is
   ! detected on root process in the end.
   subroutine testFailOn1(this)
      use Assert_mod, only: assertEqual
      use TestResult_mod
      use Exception_mod, only: throw
      use Exception_mod, only: catch
      use Exception_mod, only: MAXLEN_MESSAGE
      use TestFailure_mod
      class (Test_MpiTestCase), intent(inout) :: this

      integer :: numProcesses, ier
      type (Test_MpiTestCase) :: brokenTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure

      reslt = newTestResult()
      brokenTest = newTest_MpiTestCase(REFLECT(brokenProcess1), numProcesses = 3)

      call brokenTest%run(reslt, this%context)

      if (this%context%isRootProcess()) then
         call assertEqual(1, reslt%failureCount())
         if (1 == reslt%failureCount()) then
            failure = reslt%getIthFailure(1)
            call assertEqual('brokenProcess1', failure%testName)
            call assertEqual('Intentional fail on process 1. (process 1 of 3)', &
                 & failure%exception%getMessage())
         end if
      end if

   end subroutine testFailOn1

   ! Test that exception thrown on non root process is
   ! detected on root process in the end.
   subroutine testFailOn2(this)
      use Exception_mod, only: throw
      use Assert_mod, only: assertEqual
      use TestResult_mod
      use Exception_mod, only: catch
      use Exception_mod, only: MAXLEN_MESSAGE
      use TestFailure_mod
      class (Test_MpiTestCase), intent(inout) :: this

      integer :: numProcesses, ier
      type (Test_MpiTestCase) :: brokenTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure

      reslt = newTestResult()
      brokenTest = newTest_MpiTestCase(REFLECT(brokenOnProcess2), numProcesses = 3)
      call brokenTest%run(reslt, this%context)

      if (this%context%isRootProcess()) then
         call assertEqual(2, reslt%failureCount())
         if (reslt%failureCount() > 0) then

            failure = reslt%getIthFailure(1)
            call assertEqual('brokenOnProcess2', failure%testName)
            call assertEqual('Intentional fail (process 1 of 3)', &
                 & failure%exception%getMessage())

            failure = reslt%getIthFailure(2)
            call assertEqual('brokenOnProcess2', failure%testName)
            call assertEqual('Intentional fail (process 2 of 3)', &
                 & failure%exception%getMessage())
         end if
      end if

   end subroutine testFailOn2


   ! Purposefully request more processes than are available. 
   ! detected on root process in the end.
   subroutine testTooFewProcs(this)
      use Exception_mod, only: throw
      use Assert_mod, only: assertEqual
      use TestResult_mod
      use Exception_mod, only: catch
      use Exception_mod, only: exceptionWasThrown
      use Exception_mod, only: MAXLEN_MESSAGE
      use TestFailure_mod
      class (Test_MpiTestCase), intent(inout) :: this

      integer :: numProcesses, ier
      type (Test_MpiTestCase) :: brokenTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure
      integer, parameter :: TOO_MANY_PES = 5
      integer, parameter :: AVAILABLE_PES = 4
      integer :: i, process
      character(len=100) :: expectedMessage
      character(len=20) :: suffix

      reslt = newTestResult()
      brokenTest = newTest_MpiTestCase(REFLECT(brokenOnProcess2), numProcesses = TOO_MANY_PES)
      call brokenTest%run(reslt, this%context)

      if (this%context%isRootProcess()) then
         call assertEqual(AVAILABLE_PES, reslt%failureCount())
         if (exceptionWasThrown()) return

         do i = 1, AVAILABLE_PES
            failure = reslt%getIthFailure(i)
            call assertEqual('brokenOnProcess2', failure%testName)
            if (exceptionWasThrown()) return
            expectedMessage = "Insufficient processes to run this test."
            process = i - 1 ! C numbering convention in MPI process rank
            write(suffix,'(" (process ",i0," of ",i0,")")') i-1, AVAILABLE_PES
            call assertEqual(trim(expectedMessage) // trim(suffix), failure%exception%getMessage())
            if (exceptionWasThrown()) return
         end do

      end if

   end subroutine testTooFewProcs

   recursive subroutine runMethod(this)
      class(Test_MpiTestCase), intent(inOut) :: this
      call this%testMethod()
   end subroutine runMethod

   subroutine wasRun(runLog, mpiCommunicator)
      character(len=*), intent(inout) :: runLog
      integer, intent(in) :: mpiCommunicator
      
      integer :: numProcesses, rank, ier

      runLog = 'was run'
      call Mpi_Barrier(mpiCommunicator, ier)

   end subroutine wasRun

   subroutine delete_(this)
      type (Test_MpiTestCase), intent(inOut) :: this
      nullify(this%testMethod)
   end subroutine delete_

end module Test_MpiTestCase_mod

