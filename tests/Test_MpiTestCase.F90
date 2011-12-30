#include "reflection.h"
module Test_MpiTestCase_mod
   use MpiTestCase_mod, only: MpiTestCase
   implicit none
   private

   public :: suite
   public :: newTest_MpiTestCase
   public :: Test_MpiTestCase

   type, extends(MpiTestCase) :: Test_MpiTestCase
      character(len=20), public :: runLog
      procedure(I_method), pointer :: testMethod => null()
   contains
      procedure :: runTestMethod
   end type Test_MpiTestCase

   abstract interface
      subroutine I_method(this)
        use Test_mod
        import Test_MpiTestCase
        class (Test_MpiTestCase), intent(inOut) :: this
      end subroutine I_method
   end interface

contains

   function suite()
     use TestSuite_mod, only: TestSuite, newTestSuite
      type (TestSuite), pointer :: suite

      suite => newTestSuite('Test_MpiTestCase')
      call suite%addTest(newTest_MpiTestCase(REFLECT(testWasRun), numProcesses=1))
      call suite%addTest(newTest_MpiTestCase(REFLECT(testRunOn2Processors), numProcesses=2))
      call suite%addTest(newTest_MpiTestCase(REFLECT(testFailOn1), numProcesses=3))
      call suite%addTest(newTest_MpiTestCase(REFLECT(testFailOn2), numProcesses=3))
      
   end function suite

   function newTest_MpiTestCase(method, name, numProcesses) result(this)
      type(Test_MpiTestCase), pointer :: this
      procedure(I_method) :: method
      character(len=*), intent(in) :: name
      integer, intent(in) :: numProcesses

      allocate(this)
      this%testMethod => method
      this%numProcesses = numProcesses
      call this%setName(name)

    end function newTest_MpiTestCase

   subroutine testWasRun(this)
      use f2kunit, only: assertEqual
      class (Test_MpiTestCase) :: this

      this%runLog = ' ' ! empty
      call wasRun(this%runLog, this%getMpiCommunicator())
      call assertEqual('was run', this%runLog)

   end subroutine testWasRun

   subroutine testRunOn2Processors(this)
      use f2kunit, only: assertEqual
      class (Test_MpiTestCase) :: this

      integer :: numProcesses, ier
      call Mpi_Comm_Size(this%getMpiCommunicator(), numProcesses, ier)
      call assertEqual(2, numProcesses)

   end subroutine testRunOn2Processors

   subroutine failOn1(this)
      use f2kunit
      class (Test_MpiTestCase) :: this
      if (this%context%processRank() == 1) then
         call throw('Intentional fail on process 1.')
      end if
   end subroutine failOn1

   subroutine failOn2(this)
      use f2kunit
      class (Test_MpiTestCase) :: this
      if (this%context%processRank() == 1 .or. this%context%processRank() == 2) then
         call throw('Intentional fail')
      end if
   end subroutine failOn2

   ! Test that exception thrown on non root process is
   ! detected on root process in the end.
   subroutine testFailOn1(this)
      use f2kunit, only: throw
      use f2kunit, only: assertEqual
      use TestResult_mod
      use Exception_mod, only: catch
      use Exception_mod, only: MAXLEN_MESSAGE
      use TestFailure_mod
      class (Test_MpiTestCase) :: this

      integer :: numProcesses, ier
      type (Test_MpiTestCase) :: failTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure

      reslt = newTestResult()
      failTest = newTest_MpiTestCase(REFLECT(failOn1), numProcesses = 3)
      call failTest%run(reslt, this%context)

      if (this%context%isRootProcess()) then
         call assertEqual(1, reslt%failureCount())
         if (1 == reslt%failureCount()) then
            failure = reslt%getIthFailure(1)
            call assertEqual('failOn1', failure%testName)
            call assertEqual('Intentional fail on process 1. (process 1 of 3)', &
                 & failure%exception%getMessage())
         end if
      end if

   end subroutine testFailOn1

   ! Test that exception thrown on non root process is
   ! detected on root process in the end.
   subroutine testFailOn2(this)
      use f2kunit, only: throw
      use f2kunit, only: assertEqual
      use TestResult_mod
      use Exception_mod, only: catch
      use Exception_mod, only: MAXLEN_MESSAGE
      use TestFailure_mod
      class (Test_MpiTestCase) :: this

      integer :: numProcesses, ier
      type (Test_MpiTestCase) :: failTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure

      reslt = newTestResult()
      failTest = newTest_MpiTestCase(REFLECT(failOn2), numProcesses = 3)
      call failTest%run(reslt, this%context)

      if (this%context%isRootProcess()) then
         call assertEqual(2, reslt%failureCount())
         if (reslt%failureCount() > 0) then

            failure = reslt%getIthFailure(1)
            call assertEqual('failOn2', failure%testName)
            call assertEqual('Intentional fail (process 1 of 3)', &
                 & failure%exception%getMessage())

            failure = reslt%getIthFailure(2)
            call assertEqual('failOn2', failure%testName)
            call assertEqual('Intentional fail (process 2 of 3)', &
                 & failure%exception%getMessage())
         end if
      end if

   end subroutine testFailOn2

   recursive subroutine runTestMethod(this)
      class(Test_MpiTestCase), intent(inOut) :: this
      call this%testMethod()
   end subroutine runTestMethod

   subroutine wasRun(runLog, mpiCommunicator)
      character(len=*), intent(inout) :: runLog
      integer, intent(in) :: mpiCommunicator
      
      integer :: numProcesses, rank, ier

      runLog = 'was run'
      call Mpi_Barrier(mpiCommunicator, ier)

   end subroutine wasRun

   subroutine delete_(this)
      type (Test_MpiTestCase), intent(inOut) :: this
!!$      nullify(this%testMethod)
   end subroutine delete_

end module Test_MpiTestCase_mod

