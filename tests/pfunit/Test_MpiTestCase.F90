!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_MpiTestCase
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 21 Mar 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 21 Mar 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module Test_MpiTestCase
   use mpi
   use PF_Test
   use PF_TestCase
   use PF_MpiTestCase
   use PF_MpiTestParameter
   implicit none
   private

   public :: suite
   public :: newTestMpiTestCase
   public :: TestMpiTestCase

   type, extends(MpiTestCase) :: TestMpiTestCase
      character(len=:), allocatable, public :: runLog
      procedure(method), pointer :: testMethod => null()
   contains
      procedure :: runMethod
   end type TestMpiTestCase

   abstract interface
      subroutine method(this)
        import TestMpiTestCase
        class (TestMpiTestCase), intent(inout) :: this
      end subroutine method
   end interface
   
contains

   function suite()
     use PF_TestSuite, only: TestSuite
      type (TestSuite) :: suite

      suite = TestSuite('TestMpiTestCase')

      call suite%addTest(newTestMpiTestCase('testWasRun', &
           &                                  testWasRun, numProcesses=1))
      call suite%addTest(newTestMpiTestCase('testRunOn2Processors', &
           &                                  testRunOn2Processors, numProcesses=2))
      call suite%addTest(newTestMpiTestCase('testFailOn1', &
           &                                  testFailOn1, numProcesses=3))
      call suite%addTest(newTestMpiTestCase('testFailOn2', &
           &                                  testFailOn2, numProcesses=3))
      call suite%addTest(newTestMpiTestCase('testTooFewProcs', &
           &                                  testTooFewProcs, numProcesses=4))

!      call suite%addTest(newTestMpiTestCase(REFLECT(testWasRun), numProcesses=1))
!      call suite%addTest(newTestMpiTestCase(REFLECT(testRunOn2Processors), numProcesses=2))
!      call suite%addTest(newTestMpiTestCase(REFLECT(testFailOn1), numProcesses=3))
!      call suite%addTest(newTestMpiTestCase(REFLECT(testFailOn2), numProcesses=3))
!      call suite%addTest(newTestMpiTestCase(REFLECT(testTooFewProcs), numProcesses=4))
      
   end function suite

   function newTestMpiTestCase(name, userMethod, numProcesses) result(this)
      type(TestMpiTestCase) :: this
      character(len=*), intent(in) :: name
      procedure(method) :: userMethod
      integer, intent(in) :: numProcesses


      call this%setName(name)
      this%testMethod => userMethod
      call this%setTestParameter(MpiTestParameter(numProcesses))

    end function newTestMpiTestCase

   subroutine testWasRun(this)
      use PF_Assert, only: assertEqual
      class (TestMpiTestCase), intent(inout) :: this

      this%runLog = ' ' ! empty
      call wasRun(this%runLog, this%getMpiCommunicator())
      call assertEqual('was run', this%runLog)

   end subroutine testWasRun

   subroutine testRunOn2Processors(this)
      use PF_Assert, only: assertEqual
      class (TestMpiTestCase), intent(inout) :: this

      integer :: numProcesses, ier
      call Mpi_Comm_Size(this%getMpiCommunicator(), numProcesses, ier)
      call assertEqual(2, numProcesses)

   end subroutine testRunOn2Processors

   subroutine brokenProcess1(this)
      use PF_ExceptionList
      class (TestMpiTestCase), intent(inout) :: this

      if (this%context%processRank() == 1) then
         call throw('Intentional fail on process 1.')
      end if

   end subroutine brokenProcess1

   subroutine brokenOnProcess2(this)
      use PF_ExceptionList
      class (TestMpiTestCase), intent(inout) :: this

      if (this%context%processRank() == 1 .or. this%context%processRank() == 2) then
         call throw('Intentional fail')
      end if

   end subroutine brokenOnProcess2

   ! Test that exception thrown on non root process is
   ! detected on root process in the end.
   subroutine testFailOn1(this)
      use PF_Assert, only: assertEqual
      use PF_TestResult
      use PF_Exception, only: Exception
      use PF_ExceptionList, only: throw
      use PF_TestFailure
      class (TestMpiTestCase), intent(inout) :: this

      type (TestMpiTestCase) :: brokenTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure
      type (Exception), pointer :: e


      reslt = TestResult()
      brokenTest = newTestMpiTestCase('brokenProcess1', brokenProcess1, numProcesses = 3)

      call brokenTest%run(reslt, this%context)

      if (this%context%isRootProcess()) then
         call assertEqual(1, reslt%failureCount())
         if (1 == reslt%failureCount()) then
            failure = reslt%getIthFailure(1)
            select case (this%getNumProcesses())
            case (1)
               call assertEqual('brokenProcess1[npes=1]', failure%testName)
            case (3)
               call assertEqual('brokenProcess1[npes=3]', failure%testName)
            case default
               call throw('this test only to be run for npes=1 or npes=3')
            end select

            e => failure%exceptions%at(1)
            call assertEqual('Intentional fail on process 1. (PE=1)', &
                 & e%getMessage())
         end if
      end if

   end subroutine testFailOn1

   ! Test that exception thrown on non root process is
   ! detected on root process in the end.
   subroutine testFailOn2(this)
      use PF_Assert, only: assertEqual
      use PF_TestResult
      use PF_Exception, only: Exception
      use PF_TestFailure
      class (TestMpiTestCase), intent(inout) :: this

      type (TestMpiTestCase) :: brokenTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure
      type (Exception), pointer :: e

      reslt = TestResult()
      brokenTest = newTestMpiTestCase('brokenOnProcess2', brokenOnProcess2, numProcesses = 3)
      call brokenTest%run(reslt, this%context)

      if (this%context%isRootProcess()) then
         call assertEqual(1, reslt%failureCount())
         if (reslt%failureCount() > 0) then

            failure = reslt%getIthFailure(1)
            call assertEqual(2, failure%exceptions%size())
            call assertEqual('brokenOnProcess2[npes=3]', failure%testName)
            e => failure%exceptions%at(1)
            call assertEqual('Intentional fail (PE=1)', &
                 & e%getMessage())

            e => failure%exceptions%at(2)
            call assertEqual('Intentional fail (PE=2)', &
                 & e%getMessage())
         end if
      end if

   end subroutine testFailOn2


   ! Purposefully request more processes than are available. 
   ! detected on root process in the end.
   subroutine testTooFewProcs(this)
      use PF_Assert, only: assertEqual
      use PF_TestResult
      use PF_Exception, only: Exception
      use PF_ExceptionList, only: anyExceptions
      use PF_TestFailure
      class (TestMpiTestCase), intent(inout) :: this

      type (TestMpiTestCase) :: brokenTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure
      integer, parameter :: TOO_MANY_PES = 5
      integer, parameter :: AVAILABLE_PES = 4
      type (Exception), pointer :: e

      character(len=:), allocatable :: expectedMessage

      reslt = TestResult()
      brokenTest = newTestMpiTestCase('brokenOnProcess2', brokenOnProcess2, numProcesses = TOO_MANY_PES)
      call brokenTest%run(reslt, this%context)

      if (this%context%isRootProcess()) then
         call assertEqual(1, reslt%failureCount())
         if (anyExceptions()) return

         failure = reslt%getIthFailure(1)

         call assertEqual('brokenOnProcess2[npes=5]', failure%testName)
         if (anyExceptions()) return
         expectedMessage = this%context%labelProcess("Insufficient processes to run this test.")
         e => failure%exceptions%at(1)
         call assertEqual(expectedMessage, e%getMessage())
         if (anyExceptions()) return

      end if

   end subroutine testTooFewProcs

   recursive subroutine runMethod(this)
      class(TestMpiTestCase), intent(inOut) :: this
      call this%testMethod()
   end subroutine runMethod

   subroutine wasRun(runLog, mpiCommunicator)
      character(len=:), allocatable, intent(inout) :: runLog
      integer, intent(in) :: mpiCommunicator
      
      integer :: ier

      runLog = 'was run'
      call Mpi_Barrier(mpiCommunicator, ier)

   end subroutine wasRun

   subroutine delete_(this)
      type (TestMpiTestCase), intent(inOut) :: this
      nullify(this%testMethod)
   end subroutine delete_

end module Test_MpiTestCase

