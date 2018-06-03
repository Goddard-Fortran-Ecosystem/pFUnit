!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_MpiTestCase_mod
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
module Test_MpiTestCase_mod
   use mpi
   use PF_Test_mod
   use PF_TestCase_mod
   use PF_MpiTestCase_mod
   use PF_MpiTestParameter_mod
   implicit none
   private

   public :: suite
   public :: newTest_MpiTestCase
   public :: Test_MpiTestCase

   type, extends(MpiTestCase) :: Test_MpiTestCase
      character(len=:), allocatable, public :: runLog
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
     use PF_TestSuite_mod, only: TestSuite
      type (TestSuite) :: suite

      suite = TestSuite('Test_MpiTestCase')

      call suite%addTest(newTest_MpiTestCase('testWasRun', &
           &                                  testWasRun, numProcesses=1))
      call suite%addTest(newTest_MpiTestCase('testRunOn2Processors', &
           &                                  testRunOn2Processors, numProcesses=2))
      call suite%addTest(newTest_MpiTestCase('testFailOn1', &
           &                                  testFailOn1, numProcesses=3))
      call suite%addTest(newTest_MpiTestCase('testFailOn2', &
           &                                  testFailOn2, numProcesses=3))
      call suite%addTest(newTest_MpiTestCase('testTooFewProcs', &
           &                                  testTooFewProcs, numProcesses=4))

!      call suite%addTest(newTest_MpiTestCase(REFLECT(testWasRun), numProcesses=1))
!      call suite%addTest(newTest_MpiTestCase(REFLECT(testRunOn2Processors), numProcesses=2))
!      call suite%addTest(newTest_MpiTestCase(REFLECT(testFailOn1), numProcesses=3))
!      call suite%addTest(newTest_MpiTestCase(REFLECT(testFailOn2), numProcesses=3))
!      call suite%addTest(newTest_MpiTestCase(REFLECT(testTooFewProcs), numProcesses=4))
      
   end function suite

   function newTest_MpiTestCase(name, userMethod, numProcesses) result(this)
      type(Test_MpiTestCase) :: this
      character(len=*), intent(in) :: name
      procedure(method) :: userMethod
      integer, intent(in) :: numProcesses


      call this%setName(name)
      this%testMethod => userMethod
      call this%setTestParameter(MpiTestParameter(numProcesses))

    end function newTest_MpiTestCase

   subroutine testWasRun(this)
      use PF_Assert_mod, only: assertEqual
      class (Test_MpiTestCase), intent(inout) :: this

      this%runLog = ' ' ! empty
      call wasRun(this%runLog, this%getMpiCommunicator())
      call assertEqual('was run', this%runLog)

   end subroutine testWasRun

   subroutine testRunOn2Processors(this)
      use PF_Assert_mod, only: assertEqual
      class (Test_MpiTestCase), intent(inout) :: this

      integer :: numProcesses, ier
      call Mpi_Comm_Size(this%getMpiCommunicator(), numProcesses, ier)
      call assertEqual(2, numProcesses)

   end subroutine testRunOn2Processors

   subroutine brokenProcess1(this)
      use PF_ExceptionList_mod
      class (Test_MpiTestCase), intent(inout) :: this

      if (this%context%processRank() == 1) then
         call throw('Intentional fail on process 1.')
      end if

   end subroutine brokenProcess1

   subroutine brokenOnProcess2(this)
      use PF_ExceptionList_mod
      class (Test_MpiTestCase), intent(inout) :: this

      if (this%context%processRank() == 1 .or. this%context%processRank() == 2) then
         call throw('Intentional fail')
      end if

   end subroutine brokenOnProcess2

   ! Test that exception thrown on non root process is
   ! detected on root process in the end.
   subroutine testFailOn1(this)
      use PF_Assert_mod, only: assertEqual
      use PF_TestResult_mod
      use PF_Exception_mod, only: Exception
      use PF_ExceptionList_mod, only: throw
      use PF_ExceptionList_mod, only: catch
      use PF_TestFailure_mod
      class (Test_MpiTestCase), intent(inout) :: this

      integer :: numProcesses, ier
      type (Test_MpiTestCase) :: brokenTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure
      type (Exception), pointer :: e


      reslt = TestResult()
      brokenTest = newTest_MpiTestCase('brokenProcess1', brokenProcess1, numProcesses = 3)

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
      use PF_ExceptionList_mod, only: throw
      use PF_Assert_mod, only: assertEqual
      use PF_TestResult_mod
      use PF_Exception_mod, only: Exception
      use PF_ExceptionList_mod, only: catch
      use PF_TestFailure_mod
      class (Test_MpiTestCase), intent(inout) :: this

      integer :: numProcesses, ier
      type (Test_MpiTestCase) :: brokenTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure
      type (Exception), pointer :: e

      reslt = TestResult()
      brokenTest = newTest_MpiTestCase('brokenOnProcess2', brokenOnProcess2, numProcesses = 3)
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
      use PF_ExceptionList_mod, only: throw
      use PF_Assert_mod, only: assertEqual
      use PF_TestResult_mod
      use PF_Exception_mod, only: Exception
      use PF_ExceptionList_mod, only: catch
      use PF_ExceptionList_mod, only: anyExceptions
      use PF_TestFailure_mod
      class (Test_MpiTestCase), intent(inout) :: this

      integer :: numProcesses, ier
      type (Test_MpiTestCase) :: brokenTest
      type (TestResult) :: reslt
      type (TestFailure) :: failure
      integer, parameter :: TOO_MANY_PES = 5
      integer, parameter :: AVAILABLE_PES = 4
      type (Exception), pointer :: e

      character(len=:), allocatable :: expectedMessage

      reslt = TestResult()
      brokenTest = newTest_MpiTestCase('brokenOnProcess2', brokenOnProcess2, numProcesses = TOO_MANY_PES)
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
      class(Test_MpiTestCase), intent(inOut) :: this
      call this%testMethod()
   end subroutine runMethod

   subroutine wasRun(runLog, mpiCommunicator)
      character(len=:), allocatable, intent(inout) :: runLog
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

