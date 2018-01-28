!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: FUnit
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module sFUnit_private
   use PF_SourceLocation_mod
   use PF_Exception_mod
   use PF_ExceptionVector_mod
   use PF_ExceptionList_mod
   use PF_Expectation_mod
   use PF_Test_mod
   use PF_TestSuite_mod
   use PF_TestCase_mod
   use PF_TestMethod_mod
   use PF_AbstractTestParameter_mod
   use PF_ParameterizedTestCase_mod
   use PF_TestResult_mod
   use PF_TestRunner_mod
   use PF_BaseTestRunner_mod
   use PF_SubsetRunner_mod

   use PF_TestListener_mod
   use PF_XmlPrinter_mod
   use PF_ResultPrinter_mod
   use PF_DebugListener_mod

   use pf_Arg_mod
   use pf_ArgParser_mod
   use pf_ArgVector_mod
   use pf_StringVector_mod
   use pf_StringUnlimitedMap_mod
   use pf_CommandLineArguments_mod

   use PF_RobustRunner_mod
   use PF_Assert_mod
   use PF_ParallelContext_mod
   use PF_SerialContext_mod
   implicit none
   private

   public :: SourceLocation
   public :: Test
   public :: TestSuite, newTestSuite
   public :: TestMethod, newTestMethod
   public :: TestResult
   public :: TestRunner, newTestRunner
   public :: BaseTestRunner
   public :: SubsetRunner

   public :: ListenerPointer
   public :: ResultPrinter
   public :: newResultPrinter
   public :: newXmlPrinter
   public :: DebugListener

   public :: RobustRunner
   public :: TestCase
   public :: AbstractTestParameter
   public :: ParameterizedTestCase
   public :: ParallelContext
   public :: SerialContext, newSerialContext

   public :: assertFail
   public :: assertTrue, assertFalse
   public :: assertEqual
   public :: assertAny
   public :: assertAll
   public :: assertNone
   public :: assertNotAll
   public :: assertLessThan, assertLessThanOrEqual
   public :: assertGreaterThan, assertGreaterThanOrEqual
   public :: assertRelativelyEqual
   public :: assertExceptionRaised
   public :: assertSameShape
   public :: assertIsNan
   public :: assertIsFinite

   public :: throw, catchNext, catch, anyExceptions

   public :: Expectation, Subject, Predicate
   public :: wasCalled, wasNotCalled, wasCalledOnce

   ! Optional arguments for assertEqual
   public :: WhitespaceOptions
   public :: IGNORE_ALL, TRIM_ALL, KEEP_ALL, IGNORE_DIFFERENCES


   public :: Arg
   public :: ArgParser, to_string, to_integer, to_real
   public :: ArgVector
   public :: StringVector
   public :: StringUnlimitedMap

   public :: LoadTests_interface

   abstract interface
      function LoadTests_interface() result(suite)
         import TestSuite
         type (TestSuite) :: suite
      end function LoadTests_interface
   end interface


end module sFUnit_private

module sFUnit
   ! use these, but do not re-export:
   use sFUnit_private
   use iso_fortran_env, only: OUTPUT_UNIT
   implicit none
   ! add these

   public :: initialize
   public :: run
   public :: finalize
   public :: LoadTests_interface
   public :: stub

   logical, parameter :: SUCCEEDED = .true.
   logical, parameter :: FAILED = .false.

contains

   subroutine initialize(extra)
      procedure() :: extra  ! user-specific extra initialization steps
      call extra()
   end subroutine initialize


   logical function run(load_tests) result(status)
      procedure(LoadTests_interface) :: load_tests
      
      type (TestSuite) :: suite
      class(BaseTestRunner), allocatable :: runner
      type (TestResult) :: r
      type (SerialContext) :: c
      class (ListenerPointer), allocatable :: listeners(:)

      allocate(listeners(1))
      allocate(listeners(1)%pListener, source=newResultPrinter(OUTPUT_UNIT))

!!$      options = parse()
      suite = load_tests()
      allocate(runner, source=newTestRunner(listeners))
      r = runner%run(suite, c)
      status = r%wasSuccessful()

   end function run


   subroutine finalize(extra, successful)
#ifdef NAG
      use f90_unix_proc, only: exit
#endif
      procedure() :: extra  ! user-specific extra initialization steps
      logical, intent(in) :: successful

      call extra() ! user-specific finalize

      if (.not. successful) then
#if defined(NAG) || defined(PGI)
         call exit(-1)
#else
         error stop '*** Encountered 1 or more failures/errors during testing. ***'
#endif
      end if

   end subroutine finalize

   function get_context(use_mpi) result(context)
      class (ParallelContext), allocatable :: context
      logical, intent(in) :: use_mpi

      if (use_mpi) then
         print*,'Cannot use MPI - need to link with pfunit not sfunit.'
         stop
      end if
      context = SerialContext()
   end function get_context


   subroutine stub()
   end subroutine stub

end module sFUnit
