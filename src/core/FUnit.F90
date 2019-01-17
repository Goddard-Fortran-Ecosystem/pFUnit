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
module FUnit_private
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
   use PF_TestListenerVector_mod
   use PF_XmlPrinter_mod
   use PF_ResultPrinter_mod
   use PF_DebugListener_mod

   use gFTL_StringVectorMod
   use gFTL_StringUnlimitedMapMod

   use PF_RobustRunner_mod
   use PF_Assert_mod
   use PF_ParallelContext_mod
   use PF_SerialContext_mod

   use Pf_TestAnnotation_mod

   use pf_NameFilter_mod

   use fParse

   implicit none
   private

   public :: SourceLocation
   public :: Test
   public :: TestSuite
   public :: TestMethod
   public :: TestResult
   public :: TestRunner
   public :: BaseTestRunner
   public :: SubsetRunner

   public :: TestListener
   public :: TestListenerVector
   public :: ListenerPointer
   public :: ResultPrinter
   public :: XmlPrinter
   public :: DebugListener

   public :: NameFilter

   public :: RobustRunner
   public :: TestCase
   public :: AbstractTestParameter
   public :: ParameterizedTestCase
   public :: ParallelContext
   public :: SerialContext

   public :: assertFail
   public :: assertTrue, assertFalse
   public :: assertEqual
   public :: assertAny
   public :: assertAll
   public :: assertNone
   public :: assertNotAll
!!$   public :: assertLessThan, assertLessThanOrEqual
!!$   public :: assertGreaterThan, assertGreaterThanOrEqual
!!$   public :: assertRelativelyEqual
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

   public :: LoadTests_interface

   abstract interface
      function LoadTests_interface() result(suite)
         import TestSuite
         type (TestSuite) :: suite
      end function LoadTests_interface
   end interface

   public :: TestAnnotation
   public :: Disable

end module FUnit_private

module FUnit
   ! use these, but do not re-export:
   use FUnit_private
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
      procedure(), optional :: extra  ! user-specific extra initialization steps

      if (present(extra)) call extra()
   end subroutine initialize


   logical function run(load_tests) result(status)
      use fparse
      procedure(LoadTests_interface) :: load_tests
      
      type (TestSuite) :: suite
      class(BaseTestRunner), allocatable :: runner
      type (TestResult) :: r
      type (SerialContext) :: c
      type (TestListenerVector) :: listeners
      type(ArgParser) :: parser
      logical :: debug
      type (StringUnlimitedMap) :: options
      class(*), pointer :: option
      character(:), allocatable :: pattern

      parser = ArgParser()
      call parser%add_argument('-d', '--debug', '--verbose', action='store_true', &
           & help='make output more verbose')

      call parser%add_argument('-f', '--filter', action='store', &
           & help='only run tests that match pattern')
      options =  parser%parse_args()
      
      call listeners%push_back(ResultPrinter(OUTPUT_UNIT))
      option => options%at('debug')
      if (associated(option)) then
         call cast(option, debug)
         if (debug) call listeners%push_back(DebugListener(OUTPUT_UNIT))
      end if

      
!!$      options = parse()
      suite = load_tests()

      option => options%at('filter')
      if (associated(option)) then
         call cast(option, pattern)
         suite = suite%filter(NameFilter(pattern))
      end if
      
      allocate(runner, source=TestRunner(listeners))
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
         print*,'Cannot use MPI - need to link with pFUnit not FUnit.'
         stop
      end if
      context = SerialContext()
   end function get_context


   subroutine stub()
   end subroutine stub

end module FUnit
