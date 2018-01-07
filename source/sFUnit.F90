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

   use pf_Option_mod
   use pf_OptionParser_mod
   use pf_OptionVector_mod
   use pf_StringVector_mod
   use pf_StringUnlimitedMap_mod

#ifdef BUILD_ROBUST
   use PF_RobustRunner_mod
#endif
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

#ifdef BUILD_ROBUST
   public :: RobustRunner
#endif
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


   public :: Option
   public :: OptionParser
   public :: OptionVector
   public :: StringVector
   public :: StringUnlimitedMap


end module sFUnit_private

module sFUnit
   ! use these, but do not re-export
   ! export these
   use sFUnit_private
   ! add these
   public :: initialize
   public :: finalize


contains

   subroutine initialize()
   end subroutine initialize

   subroutine finalize(successful)
#ifdef NAG
      use f90_unix_proc, only: exit
#endif
      logical, intent(in) :: successful

      if (.not. successful) then
#if defined(NAG) || defined(PGI)
         call exit(-1)
#else
         error stop '*** Encountered 1 or more failures/errors during testing. ***'
#endif
      end if

   end subroutine finalize

   
end module sFUnit
