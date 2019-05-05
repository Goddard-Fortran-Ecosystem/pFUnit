module FUnit_Core
   use PF_SourceLocation
   use PF_Exception
   use PF_ExceptionVector
   use PF_ExceptionList
   use PF_Expectation
   use PF_Test
   use PF_TestSuite
   use PF_TestCase
   use PF_TestMethod
   use PF_AbstractTestParameter
   use PF_ParameterizedTestCase
   use PF_TestResult
   use PF_TestRunner
   use PF_BaseTestRunner
   use PF_RemoteRunner

   use PF_TestListener
   use PF_TestListenerVector
   use PF_XmlPrinter
   use PF_ResultPrinter
   use PF_DebugListener

   use gFTL_StringVector
   use gFTL_StringUnlimitedMap

   use PF_RobustRunner
   use PF_Assert
   use PF_ParallelContext
   use PF_SerialContext

   use Pf_TestAnnotation
   use Pf_DisableAnnotation
   use Pf_TimeoutAnnotation

   use pf_NameFilter

   use fArgParse

   implicit none
   private

   public :: SourceLocation
   public :: Test
   public :: TestSuite
   public :: TestMethod
   public :: TestResult
   public :: TestRunner
   public :: BaseTestRunner
   public :: RemoteRunner

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
   public :: TimeoutAnnotation

end module FUnit_Core

