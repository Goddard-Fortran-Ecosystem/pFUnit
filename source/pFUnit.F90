!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: pFUnit
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
module pFUnit
   use PF_SourceLocation_mod
   use PF_Exception_mod
   use PF_ParallelException_mod
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

#ifdef BUILD_ROBUST
   use PF_RobustRunner_mod
#endif
   use PF_Assert_mod
   use PF_ParallelContext_mod
   use PF_SerialContext_mod
#ifdef USE_MPI
   use PF_MpiContext_mod
   use PF_MpiTestCase_mod
   use PF_MpiTestParameter_mod
   use PF_MpiTestMethod_mod
#endif
   implicit none
   private

   public :: initialize
   public :: finalize

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
#ifdef USE_MPI
   public :: MpiContext, newMpiContext
   public :: MpiTestCase
   public :: MpiTestParameter
   public :: MpiTestMethod, newMpiTestMethod
#endif

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


#ifdef USE_MPI
   logical :: useMpi_
#endif

contains

   subroutine initialize(useMpi)
      logical, optional, intent(in) :: useMpi
#ifdef USE_MPI
      include 'mpif.h'
      integer :: error

      useMpi_ = .true.
      if (present(useMpi)) useMpi_ = useMpi

      if (useMpi_) then
         call mpi_init(error)
      end if
#endif
      call initializeGlobalExceptionList()

   end subroutine initialize

   subroutine finalize(successful)
#ifdef NAG
      use f90_unix_proc, only: exit
#endif
      logical, intent(in) :: successful

      logical :: allSuccessful
      logical :: amRoot

#ifdef USE_MPI
      integer :: error
      integer :: rank
      include 'mpif.h'

      allSuccessful = successful
      if (useMpi_) then
         call MPI_Comm_rank(MPI_COMM_WORLD, rank, error)
         amRoot = (rank == 0)
         call MPI_Bcast(allSuccessful, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, error)
         call mpi_finalize(error)
      else
         ! If using MPI-PFUNIT on serial code, ensure amRoot is set.
         amRoot = .true.
      end if
#else
      amRoot = .true.
      allSuccessful = successful
#endif

   if (.not. allSuccessful) then
#if defined(NAG) || defined(PGI)
      call exit(-1)
#else

      if (amRoot) then
         error stop '*** Encountered 1 or more failures/errors during testing. ***'
      else
         error stop
      end if
#endif
   end if

   end subroutine finalize

end module pFUnit
