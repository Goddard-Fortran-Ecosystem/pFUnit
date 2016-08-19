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
   use sFUnit_private
   use PF_MpiContext_mod
   use PF_MpiTestCase_mod
   use PF_MpiTestParameter_mod
   use PF_MpiTestMethod_mod

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
   public :: MpiContext, newMpiContext
   public :: MpiTestCase
   public :: MpiTestParameter
   public :: MpiTestMethod, newMpiTestMethod

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

   logical, save :: use_mpi_

contains

   subroutine initialize(use_mpi)
      use PF_Exception_mod, only: initializeGlobalExceptionList
      use mpi
      logical, optional, intent(in) :: use_mpi
      integer :: error

      if (present(use_mpi)) then
         use_mpi_ = use_mpi ! save for finalize() step
      else
         use_mpi_ = .true.
      end if

      if (use_mpi_) then
         call mpi_init(error)
         if (error /= MPI_SUCCESS) stop
      end if

      call initializeGlobalExceptionList()

   end subroutine initialize

   subroutine finalize(successful)
      use mpi
#ifdef NAG
      use f90_unix_proc, only: exit
#endif
      logical, intent(in) :: successful

      logical :: allSuccessful
      logical :: amRoot

      integer :: error
      integer :: rank

      allSuccessful = successful
      if (use_mpi_) then
         call MPI_Comm_rank(MPI_COMM_WORLD, rank, error)
         amRoot = (rank == 0)
         call MPI_Bcast(allSuccessful, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, error)
         call MPI_Finalize(error)
      else
         ! If using MPI-PFUNIT on serial code, ensure amRoot is set.
         amRoot = .true.
      end if

      if (.not. allSuccessful) then
#if defined(NAG) || defined(PGI)
         call exit(-1)
#else

         if (amRoot) then
            error stop '*** Encountered 1 or more failures/errors during testing. ***'
         else
            stop
         end if
#endif
      else
         stop
      end if
   end subroutine finalize

end module pFUnit
