!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: pFUnit
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC 
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
module pFUnit_mod
   use SourceLocation_mod
   use Exception_mod
   use ParallelException_mod
   use Test_mod
   use TestSuite_mod
   use TestCase_mod
   use TestMethod_mod
   use AbstractTestParameter_mod
   use ParameterizedTestCase_mod
   use TestResult_mod
   use BaseTestRunner_mod
   use TestRunner_mod
   use SubsetRunner_mod
#ifndef Windows
   use RobustRunner_mod
#endif
   use Assert_mod
   use AssertReal_mod
   use ParallelContext_mod
   use SerialContext_mod
#ifdef USE_MPI
   use MpiContext_mod
   use MpiTestCase_mod
   use MpiTestParameter_mod
   use MpiTestMethod_mod
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
#ifndef Windows
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

   public :: assertTrue, assertFalse
   public :: assertEqual
   public :: assertAny
   public :: assertAll
   public :: assertNone
   public :: assertNotAll
   public :: assertLessThan, assertLessThanOrEqual
   public :: assertGreaterThan, assertGreaterThanOrEqual
   public :: assertExceptionRaised
   public :: assertSameShape
   public :: assertIsNan
   public :: assertIsFinite

   public :: throw, catchAny, catch, anyExceptions

   logical :: useMpi_

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
      end if
#else
      amRoot = .true.
      allSuccessful = successful
#endif

   if (.not. allSuccessful) then
#ifdef NAG
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

end module pFUnit_mod
