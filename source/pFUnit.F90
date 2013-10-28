module pFUnit_mod
   use SourceLocation_mod
   use Exception_mod
   use Test_mod
   use TestSuite_mod
   use TestCase_mod
   use TestMethod_mod
   use ParameterizedTestCase_mod
   use BaseTestRunner_mod
   use TestRunner_mod
   use SubsetRunner_mod
   use RobustRunner_mod
   use Assert_mod
   use AssertReal_mod
   use ParallelContext_mod
   use SerialContext_mod
#ifdef USE_MPI
   use MpiContext_mod
   use MpiTestCase_mod
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
   public :: BaseTestRunner
   public :: TestRunner, newTestRunner
   public :: SubsetRunner
   public :: RobustRunner
   public :: TestCase
   public :: ParameterizedTestCase, AbstractTestParameter
   public :: ParallelContext
   public :: SerialContext, newSerialContext
#ifdef USE_MPI
   public :: MpiContext, newMpiContext
   public :: MpiTestCase
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
   
   subroutine finalize()
#ifdef USE_MPI
      integer :: error
      if (useMpi_) then
         call mpi_finalize(error)
      end if
#endif
   end subroutine finalize

end module pFUnit_mod
