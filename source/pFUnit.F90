module pFUnit_mod
   use Exception_mod
   use TestSuite_mod
   use TestCase_mod
   use TestMethod_mod
   use TestRunner_mod
   use Assert_mod
   use AssertReal_mod
   use ParallelContext_mod
   use SerialContext_mod
#ifdef USE_MPI
   use MpiContext_mod
#endif
   implicit none
   private

   public :: initialize
   public :: finalize

   public :: TestSuite, newTestSuite
   public :: TestMethod, newTestMethod
   public :: TestRunner, newTestRunner
   public :: ParallelContext
   public :: SerialContext, newSerialContext
#ifdef USE_MPI
   public :: MpiContext,newMpiContext
#endif

   public :: assertEqual
   public :: throw

contains

   subroutine initialize()
#ifdef USE_MPI
      include 'mpif.h'
      integer :: error
      call mpi_init(error)
#endif
      call initializeGlobalExceptionList()

   end subroutine initialize
   
   subroutine finalize()
#ifdef USE_MPI
      integer :: error
      call mpi_finalize(error)
#endif
   end subroutine finalize

end module pFUnit_mod
