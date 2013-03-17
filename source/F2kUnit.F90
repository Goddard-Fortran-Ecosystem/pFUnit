module F2kUnit
   use Exception_mod
   use TestSuite_mod
   use TestRunner_mod
   use ParallelContext_mod
   use Assert_mod
#ifdef USE_MPI
   use MpiContext_mod
#endif
   implicit none
   private

   public :: initializeF2kUnit
   public :: finalizeF2kUnit

   public :: newTestSuite
   public :: TestSuite
   public :: TestRunner, newTestRunner
   public :: ParallelContext
#ifdef USE_MPI
   public :: MpiContext
   public :: newMpiContext
#endif

   public :: assertEqual
   public :: throw

contains

   subroutine initializeF2kUnit()
#ifdef USE_MPI
      include 'mpif.h'
      integer :: error
      call mpi_init(error)
#endif
      call initializeGlobalExceptionList()

   end subroutine initializeF2kUnit

   subroutine finalizeF2kUnit()
#ifdef USE_MPI
      integer :: error
      call mpi_finalize(error)
#endif
   end subroutine finalizeF2kUnit

end module F2kUnit
