module F2kUnit
   use Exception_mod
   use TestSuite_mod
   use TestRunner_mod
   use ParallelContext_mod
   use Assert_mod
   use MpiContext_mod
   implicit none
   private

   public :: initializeF2kUnit
   public :: finalizeF2kUnit

   public :: newTestSuite
   public :: TestSuite
   public :: TestRunner, newTestRunner
   public :: ParallelContext
   public :: MpiContext
   public :: newMpiContext

   public :: assertEqual
   public :: throw

contains

   subroutine initializeF2kUnit()
      include 'mpif.h'
      integer :: error

      call mpi_init(error)

      call initializeGlobalExceptionList()

   end subroutine initializeF2kUnit

   subroutine finalizeF2kUnit()
      integer :: error
      call mpi_finalize(error)
   end subroutine finalizeF2kUnit

end module F2kUnit
