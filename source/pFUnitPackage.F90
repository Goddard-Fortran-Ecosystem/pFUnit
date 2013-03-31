!
! This module packages pFUnit entities while simultaneously inserting
! "pfunit_" as a prefix on all names.  Some developers may provide this explicit
! naming convention.  Others may choose to use the vanilla pFUnit module that
! has no such prefix.

#define TOKEN(a) a
#define RENAME(item) TOKEN(pFUnit_)TOKEN(item) => item

module pFUnit
   use Exception_mod, only:  RENAME(throw)
   use Exception_mod, only:  RENAME(catchAny)
   use Exception_mod, only:  RENAME(noExceptions)

   use TestSuite_mod, only: RENAME(TestSuite)
   use TestSuite_mod, only: RENAME(newTestSuite)

   use TestRunner_mod, only: RENAME(TestRunner)
   use TestRunner_mod, only: RENAME(newTestRunner)

   use ParallelContext_mod, only: RENAME(ParallelContext)

   use Assert_mod, only: RENAME(assertTrue)
   use Assert_mod, only: RENAME(assertFalse)
   use Assert_mod, only: RENAME(assertEqual)
   use Assert_mod, only: RENAME(assertExceptionRaised)
   use AssertReal_mod, only: RENAME(assertEqual)

#ifdef USE_MPI
   use MpiContext_mod, only: RENAME(MpiContext)
   use MpiContext_mod, only: RENAME(newMpiContext)
#endif

   use pFUnit_mod, only: RENAME(initialize)
   use pFUnit_mod, only: RENAME(finalize)

   implicit none
   public ! Nothing private in this module, just renaming exports.

end module pFUnit
