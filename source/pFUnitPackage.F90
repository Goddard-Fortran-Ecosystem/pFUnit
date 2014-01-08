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
!
! This module packages pFUnit entities while simultaneously inserting
! a prefix on all names.  Some developers may provide this explicit
! naming convention.  Others may choose to use the vanilla pFUnit module that
! has no such prefix.   
!
! The default prefix is "pf_", but just edit the #define below to suit
! your own preference.
!
!

#ifndef PFUNIT_PREFIX
#define PFUNIT_PREFIX pf_
#endif

#define TOKEN(a) a
#define RENAME(item) TOKEN(PFUNIT_PREFIX)TOKEN(item) => item

module pFUnit

   use pFUnit_mod, only: RENAME(initialize)
   use pFUnit_mod, only: RENAME(finalize)

   use SourceLocation_mod, only: RENAME(SourceLocation)
   use Exception_mod, only:  RENAME(throw), RENAME(catch), RENAME(catchAny)
   use Exception_mod, only:  RENAME(anyExceptions)
   use ParallelException_mod, only:  RENAME(anyExceptions)
   use Assert_mod, only: RENAME(assertTrue), RENAME(assertFalse)
   use Assert_mod, only: RENAME(assertSameShape)
   use Assert_mod, only: RENAME(assertEqual)
   use Assert_mod, only: RENAME(assertAny), RENAME(assertAll)
   use Assert_mod, only: RENAME(assertNone), RENAME(assertNotAll)
   use Assert_mod, only: RENAME(assertLessThan)
   use Assert_mod, only: RENAME(assertLessThanOrEqual)
   use Assert_mod, only: RENAME(assertGreaterThan)
   use Assert_mod, only: RENAME(assertGreaterThanOrEqual)
   use Assert_mod, only: RENAME(assertExceptionRaised)

   use Assert_mod, only: RENAME(assertIsNan)
   use Assert_mod, only: RENAME(assertIsFinite)

   use Test_mod, only: RENAME(Test)
   use TestCase_mod, only: RENAME(TestCase)
   use TestSuite_mod, only: RENAME(TestSuite)
   use TestSuite_mod, only: RENAME(newTestSuite)
   use TestMethod_mod, only: RENAME(TestMethod)
   use TestMethod_mod, only: RENAME(newTestMethod)
   use BaseTestRunner_mod, only: RENAME(BaseTestRunner)
   use TestRunner_mod, only: RENAME(TestRunner)
   use TestRunner_mod, only: RENAME(newTestRunner)
#ifdef BUILD_ROBUST
   use RobustRunner_mod, only: RENAME(RobustRunner)
#endif

   use ParallelContext_mod, only: RENAME(ParallelContext)
   use SerialContext_mod, only: RENAME(SerialContext)
   use SerialContext_mod, only: RENAME(newSerialContext)
#ifdef USE_MPI
   use MpiContext_mod, only: RENAME(MpiContext)
   use MpiContext_mod, only: RENAME(newMpiContext)
   use MpiTestCase_mod, only: RENAME(MpiTestCase)
   use MpiTestMethod_mod, only: RENAME(MpiTestMethod)
   use MpiTestMethod_mod, only: RENAME(newMpiTestMethod)
#endif

   implicit none
   public ! Nothing private in this module, just renaming exports.

end module pFUnit
