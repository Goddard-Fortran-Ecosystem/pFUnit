program main
   use pfunit_mod
   implicit none

   type (TestSuite) :: all
   type (TestRunner) :: runner
   class (ParallelContext), pointer :: context


   call initialize()

   all = getTestSuites()
   runner = newTestRunner()

#ifdef USE_MPI
   allocate(context, source = newMpiContext())
#else
   allocate(context, source = newSerialContext())
#endif

   call runner%run(all, context)

contains

   function getTestSuites() result(suite)
      type (TestSuite) :: suite

#define ADD_TEST_SUITE(s) type (TestSuite), external :: s
#include "testSuites.inc"
#undef ADD_TEST_SUITE

      suite = newTestSuite()

   ! accumulate tests in top suite
#define ADD_TEST_SUITE(s) call suite%addTest(s())
#include "testSuites.inc"
#undef ADD_TEST_SUITE

   end function getTestSuites
end program main

   
   






