#ifdef USE_MPI
subroutine debug(line, file)
   integer, intent(in) :: line
   character(len=*), intent(in) :: file
   include 'mpif.h'

   integer :: rank, ier
   call mpi_comm_rank(MPI_COMM_WORLD, rank ,ier)
   write(20+rank, *) 'here', line, trim(file), MPI_COMM_WORLD
   call flush(20+rank)
end subroutine debug
#endif

#include "reflection.h"
program main
   use F2kUnit, only: initializeF2kUnit
   use F2kUnit, only: finalizeF2kUnit
   implicit none

   call initializeF2kUnit()

   call runTests()

   call finalizeF2kUnit()

contains

   subroutine runTests()
      use F2kUnit, only: newTestSuite
      use F2kUnit, only: TestSuite
      use F2kUnit, only: TestRunner, newTestRunner
#ifdef USE_MPI
      use MpiContext_mod
#else
      use SerialContext_mod
#endif
      use ParallelContext_mod

      use Test_StringUtilities_mod, only: stringUtilitiesSuite => suite    ! (1)
      use Test_Exception_mod, only: exceptionSuite => suite                ! (2)
      use Test_Assert_mod, only: assertSuite => suite                      ! (3)
      use Test_AssertionLocation_mod, only: assertLocationSuite => suite   ! (4)
      use Test_AssertReal_mod, only: assertRealSuite => suite              ! (5)

      use Test_TestResult_mod, only: testResultSuite => suite              ! (6)
      use Test_TestSuite_mod, only: testSuiteSuite => suite                ! (7)

      use Test_TestMethod_mod, only: testSimpleMethodSuite => suite  ! (8)
      use Test_SimpleTestCase_mod, only: testSimpleSuite => suite          ! (9)
      use Test_FixtureTestCase_mod, only: testFixtureSuite => suite        ! (10)


      use Test_MockCall_mod, only: testMockCallSuite => suite      ! (11)
      use Test_MockRepository_mod, only: testMockRepositorySuite => suite      ! (11)

#ifdef USE_MPI
      use Test_MpiTestCase_mod, only: MpiTestCaseSuite => suite            ! (12)
#endif

      type (TestSuite) :: allTests
      type (TestRunner) :: runner

      allTests = newTestSuite('allTests')
      runner = newTestRunner()

#define ADD(suite) call allTests%addTest(suite())

      ADD(stringUtilitiesSuite)
      ADD(exceptionSuite)

      ADD(assertSuite)
      ADD(assertLocationSuite)
      ADD(assertRealSuite)

      ADD(testResultSuite)
      ADD(testSuiteSuite)

      ADD(testSimpleMethodSuite)
      ADD(testSimpleSuite)
      ADD(testFixtureSuite)

      ADD(testMockCallSuite)
      ADD(testMockRepositorySuite)

#ifdef USE_MPI
      ADD(MpiTestCaseSuite)
      call runner%run(allTests, newMpiContext())
#else
      call runner%run(allTests, newSerialContext())
#endif

   end subroutine runTests

end program main

