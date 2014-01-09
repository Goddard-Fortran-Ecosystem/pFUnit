#ifdef USE_MPI
subroutine debug(line, file)
   integer, intent(in) :: line
   character(len=*), intent(in) :: file
   include 'mpif.h'

   integer :: rank, ier
   call mpi_comm_rank(MPI_COMM_WORLD, rank ,ier)
   write(20+rank, *) 'here', line, trim(file), MPI_COMM_WORLD
   flush(20+rank)
end subroutine debug
#endif

#include "reflection.h"
program main
   use pFUnit_mod, only: initialize
   use pFUnit_mod, only: finalize
   implicit none

   call initialize()
   call runTests()
   call finalize()

contains

   subroutine runTests()
      use pFUnit_mod, only: newTestSuite
      use pFUnit_mod, only: TestSuite
      use pFUnit_mod, only: TestRunner, newTestRunner
#ifdef USE_MPI
      use MpiContext_mod
      use ParallelException_mod
#else
      use SerialContext_mod
#endif
      use ParallelContext_mod

      use Test_StringConversionUtilities_mod, only: StringConversionUtilitiesSuite => suite    ! (1)
#ifdef BUILD_ROBUST
      use Test_UnixProcess_mod, only: unixProcessSuite => suite                ! (1)
#endif
      use Test_Exception_mod, only: exceptionSuite => suite                ! (2)
      use Test_AssertBasic_mod, only: assertBasicSuite => suite            !
      use Test_Assert_mod, only: assertSuite => suite                      ! (3)
      use Test_AssertInteger_mod, only: assertIntegerSuite => suite        !
      use Test_AssertReal_mod, only: assertRealSuite => suite              ! (5)
      use Test_AssertComplex_mod, only: assertComplexSuite => suite              ! (5)

      use Test_TestResult_mod, only: testResultSuite => suite              ! (6)
      use Test_TestSuite_mod, only: testSuiteSuite => suite                ! (7)

      use Test_TestMethod_mod, only: testSimpleMethodSuite => suite  ! (8)
      use Test_SimpleTestCase_mod, only: testSimpleSuite => suite          ! (9)
      use Test_FixtureTestCase_mod, only: testFixtureSuite => suite        ! (10)

      use Test_BasicOpenMP_mod, only: testBasicOpenMpSuite => suite  ! (8)

      use Test_MockCall_mod, only: testMockCallSuite => suite      ! (11)
      use Test_MockRepository_mod, only: testMockRepositorySuite => suite      ! (11)

#ifdef BUILD_ROBUST
      use Test_RobustRunner_mod, only: testRobustRunnerSuite => suite
#endif

#ifdef USE_MPI
      use Test_MpiContext_mod, only: MpiContextSuite => suite            ! (12)
      use Test_MpiException_mod, only: ParallelExceptionSuite => suite
      use Test_MpiTestCase_mod, only: MpiTestCaseSuite => suite            ! (12)
#endif

      type (TestSuite) :: allTests
      type (TestRunner) :: runner

      allTests = newTestSuite('allTests')
      runner = newTestRunner()

#define ADD(suite) call allTests%addTest(suite())

      ADD(StringConversionUtilitiesSuite)
#ifdef BUILD_ROBUST
      ADD(UnixProcessSuite)
#endif
      ADD(exceptionSuite)

      ADD(assertBasicSuite)
      ADD(assertSuite)
      ADD(assertIntegerSuite)
      ADD(assertRealSuite)
      ADD(assertComplexSuite)

      ADD(testResultSuite)
      ADD(testSuiteSuite)

      ADD(testSimpleMethodSuite)
      ADD(testSimpleSuite)
      ADD(testFixtureSuite)

      ADD(testBasicOpenMpSuite)

      ADD(testMockCallSuite)
      ADD(testMockRepositorySuite)

#ifdef BUILD_ROBUST
      ADD(testRobustRunnerSuite)
#endif

#ifdef USE_MPI
      ADD(MpiContextSuite)
      ADD(ParallelExceptionSuite)
      ADD(MpiTestCaseSuite)
#endif

#ifdef USE_MPI
      call runner%run(allTests, newMpiContext())
#else
      call runner%run(allTests, newSerialContext())
#endif

   end subroutine runTests

end program main

