module Test_BasicOpenMP_mod
   use TestSuite_mod, only: TestSuite, newTestSuite
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use TestMethod_mod, only: newTestMethod
      type (TestSuite) :: suite

      suite = newTestSuite('Test_TestMethod')
!$    call suite%addTest(newTestMethod('testRunWithOpenMP', testRunWithOpenMP))
!$    call suite%addTest(newTestMethod('testSerializeExceptions', testSerializeExceptions))

   end function suite

   ! run on 4 threads.
   subroutine testRunWithOpenMP()
      use Assert_mod, only: assertAll

      !$ integer :: omp_get_thread_num
      integer, parameter :: N = 4
      logical :: wasCalled(0:N-1)

      wasCalled = .false.

      !$ call omp_set_dynamic(.true.)
      !$ call omp_set_num_threads(N)
      !$ call omp_set_dynamic(.false.)

      !$omp parallel
      !$ wasCalled(omp_get_thread_num()) = .true.
      !$omp end parallel

      call assertAll(wasCalled)

   end subroutine testRunWithOpenMP

   ! Attempt to cause a race condition in accessing the global exception stack.
   ! Each thread throws several exceptions.  The stack should be the same size
   ! as the number that were thrown.   Of course actually crashing is a more
   ! likely failure mode than a mismatch in the count.
   subroutine testSerializeExceptions()
      use Exception_mod, only: throw, getNumExceptions, clearAll
      use Assert_mod, only: AssertEqual

      !$ integer :: omp_get_thread_num
      integer, parameter :: N = 8 ! threads
      integer, parameter :: M = 10 ! throws per thread
      integer :: numThrown
      integer :: i
      integer, parameter :: NUM_EXPECTED = N*M

      !$ call omp_set_dynamic(.true.)
      !$ call omp_set_num_threads(N)
      !$ call omp_set_dynamic(.false.)

      !$omp parallel
      do i = 1, M
         call throw('fail')
      end do
      !$omp end parallel

      numThrown = getNumExceptions()
      call clearAll()
      call assertEqual(NUM_EXPECTED, numThrown)

   end subroutine testSerializeExceptions

end module Test_BasicOpenMP_mod

