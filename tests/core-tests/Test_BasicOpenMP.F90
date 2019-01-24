!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_BasicOpenMP
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 20 Mar 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 20 Mar 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module Test_BasicOpenMP
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite, only: TestSuite
!$    use PF_TestMethod, only: TestMethod
      type (TestSuite) :: suite

      suite = TestSuite('Test_TestBasicOpenMP')
!$    call suite%addTest(TestMethod('testRunWithOpenMP', testRunWithOpenMP))
!$    call suite%addTest(TestMethod('testSerializeExceptions', testSerializeExceptions))

   end function suite

   ! run on 4 threads.
   subroutine testRunWithOpenMP()
      use PF_Assert, only: assertAll

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
      use PF_Params, only : i32
      use PF_ExceptionList, only: getNumExceptions, clearAll, throw
      use PF_Assert, only: AssertEqual

      !$ integer :: omp_get_thread_num
      integer, parameter :: N = 8 ! threads
      integer, parameter :: M = 10 ! throws per thread
      integer(kind=i32) :: numThrown
      integer(kind=i32), parameter :: NUM_EXPECTED = N*M
      integer :: i

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

end module Test_BasicOpenMP

