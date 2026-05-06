#include "unused_dummy.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_TestSuite_Shuffle
!
!> @brief
!! Unit tests for TestSuite shuffle functionality
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 10 Mar 2025
!
!-------------------------------------------------------------------------------
module Test_TestSuite_Shuffle
   use PF_TestSuite, only: TestSuite
   use PF_TestResult
   use PF_Assert, only: assertEqual, assertNotEqual, assertTrue, assertFalse
   use PF_TestMethod, only: TestMethod
   use PF_SerialContext
   implicit none
   private

   public :: suite

   ! Internal mock for TestResult that logs test execution order
   type, extends(TestResult) :: LoggingResult
      character(len=200) :: log
   contains
      procedure :: run => logging_run
   end type LoggingResult

contains

   function suite()
      type (TestSuite) :: suite

      suite = TestSuite('TestSuite_Shuffle_Suite')

      call suite%addTest(TestMethod('test_no_shuffle_deterministic', &
           & test_no_shuffle_deterministic))
      call suite%addTest(TestMethod('test_shuffle_seed_reproducible', &
           & test_shuffle_seed_reproducible))
      call suite%addTest(TestMethod('test_shuffle_changes_order', &
           & test_shuffle_changes_order))
      call suite%addTest(TestMethod('test_shuffle_empty_suite', &
           & test_shuffle_empty_suite))
      call suite%addTest(TestMethod('test_shuffle_single_test', &
           & test_shuffle_single_test))
      call suite%addTest(TestMethod('test_shuffle_preserves_suite_boundaries', &
           & test_shuffle_preserves_suite_boundaries))
   end function suite


   subroutine test_no_shuffle_deterministic()
      ! Verify that without shuffle, tests run in deterministic order
      type(TestSuite) :: test_suite
      type(LoggingResult) :: result1, result2

      test_suite = TestSuite('test')
      call test_suite%addTest(TestMethod('t1', dummy_method))
      call test_suite%addTest(TestMethod('t2', dummy_method))
      call test_suite%addTest(TestMethod('t3', dummy_method))
      call test_suite%addTest(TestMethod('t4', dummy_method))
      call test_suite%addTest(TestMethod('t5', dummy_method))

      result1%TestResult = TestResult()
      result1%log = ''
      result2%TestResult = TestResult()
      result2%log = ''

      call test_suite%run(result1, SerialContext())
      call test_suite%run(result2, SerialContext())

      ! Both runs should produce identical order
      call assertEqual(result1%log, result2%log, 'Order should be deterministic without shuffle')
   end subroutine test_no_shuffle_deterministic


   subroutine test_shuffle_seed_reproducible()
      ! Verify that same seed produces same order
      type(TestSuite) :: suite1, suite2
      type(LoggingResult) :: result1, result2
      character(len=200) :: order1, order2
      integer :: i, pos1, pos2

      suite1 = TestSuite('test1')
      call suite1%addTest(TestMethod('t1', dummy_method))
      call suite1%addTest(TestMethod('t2', dummy_method))
      call suite1%addTest(TestMethod('t3', dummy_method))
      call suite1%addTest(TestMethod('t4', dummy_method))
      call suite1%addTest(TestMethod('t5', dummy_method))
      call suite1%addTest(TestMethod('t6', dummy_method))
      call suite1%addTest(TestMethod('t7', dummy_method))
      call suite1%addTest(TestMethod('t8', dummy_method))
      call suite1%addTest(TestMethod('t9', dummy_method))
      call suite1%addTest(TestMethod('t10', dummy_method))

      suite2 = TestSuite('test1')  ! Use same suite name for easier comparison
      call suite2%addTest(TestMethod('t1', dummy_method))
      call suite2%addTest(TestMethod('t2', dummy_method))
      call suite2%addTest(TestMethod('t3', dummy_method))
      call suite2%addTest(TestMethod('t4', dummy_method))
      call suite2%addTest(TestMethod('t5', dummy_method))
      call suite2%addTest(TestMethod('t6', dummy_method))
      call suite2%addTest(TestMethod('t7', dummy_method))
      call suite2%addTest(TestMethod('t8', dummy_method))
      call suite2%addTest(TestMethod('t9', dummy_method))
      call suite2%addTest(TestMethod('t10', dummy_method))

      ! Set same seed for both
      call suite1%set_shuffle(12345)
      call suite2%set_shuffle(12345)

      result1%TestResult = TestResult()
      result1%log = ''
      result2%TestResult = TestResult()
      result2%log = ''

      call suite1%run(result1, SerialContext())
      call suite2%run(result2, SerialContext())

      ! Same seed should produce same order
      call assertEqual(result1%log, result2%log, 'Same seed should produce same order')
   end subroutine test_shuffle_seed_reproducible


   subroutine test_shuffle_changes_order()
      ! Verify that shuffle actually changes order
      ! With 10 tests, probability of same order is 1/10! ≈ 0.0000003%
      type(TestSuite) :: test_suite
      type(LoggingResult) :: no_shuffle_result, shuffle_result
      logical :: order_changed

      test_suite = TestSuite('test')
      call test_suite%addTest(TestMethod('t1', dummy_method))
      call test_suite%addTest(TestMethod('t2', dummy_method))
      call test_suite%addTest(TestMethod('t3', dummy_method))
      call test_suite%addTest(TestMethod('t4', dummy_method))
      call test_suite%addTest(TestMethod('t5', dummy_method))
      call test_suite%addTest(TestMethod('t6', dummy_method))
      call test_suite%addTest(TestMethod('t7', dummy_method))
      call test_suite%addTest(TestMethod('t8', dummy_method))
      call test_suite%addTest(TestMethod('t9', dummy_method))
      call test_suite%addTest(TestMethod('t10', dummy_method))

      ! Run without shuffle first
      no_shuffle_result%TestResult = TestResult()
      no_shuffle_result%log = ''
      call test_suite%run(no_shuffle_result, SerialContext())

      ! Now enable shuffle with a fixed seed and run again
      call test_suite%set_shuffle(99999)
      shuffle_result%TestResult = TestResult()
      shuffle_result%log = ''
      call test_suite%run(shuffle_result, SerialContext())

      ! Order should be different
      order_changed = (no_shuffle_result%log /= shuffle_result%log)
      call assertTrue(order_changed, 'Shuffle should change test order')
   end subroutine test_shuffle_changes_order


   subroutine test_shuffle_empty_suite()
      ! Verify shuffle doesn't crash on empty suite
      type(TestSuite) :: test_suite
      type(LoggingResult) :: result

      test_suite = TestSuite('empty')
      call test_suite%set_shuffle(12345)

      result%TestResult = TestResult()
      result%log = ''

      ! Should not crash
      call test_suite%run(result, SerialContext())

      call assertEqual('', result%log, 'Empty suite should have empty log')
   end subroutine test_shuffle_empty_suite


   subroutine test_shuffle_single_test()
      ! Verify shuffle works correctly with single test
      type(TestSuite) :: test_suite
      type(LoggingResult) :: result

      test_suite = TestSuite('single')
      call test_suite%addTest(TestMethod('only_test', dummy_method))
      call test_suite%set_shuffle(12345)

      result%TestResult = TestResult()
      result%log = ''

      call test_suite%run(result, SerialContext())

      call assertTrue(index(result%log, 'only_test') > 0, 'Single test should run')
   end subroutine test_shuffle_single_test


   subroutine test_shuffle_preserves_suite_boundaries()
      ! Verify that shuffle preserves suite boundaries (no inter-suite mixing)
      type(TestSuite) :: parent_suite, childA, childB
      type(LoggingResult) :: result
      character(len=200) :: log_str
      integer :: pos_a1, pos_a2, pos_b1, pos_b2

      parent_suite = TestSuite('parent')

      childA = TestSuite('childA')
      call childA%addTest(TestMethod('a1', dummy_method))
      call childA%addTest(TestMethod('a2', dummy_method))
      call childA%addTest(TestMethod('a3', dummy_method))

      childB = TestSuite('childB')
      call childB%addTest(TestMethod('b1', dummy_method))
      call childB%addTest(TestMethod('b2', dummy_method))
      call childB%addTest(TestMethod('b3', dummy_method))

      call parent_suite%addTest(childA)
      call parent_suite%addTest(childB)
      
      ! Enable shuffle on parent (note: current implementation only shuffles
      ! tests within each suite, not the suites themselves)
      call parent_suite%set_shuffle(54321)

      result%TestResult = TestResult()
      result%log = ''

      call parent_suite%run(result, SerialContext())
      log_str = result%log

      ! All childA tests should appear before all childB tests
      ! (or vice versa if suites are shuffled, but tests within suites shouldn't mix)
      pos_a1 = index(log_str, 'childA.a1')
      pos_a2 = index(log_str, 'childA.a2')
      pos_b1 = index(log_str, 'childB.b1')
      pos_b2 = index(log_str, 'childB.b2')

      ! Verify all tests were found
      call assertTrue(pos_a1 > 0, 'childA.a1 should be in log')
      call assertTrue(pos_a2 > 0, 'childA.a2 should be in log')
      call assertTrue(pos_b1 > 0, 'childB.b1 should be in log')
      call assertTrue(pos_b2 > 0, 'childB.b2 should be in log')

      ! Either all A's before all B's, or all B's before all A's
      if (pos_a1 < pos_b1) then
         ! A's should all come before B's
         call assertTrue(pos_a2 < pos_b1, 'Suite boundaries should be preserved (A before B)')
      else
         ! B's should all come before A's
         call assertTrue(pos_b2 < pos_a1, 'Suite boundaries should be preserved (B before A)')
      end if
   end subroutine test_shuffle_preserves_suite_boundaries


   ! Helper methods
   subroutine dummy_method()
      ! Empty test method
   end subroutine dummy_method


   recursive subroutine logging_run(this, test, context)
      use PF_TestCase
      use PF_SurrogateTestCase
      use PF_ParallelContext
      class (LoggingResult), intent(inout) :: this
      class (SurrogateTestCase), intent(inout) :: test
      class (ParallelContext), intent(in) :: context

      _UNUSED_DUMMY(context)
      this%log = trim(this%log)//' ::'//trim(test%getName())
   end subroutine logging_run

end module Test_TestSuite_Shuffle
