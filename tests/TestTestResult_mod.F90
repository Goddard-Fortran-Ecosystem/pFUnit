module TestTestResult_mod
   use Assert_mod
   use Report_mod
   use TestResult_mod
   implicit none
   private

   public :: test_testStarted
   public :: test_testFailed
   public :: test_testFailedReport
   public :: test_summary

contains

   subroutine test_testStarted()
      type (TestResult_type) :: result

      result = newTestResult()
      call assertEqual(0, numRun(result))

      call testStarted(result, 'noname')
      call assertEqual(1, numRun(result))
      
      call clean(result)

   end subroutine test_testStarted

   subroutine test_testFailed()
      type (TestResult_type) :: result

      result = newTestResult()
      call assertEqual(0, numFailed(result))

      call testStarted(result)
      call assertEqual(0, numFailed(result))

      call testFailed(result, 'dummy test name', 'dummy message')
      call assertEqual(1, numFailed(result))
      call testFailed(result, 'dummy test name', 'dummy message')
      call assertEqual(2, numFailed(result))
      
      call clean(result)

   end subroutine test_testFailed

   subroutine test_testFailedReport()
      type (TestResult_type) :: result
      type (Report_type) :: rprt

      result = newTestResult()
      rprt = Report( (/ 'message 1', 'message 2' /) )

      call testFailed(result, 'dummy test name', rprt)
      call assertEqual(1, numFailed(result))
      call assertEqual(2, numSevere(result))
      
      call clean(result)
      call clean(rprt)

   end subroutine test_testFailedReport

   subroutine test_summary()
      type (TestResult_type) :: result
      result = newTestResult()

      call assertEqual("0 run, 0 failed",Summary(result,timer=.false.))
      call testStarted(result)
      call assertEqual("1 run, 0 failed",Summary(result,timer=.false.))
      call testFailed(result, 'dummy test name', 'dummy message')
      call assertEqual("1 run, 1 failed (1 severe)",Summary(result,timer=.false.))

      call clean(result)
   end subroutine test_summary

end module TestTestResult_mod
