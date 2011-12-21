program main
   use pFUnit
   use testUseFixture_mod, only: getTestSuite
   implicit none

   type (TestSuite_type) :: suite
   type (TestResult_type) :: result
   character(len=100) :: summary_statement

   call pFUnit_init()

! Build suite from test procedures:
   suite = TestSuite('all suites')
   call add(suite, getTestSuite())

! Run the tests and accumulate the results in "result"
   result = newTestResult(mode=MODE_USE_STDOUT)
   call Run(suite, result)
   summary_statement=Summary(result)
   print*,' '
   print*,trim(summary_statement)

   call clean(result)
   call clean(suite)

   call pFUnit_finalize()

end program main
