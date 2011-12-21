program main
   use pFUnit
   implicit none

   external testZeroVectors
   external testSameVector
   external testOrthogonalVectors
   external testFailOnPurpose

   type (TestSuite_type) :: suite
   type (TestResult_type) :: result
   character(len=100) :: summary_statement

   call pFUnit_init()

! Build suite from test procedures:
   suite = TestSuite('vector tests')

   call add(suite, TestCase1Step('testZeroVectors', testZeroVectors))
   call add(suite, TestCase1Step('testSameVector', testSameVector))
   call add(suite, TestCase1Step('testOrthogonalVectors', testOrthogonalVectors))
   call add(suite, TestCase1Step('testFailOnPurpose', testFailOnPurpose))

! Run the tests and accumulate the results in "result"
   result = newTestResult(mode=MODE_USE_STDOUT)
   call Run(suite, result)
   summary_statement=Summary(result)
   print*,trim(summary_statement)

   call clean(result)
   call clean(suite)

   call pFUnit_finalize()

end program main
