program TestDriver
   use pFUnit
   use TestSumInverse_mod
   implicit none

   type (TestSuite_type) :: suite
   type (TestResult_type) :: result
   character(len=100) :: summary_statement

   call pFUnit_init()

   ! Build suite from test procedures:
   suite = TestSuite('Sum Inverse tests')

   call add(suite,TestCase1Step('testSumInverseEmpty',testSumInverseEmpty))
   call add(suite,TestCase1Step('testSumInverseScalar',testSumInverseScalar))
   call add(suite,TestCase1Step('testSumInverseArray',testSumInverseArray))

   result = newTestResult(mode=MODE_USE_STDOUT)
   call Run(suite,result)

   summary_statement = Summary(result)
   print *, trim(summary_statement)

   call clean(result)
   call clean(suite)

   call pFUnit_finalize() 

end program TestDriver
