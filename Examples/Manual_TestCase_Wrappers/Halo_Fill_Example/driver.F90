program main
   use pFUnit
   use testHalo_mod
   implicit none

   type (TestSuite_type) :: suite
   type (TestResult_type) :: result
   character(len=100) :: summary_statement

   call pFUnit_init()

! Build suite from test procedures:
   suite = TestSuite('Halo tests')

   call add(suite, MpiTestCase('testHaloFill', testHaloFill, numProcesses=1))
   call add(suite, MpiTestCase('testHaloFill', testHaloFill, numProcesses=2))
   call add(suite, MpiTestCase('testHaloFill', testHaloFill, numProcesses=5))

! Run the tests and accumulate the results in "result"
   result = newTestResult(mode=MODE_USE_STDOUT)
   call Run(suite, result)

   if (amRoot()) then
      summary_statement=Summary(result)
      print*,' '
      print*,trim(summary_statement)
   end if
  call pFUnit_finalize()

end program main
