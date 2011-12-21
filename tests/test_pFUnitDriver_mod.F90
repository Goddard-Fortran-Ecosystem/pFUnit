module test_pFUnitDriver_mod
   use pfunit
   implicit none
   private

   public :: test_loadDsoTests
   public :: test_runDsoTests
   public :: test_loadTestsFromFile1
   public :: test_loadTestsFromFile2

contains

   subroutine test_loadDsoTests()
      use SharedObjLibUtilities_mod
      type (SharedObjLibUtilities_type) :: dsoHandle
      type (TestSuite_type) :: suite

      character(len=MAX_SYMBOL_LENGTH) :: testListA(1)
      character(len=MAX_SYMBOL_LENGTH) :: testListB(2)

      dsoHandle = openLibrary(libraryName = 'tests/testData/libpFUnitTestsA.so')
      if (catch(preserve=.true.)) return
      suite = TestSuite('top')

      testListA(1) = 'test_succeed'
      testListB(1) = 'test_succeed'
      testListB(2) = 'test_fail'

      call loadDsoTests(suite, dsoHandle, testNames = testListA)
      call assertEqual(1, countTests(suite), 'first count')

      call loadDsoTests(suite, dsoHandle, testNames = testListB)
      call assertEqual(3, countTests(suite), 'second count')

      call closeLibrary(dsoHandle)
      call clean(suite)

   end subroutine test_loadDsoTests

   subroutine test_runDsoTests()
      type (TestResult_type) :: testResult
      
      testResult = newTestResult()
      call runDsoTests(libraryName = 'tests/testData/libpFUnitTestsA.so', &
           & pattern = 'test_', &
           & testResult = testResult)
      if (catch(preserve=.true.)) return
      call assertEqual(2, numRun(testResult), 'number run')
      call assertEqual(1, numFailed(testResult), 'number failed')
      call clean(testResult)
      
   end subroutine test_runDsoTests

   subroutine test_loadTestsFromFile1()
      use SharedObjLibUtilities_mod
      type (TestSuite_type) :: suite
      type (TestResult_type) :: testResult
      type (SharedObjLibUtilities_type) :: dsoHandle
      
      suite = TestSuite('top')
      
      testResult = newTestResult()
      dsoHandle = openLibrary(libraryName = 'tests/testData/libpFUnitTestsA.so')
      call loadDsoTests(suite, dsoHandle, 'tests/testData/list1')
      call run(suite, testResult)

      call assertEqual(1, numRun(testResult), 'number run')
      call assertEqual(0, numFailed(testResult), 'number failed')

      call clean(testResult)
      call clean(suite)
      call closeLibrary(dsoHandle)

   end subroutine test_loadTestsFromFile1
   
   subroutine test_loadTestsFromFile2()
      use SharedObjLibUtilities_mod
      type (TestSuite_type) :: suite
      type (TestResult_type) :: testResult
      type (SharedObjLibUtilities_type) :: dsoHandle

      testResult = newTestResult()
      suite = TestSuite('top')
      dsoHandle = openLibrary(libraryName = 'tests/testData/libpFUnitTestsA.so')
      call loadDsoTests(suite, dsoHandle, 'tests/testData/list2')
      call run(suite, testResult)

      call assertEqual(2, numRun(testResult), 'number run')
      call assertEqual(1, numFailed(testResult), 'number failed')

      call clean(testResult)
      call clean(suite)
      call closeLibrary(dsoHandle)

   end subroutine test_loadTestsFromFile2
   
end module test_pFUnitDriver_mod
