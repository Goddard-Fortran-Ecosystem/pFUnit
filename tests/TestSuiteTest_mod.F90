#include "reflection.h"
Module TestSuiteTest_mod
  Use pFUnit
  Implicit None
  Private

  Public :: testAddSuite
  Public :: testRunSuite
  Public :: testReportLineage
  Public :: testCountTestsA
  Public :: testCountTestsB
  Public :: testCountTestsC
#ifdef USE_MPI
  Public :: testCountTestsD
#endif

Contains

  Subroutine testRunSuite()
    Use TestWasRun_mod
    Type (TestSuite_type) :: suite
    Type (TestResult_type) :: test_result

    suite = TestSuite('suite')
    test_result=NewTestResult()

    call Add(suite, TestCase1StepFixture(setup, teardown, REFLECT(testMethod)))
    call Add(suite, TestCase1StepFixture(setup, teardown, REFLECT(brokenMethod)))

    Call Run(suite, test_result)
    call assertEqual(2, numRun(test_result),'numRun')
    call assertEqual(1, numFailed(test_result),'numFailed')
    call assertEqual(1, numSevere(test_result),'numSevere')

    call clean(test_result)
    call clean(suite)

  End Subroutine testRunSuite

  Subroutine testAddSuite()
    Use TestWasRun_mod
    Type (TestResult_type) :: test_result
    Type (TestSuite_type) :: suite_A, suite_B

    test_result=NewTestResult()
    suite_A = TestSuite('suite')

    call Add(suite_A, TestCase1StepFixture(setup, teardown, REFLECT(testMethod)))
    call Add(suite_A, TestCase1StepFixture(setup, teardown, REFLECT(brokenMethod)))

    suite_B = TestSuite('suiteB')
    call Add(suite_B, TestCase1StepFixture(setup, teardown, REFLECT(testMethod)))
    call Add(suite_B, suite_A)

    Call Run(suite_B, test_result)

    call assertEqual(3, numRun(test_result))
    call assertEqual(1, numFailed(test_result))
    call assertEqual(1, numSevere(test_result))

    call clean(suite_B)
    call clean(test_result)

  End Subroutine testAddSuite

  Subroutine testReportLineage()
    Use TestWasRun_mod
    Type (TestResult_type) :: test_result
    Type (TestSuite_type) :: suite_A, suite_B
    type (Report_type) :: rprt

    test_result=NewTestResult()
    suite_A = TestSuite('suite')

    call Add(suite_A, TestCase1StepFixture(setup, teardown, REFLECT(testMethod)))
    call Add(suite_A, TestCase1StepFixture(setup, teardown, REFLECT(BrokenMethod)))

    suite_B = TestSuite('suiteB')
    call Add(suite_B, TestCase1StepFixture(setup, teardown, REFLECT(testMethod)))
    call Add(suite_B, suite_A)

    Call Run(suite_B, test_result)

    rprt = GenerateReport(test_result)
    Call AssertEqual("Failure in suiteB::suite::BrokenMethod - Broken method", getLine(rprt,1))

    call clean(rprt)
    call clean(suite_B)
    call clean(test_result)

  End Subroutine testReportLineage

  subroutine testCountTestsA()
     Use TestWasRun_mod
     type (TestSuite_type) :: suite
     type (TestCase_type) :: test
     
     suite = TestSuite('suite')
     call assertEqual(0, countTests(suite))
     
     call add(suite, testCase1Step(REFLECT(testMethod)))
     call assertEqual(1, countTests(suite))
     
     test = testCase1Step(REFLECT(testMethod))
     call addTestMethod(test, REFLECT(testMethod))
     call add(suite, test)
     call assertEqual(3, countTests(suite))
     
     call clean(suite)
     
  end subroutine testCountTestsA
  
  subroutine testCountTestsB()
     Use TestWasRun_mod
     type (TestSuite_type) :: suite_A
     type (TestSuite_type) :: suite_B
     
     suite_A = TestSuite('suiteA')
     suite_B = TestSuite('suiteB')
     
     call add(suite_A, testCase1Step(REFLECT(testMethod)))
     call add(suite_A, testCase1Step(REFLECT(testMethod)))
     
     call add(suite_B, suite_A)
     call assertEqual(2, countTests(suite_B))
     
     call clean(suite_B)
     
  end subroutine testCountTestsB
  
  subroutine testCountTestsC()
     use TestParamType_mod
     type (TestSuite_type) :: suite
     type (ParameterizedTestCase_type) :: testA, testB
     type (Sequence_type) :: sequence
     
     external BaseAddress
     type (BaseAddress_type) :: BaseAddress
     type(BaseAddress_type), Pointer :: wrap_params(:) => null()
     
     suite = TestSuite('suite')
     testA = newParameterizedTestCase(BaseAddress(sequence), REFLECT(testmethod), setup, teardown)
     testB = newParameterizedTestCase(BaseAddress(sequence), REFLECT(testmethod), setup, teardown)
     
     allocate(wrap_params(5))
     call setParams(testA, wrap_params )
     call setParams(testB, wrap_params(1:3) )
     
     call add(suite, testA)
     call assertEqual(5, countTests(suite))
     
     call add(suite, testB)
     call assertEqual(8, countTests(suite))
     
     deallocate(wrap_params)
     call clean(suite)
     
  end subroutine testCountTestsC

#ifdef USE_MPI  
  subroutine testCountTestsD()
     Use TestWasRun_mod
     use Test_MPI_WasRun_mod

     type (TestSuite_type) :: suite

     type (MpiTestCase_type) :: testA, testB

     suite = TestSuite('suite')
     
     testA = MpiTestCase(REFLECT(HelloWorld), numProcesses=4)
     call add(suite, testA)
     call assertEqual(1, countTests(suite))
     
     testB = MpiTestCase(REFLECT(HelloWorld), numProcesses=3)
     call add(suite, testB)
     call assertEqual(2, countTests(suite))
     
     call clean(suite)

  end subroutine testCountTestsD
#endif

End Module TestSuiteTest_mod
