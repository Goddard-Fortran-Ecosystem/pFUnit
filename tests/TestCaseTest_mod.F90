Module TestCaseTest_mod
  Use TestResult_mod
  Use TestCase_mod
  use pFUnitException_mod
  Implicit None
  Private

  public :: test_runTest
  public :: test_run2Tests
  public :: test_runSetUp
  public :: test_runSetUpMissing
  public :: test_runTearDown
  public :: test_runTearDownMissing
  public :: test_runSimpleTestCase_A
  public :: test_runSimpleTestCase_B
  public :: test_runSimpleTestCase_C
  public :: test_runSimpleTestCaseFail
  public :: test_runTestCaseFailSetUp
  public :: test_runTestCaseFailTearDown
  public :: test_runTestCaseSpoofFixture
  public :: test_countTests

  type testFixture_type
     private
     integer :: value
  end type testFixture_type

contains

   subroutine test_runTest()
      type (TestCase_type) :: test

      test = TestCase()
      call addTestMethod(test,'test_wasRun_A', wasRun_A)
      call run(test, 'test_wasRun_A')
      if (catch('Procedure wasRun_A was run.')) then
      else
         call throw(Exception('Failed to run procedure wasRun_A.'))
      end if

      call clean(test)

   end subroutine test_runTest

   subroutine test_run2Tests()
      type (TestCase_type) :: test

      test = TestCase()
      call addTestMethod(test,'test_wasRun_A', wasRun_A)
      call addTestMethod(test,'test_wasRun_B', wasRun_B)

      call run(test, 'test_wasRun_A')
      if (catch('Procedure wasRun_A was run.')) then
      else
         call throw(Exception('Failed to run procedure wasRun_A.'))
      end if

      call run(test, 'test_wasRun_B')
      if (catch('Procedure wasRun_B was run.')) then
      else
         call throw(Exception('Failed to run procedure wasRun_B.'))
      end if

      call clean(test)

   end subroutine test_run2Tests

   subroutine test_runSetUp()
      type (TestCase_type) :: test

      test = TestCase(setUp = privateSetUp, tearDown = privateTearDown)

      call runSetUp(test)
      if (catch('Procedure privateSetUp() was run.')) then
      else
         call throw(Exception('Failed to run procedure privateSetUp().'))
      end if

      call clean(test)

   end subroutine test_runSetUp

   subroutine test_runSetUpMissing()
      type (TestCase_type) :: test

      test = TestCase()

      call runSetUp(test)
      if (catch('No setUp() associated with this test case.')) then
      else
         call throw(Exception('Failed to report missing setUp() for test case.'))
      end if

      call clean(test)

   end subroutine test_runSetUpMissing

   subroutine test_runTearDown()
      type (TestCase_type) :: test

      test = TestCase(setUp = privateTearDown, tearDown = privateTearDown)

      call runTearDown(test)
      if (catch('Procedure privateTearDown() was run.')) then
      else
         call throw(Exception('Failed to run procedure privateTearDown().'))
      end if

      call clean(test)

   end subroutine test_runTearDown

   subroutine test_runTearDownMissing()
      type (TestCase_type) :: test

      test = TestCase()

      call runTearDown(test)
      if (catch('No tearDown() associated with this test case.')) then
      else
         call throw(Exception('Failed to report missing tearDown() for test case.'))
      end if

      call clean(test)

   end subroutine test_runTearDownMissing

   subroutine test_runSimpleTestCase_A()
      use Assert_mod
      type (TestCase_type) :: test
      type (TestResult_type)  :: aResult

      test = TestCase()
      aResult = newTestResult()

      call run(test, aResult)
      call assertEqual(0, numRun(aResult))
      call assertEqual(0, numFailed(aResult))

      call clean(aResult)
      call clean(test)

   end subroutine test_runSimpleTestCase_A

   subroutine test_runSimpleTestCase_B()
      use Assert_mod
      type (TestCase_type) :: test
      type (TestResult_type)  :: aResult

      test = TestCase()
      call addTestMethod(test,'testMethod', testMethod)
      aResult = newTestResult()

      call run(test, aResult)
      call assertEqual(1, numRun(aResult))
      call assertEqual(0, numFailed(aResult))

      call clean(aResult)
      call clean(test)
      
   end subroutine test_runSimpleTestCase_B

   subroutine test_runSimpleTestCase_C()
      use Assert_mod
      type (TestCase_type) :: test
      type (TestResult_type)  :: aResult

      test = TestCase()
      call addTestMethod(test,'testMethod', testMethod)
      call addTestMethod(test,'testMethod', testMethod)
      aResult = newTestResult()

      call run(test, aResult)
      call assertEqual(2, numRun(aResult))
      call assertEqual(0, numFailed(aResult))

      call clean(aResult)
      call clean(test)
      
   end subroutine test_runSimpleTestCase_C

   subroutine test_runTestCaseFailSetUp()
      use Assert_mod
      use Report_mod
      type (TestCase_type) :: test
      type (TestResult_type)  :: aResult

      type (Report_type) :: rprt

      test = TestCase(setUp = testMethodFail, tearDown = testMethod)
      call addTestMethod(test,'testMethod', testMethod)
      aResult = newTestResult()

      call run(test, aResult)
      call assertEqual(1, numRun(aResult))
      call assertEqual(1, numFailed(aResult))

      rprt= GenerateReport(aResult)
      Call AssertEqual("2 failures in testMethod:", getLine(rprt,1))
      Call AssertEqual("- forced exception", getLine(rprt,2), ignoreWhiteSpace = .true.)
      Call AssertEqual("- (setUp() failed)", getLine(rprt,3), ignoreWhiteSpace = .true.)

      call clean(rprt)
      call clean(test)
      call clean(aResult)
      
   end subroutine test_runTestCaseFailSetUp

   subroutine test_runTestCaseFailTearDown()
      use Assert_mod
      use Report_mod
      type (TestCase_type) :: test
      type (TestResult_type)  :: aResult

      type (Report_type) :: rprt

      test = TestCase(setUp = testMethod, tearDown = testMethodFail)
      call addTestMethod(test,'testMethod', testMethod)
      aResult = newTestResult()

      call run(test, aResult)
      call assertEqual(1, numFailed(aResult))
      call assertEqual(2, numSevere(aResult))

      rprt= GenerateReport(aResult)
      Call AssertEqual("2 failures in testMethod:", getLine(rprt,1))
      Call AssertEqual("- forced exception", getLine(rprt,2), ignoreWhiteSpace = .true.)
      Call AssertEqual("- (tearDown() failed)", getLine(rprt,3), ignoreWhiteSpace = .true.)

      call clean(rprt)
      call clean(test)
      call clean(aResult)

   end subroutine test_runTestCaseFailTearDown

   subroutine test_runSimpleTestCaseFail()
      use Assert_mod
      use Report_mod
      type (TestCase_type) :: test
      type (TestResult_type)  :: aResult

      type (Report_type) :: rprt

      test = TestCase()
      call addTestMethod(test,'testMethodFail', testMethodFail)
      aResult = newTestResult()

      call run(test, aResult)
      call AssertEqual(1, numRun(aResult))
      call AssertEqual(1, numFailed(aResult))

      rprt= GenerateReport(aResult)
      Call AssertEqual('Failure in testMethodFail - forced exception', getLine(rprt, 1))

      call clean(rprt)
      call clean(test)
      call clean(aResult)

   end subroutine test_runSimpleTestCaseFail

   subroutine test_runTestCaseSpoofFixture()
      use Assert_mod
      type (TestCase_type) :: test
      type (TestResult_type)  :: aResult

      aResult = newTestResult()

      test = TestCase(setUp = setUpFixture, tearDown = tearDownFixture, passFixture=.true.)
      call addTestMethod(test, 'testMethodFixture', testMethodFixture)
      call addTestMethod(test, 'testMethodFixtureFail', testMethodFixtureFail)
      call run(test, aResult)
      call AssertEqual(2, numRun(aResult))
      call AssertEqual(1, numFailed(aResult))
      call AssertEqual(1, numSevere(aResult))

      call clean(aResult)
      call clean(test)

   end subroutine test_runTestCaseSpoofFixture

   subroutine test_CountTests()
      use Assert_mod
      type (TestCase_type)       :: test

      test = TestCase(setUp = testMethod, tearDown = testMethodFail)
      call assertEqual(0, countTests(test))

      call addTestMethod(test,'testMethod', testMethod)
      call assertEqual(1, countTests(test))

      call addTestMethod(test,'testMethod', testMethod)
      call assertEqual(2, countTests(test))

      call clean(test)

   end subroutine test_CountTests
      
! The remaining procedures are procedures used as probes in the actual tests above.
   subroutine wasRun_A()
      call throw(Exception('Procedure wasRun_A was run.'))
   end subroutine wasRun_A

   subroutine wasRun_B()
      call throw(Exception('Procedure wasRun_B was run.'))
   end subroutine wasRun_B

   subroutine privateSetUp()
      call throw(Exception('Procedure privateSetUp() was run.'))
   end subroutine privateSetUp

   subroutine privateTearDown()
      call throw(Exception('Procedure privateTearDown() was run.'))
   end subroutine privateTearDown

   subroutine testMethod()
   end subroutine testMethod

   subroutine testMethodFail()
      call throw(Exception('forced exception'))
   end subroutine testMethodFail

   subroutine setUpFixture(this)
      type (testFixture_type) :: this

      this%value = 0
   end subroutine setUpFixture

   subroutine tearDownFixture(this)
      use Assert_mod
      type (testFixture_type) :: this
      this%value = this%value + 1
      call assertEqual(2, this%value)
   end subroutine tearDownFixture

   subroutine testMethodFixture(this)
      use Assert_mod
      type (testFixture_type) :: this
      this%value = this%value + 1
      call assertEqual(1, this%value)
   end subroutine testMethodFixture

   subroutine testMethodFixtureFail(this)
      use Assert_mod
      type (testFixture_type) :: this
      this%value = this%value + 1
      call assertEqual(2, this%value)
   end subroutine testMethodFixtureFail

End Module TestCaseTest_mod
