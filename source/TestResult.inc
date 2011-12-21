public :: TestResult
public :: newTestResult

type TestResult
  integer :: numFailed
  integer :: numRun
  type (ListenerPointer), allocatable :: listeners(:)
  type (TestFailure), allocatable :: failures(:)
contains
   procedure :: addFailure
   procedure :: failureCount
   procedure :: getIthFailure
   procedure :: startTest
   procedure :: endTest
   procedure :: runCount
   procedure :: run => run_TestResult
   procedure :: addListener
   procedure :: wasSuccessful

end type TestResult
