!Use Test
public :: MpiTestCase

type, abstract, extends(Test) :: MpiTestCase
contains
   procedure :: countTestCases => countTestCases_MpiTestCase
   procedure :: run => run_TestCaseMpi
   procedure :: runBare => runBareMpi
!!$   procedure(I_suite), deferred :: suite
   procedure :: setUp => setUpMpi
   procedure :: tearDown => tearDownMpi
   procedure(I_methodMpi), deferred :: runTestMethod
  
end type MpiTestCase
   
