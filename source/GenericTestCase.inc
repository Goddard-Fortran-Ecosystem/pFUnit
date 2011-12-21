!Use Test
public :: GenericTestCase

type, abstract, extends(Test) :: GenericTestCase
contains
   procedure :: countTestCases => countTestCases_TestCase
   procedure :: runBare
!!$   procedure(I_suite), deferred :: suite
   procedure :: setUp
   procedure :: tearDown
   procedure(I_method), deferred :: runTestMethod
   procedure (getNumProcessesMethod), deferred :: getNumProcesses
  
end type GenericTestCase
   
