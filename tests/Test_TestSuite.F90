#include "reflection.h"
module Test_TestSuite_mod
   use TestSuite_mod, only: newTestSuite, TestSuite
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestCase_mod, only: TestCase
      use SimpleTestMethod_mod, only: newSimpleTestMethod
      use TestSuite_mod, only: newTestSuite, TestSuite
      type (TestSuite), pointer :: suite

      allocate(suite)
      suite => newTestSuite('TestSuiteSuite')

#define ADD(method) call suite%addTest(newSimpleTestMethod(REFLECT(method)))

      ADD(testCountTestCases)
      ADD(testCountTestCasesNestedA)
      ADD(testCountTestCasesNestedB)
      ADD(testCountTestCasesNestedC)

   end function suite

   subroutine testCountTestCases()
      use TestSuite_mod, only: newTestSuite, TestSuite
      use SimpleTestCase_mod, only: newSimpleTestCase
      use SimpleTestCase_mod, only: method1, method2
      use TestSuite_mod, only: newTestSuite, TestSuite
      use Assert_mod, only: assertEqual
      type (TestSuite), pointer :: suite

      suite => newTestSuite('aSuite')
      call assertEqual(0, suite%countTestCases())
      call suite%addTest(newSimpleTestCase(method1, 'method1'))
      call assertEqual(1, suite%countTestCases())
      call suite%addTest(newSimpleTestCase(method2, 'method2'))
      call assertEqual(2, suite%countTestCases())

   end subroutine testCountTestCases

   subroutine testCountTestCasesNestedA()
      use TestSuite_mod, only: newTestSuite, TestSuite
      use Assert_mod, only: assertEqual

      type (TestSuite) :: innerSuite
      type (TestSuite) :: outerSuite

      innerSuite = newTestSuite('inner')
      outerSuite = newTestSuite('outer')
      call outerSuite%addTest(innerSuite)
      call assertEqual(0, outerSuite%countTestCases())

   end subroutine testCountTestCasesNestedA

   subroutine testCountTestCasesNestedB()
      use TestSuite_mod, only: newTestSuite, TestSuite
      use SimpleTestCase_mod, only: SimpleTestCase
      use Assert_mod, only: assertEqual
      type (TestSuite) :: innerSuite
      type (TestSuite) :: outerSuite

      type (SimpleTestCase) :: aTest

      innerSuite = newTestSuite('inner')
      outerSuite = newTestSuite('outer')
      call innerSuite%addTest(aTest)
      call outerSuite%addTest(innerSuite)
      call assertEqual(1, outerSuite%countTestCases())
      call assertEqual(1, innerSuite%countTestCases())

   end subroutine testCountTestCasesNestedB


   !
   ! Complex Suite nested structure:
   !   topSuite
   !      ->  suiteA
   !          -> Test1
   !          -> suiteC
   !             -> Test1
   !             -> Test2
   !      ->  suiteB
   !          -> Test1
   !          -> Test2
   !
   subroutine testCountTestCasesNestedC()
      use TestSuite_mod, only: newTestSuite, TestSuite
      use SimpleTestCase_mod, only: SimpleTestCase
      use Assert_mod, only: assertEqual
      type (TestSuite) :: suiteA, suiteB, suiteC, topSuite
      type (SimpleTestCase) :: aTest

      topSuite = newTestSuite('top')
      suiteA = newTestSuite('A')
      suiteB = newTestSuite('B')
      suiteC = newTestSuite('C')

      call suiteC%addTest(aTest)
      call suiteC%addTest(aTest)

      call suiteB%addTest(aTest)
      call suiteB%addTest(aTest)

      call suiteA%addTest(aTest)
      call suiteA%addTest(suiteC)

      call topSuite%addTest(suiteA)
      call topSuite%addTest(suiteB)

      call assertEqual(2+2+1, topSuite%countTestCases())

   end subroutine testCountTestCasesNestedC

end module Test_TestSuite_mod
