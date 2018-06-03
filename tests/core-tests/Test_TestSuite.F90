!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_TestSuite_mod
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 21 Mar 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 21 Mar 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module Test_TestSuite_mod
   use PF_TestSuite_mod, only: TestSuite
   use PF_TestResult_mod
   implicit none
   private

   public :: suite

   character(len=80) :: log

   ! Internal mock for TestResult
   type, extends(TestResult) :: Verbose
      character(len=80) :: log
   contains
      procedure :: run
   end type Verbose


contains

   function suite()
      use PF_TestMethod_mod, only: TestMethod
      use PF_TestSuite_mod, only: TestSuite, TestSuite
      type (TestSuite) :: suite

      suite = TestSuite('TestSuiteSuite')

!#define ADD(method) call suite%addTest(TestMethod(REFLECT(method)))

      call suite%addTest( &
           &   TestMethod('testCountTestCases', &
           &                  testCountTestCases))
      call suite%addTest( &
           &   TestMethod('testCountTestCasesNestedA', &
           &                  testCountTestCasesNestedA))
      call suite%addTest( &
           &   TestMethod('testCountTestCasesNestedB', &
           &                  testCountTestCasesNestedB))
      call suite%addTest( &
           &   TestMethod('testCountTestCasesNestedC', &
           &                  testCountTestCasesNestedC))
      call suite%addTest( &
           &   TestMethod('testGetTestCases', &
           &                  testGetTestCases))

   end function suite

   subroutine testCountTestCases()
      use PF_TestSuite_mod, only: TestSuite, TestSuite
      use SimpleTestCase_mod, only: newSimpleTestCase
      use SimpleTestCase_mod, only: method1, method2
      use PF_TestSuite_mod, only: TestSuite, TestSuite
      use PF_Assert_mod, only: assertEqual
      type (TestSuite) :: suite

      suite = TestSuite('aSuite')
      call assertEqual(0, suite%countTestCases(),'a')
      call suite%addTest(newSimpleTestCase('method1', method1))
      call assertEqual(1, suite%countTestCases(),'b')
      call suite%addTest(newSimpleTestCase('method2', method2))
      call assertEqual(2, suite%countTestCases(),'c')

   end subroutine testCountTestCases

   subroutine testCountTestCasesNestedA()
      use PF_TestSuite_mod, only: TestSuite, TestSuite
      use PF_Assert_mod, only: assertEqual

      type (TestSuite) :: innerSuite
      type (TestSuite) :: outerSuite

      innerSuite = TestSuite('inner')
      outerSuite = TestSuite('outer')
      call outerSuite%addTest(innerSuite)
      call assertEqual(0, outerSuite%countTestCases())

   end subroutine testCountTestCasesNestedA

   subroutine testCountTestCasesNestedB()
      use PF_TestSuite_mod, only: TestSuite, TestSuite
      use SimpleTestCase_mod, only: SimpleTestCase
      use PF_Assert_mod, only: assertEqual
      type (TestSuite) :: innerSuite
      type (TestSuite) :: outerSuite

      type (SimpleTestCase) :: aTest

      call aTest%setName('aTest')
      innerSuite = TestSuite('inner')
      outerSuite = TestSuite('outer')
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
      use PF_TestSuite_mod, only: TestSuite, TestSuite
      use SimpleTestCase_mod, only: SimpleTestCase
      use PF_Assert_mod, only: assertEqual
      type (TestSuite) :: suiteA, suiteB, suiteC, topSuite
      type (SimpleTestCase) :: aTest

      call aTest%setName('aTest')
      topSuite = TestSuite('top')
      suiteA = TestSuite('A')
      suiteB = TestSuite('B')
      suiteC = TestSuite('C')

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

   subroutine testGetTestCases()
      use PF_Test_mod
      use PF_TestVector_mod
      use PF_TestCase_mod
      use PF_TestMethod_mod
      use PF_SerialContext_mod
      use PF_Assert_mod

      type (TestSuite) :: top
      type (TestSuite) :: childA, childB
      type (Verbose) :: aResult
      type (TestVector) :: testCases
      class (Test), pointer :: t
      integer :: i

      childA = TestSuite('childA')
      call childA%addTest(TestMethod('a1', myTestMethod))
      call childA%addTest(TestMethod('a2', myTestMethod))
      call childA%addTest(TestMethod('a3', myTestMethod))

      childB = TestSuite('childB')
      call childB%addTest(TestMethod('b1', myTestMethod))
      call childB%addTest(TestMethod('b2', myTestMethod))

      top = TestSuite('top')
      call top%addTest(childA)
      call top%addTest(childB)

      aResult%TestResult = TestResult()
      aResult%log = ''

      call top%getTestCases(testCases)

      do i = 1, testCases%size()
         t => testCases%at(i)
         call t%run(aResult, SerialContext())
      end do

      call assertEqual('::childA.a1::childA.a2::childA.a3::childB.b1::childB.b2', aResult%log)

   end subroutine testGetTestCases

   subroutine myTestMethod()
   end subroutine myTestMethod

   recursive subroutine run(this, test, context)
      use PF_TestCase_mod
      use PF_SurrogateTestCase_mod
      use PF_ParallelContext_mod
      class (Verbose), intent(inout) :: this
      class (SurrogateTestCase), intent(inout) :: test
      class (ParallelContext), intent(in) :: context

      this%log = trim(this%log)//'::'//trim(test%getName())

   end subroutine run

end module Test_TestSuite_mod
