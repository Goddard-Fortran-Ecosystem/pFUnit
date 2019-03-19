module MpiTestCaseB_mod
   use pfunit_mod
   implicit none

   
!@testCase(npes = [1,3,5])
   type, extends(MpiTestCase) :: MpiTestCaseB
      integer :: componentI
   contains
      procedure :: setUp
      procedure :: tearDown
   end type MpiTestCaseB

contains

   subroutine setUp(this)
      class (MpiTestCaseB), intent(inout) :: this
      this%componentI = 5
   end subroutine setUp

   subroutine tearDown(this)
      class (MpiTestCaseB), intent(inout) :: this
      this%componentI = -1
   end subroutine tearDown

   !@test(npes = [1,2])
   ! First test
   subroutine testA(this)
      class (MpiTestCaseB), intent(inout) :: this
   end subroutine testA

   !@test
   ! Second test
   subroutine testB(this)
      class (MpiTestCaseB), intent(inout) :: this
   end subroutine testB

end module MpiTestCaseB_mod



module WrapMpiTestCaseB_mod
   use FUnit
   use MpiTestCaseB_mod
   implicit none
   private

   public :: WrapUserTestCase
   public :: makeCustomTest
   type, extends(MpiTestCaseB) :: WrapUserTestCase
      procedure(userTestMethod), nopass, pointer :: testMethodPtr
   contains
      procedure :: runMethod
   end type WrapUserTestCase

   abstract interface
     subroutine userTestMethod(this)
        use MpiTestCaseB_mod
        class (MpiTestCaseB), intent(inout) :: this
     end subroutine userTestMethod
   end interface

contains

   subroutine runMethod(this)
      class (WrapUserTestCase), intent(inout) :: this

      call this%testMethodPtr(this)
   end subroutine runMethod

   function makeCustomTest(methodName, testMethod, testParameter) result(aTest)
      type (WrapUserTestCase) :: aTest
      character(len=*), intent(in) :: methodName
      procedure(userTestMethod) :: testMethod
      type (MpiTestParameter), intent(in) :: testParameter
      aTest%testMethodPtr => testMethod
      call aTest%setName(methodName)
      call aTest%setTestParameter(testParameter)
   end function makeCustomTest

end module WrapMpiTestCaseB_mod

function MpiTestCaseB_mod_suite() result(suite)
   use FUnit
   use MpiTestCaseB_mod
   use WrapMpiTestCaseB_mod
   implicit none
   type (TestSuite) :: suite

   class (Test), allocatable :: t

   type (MpiTestParameter), allocatable :: testParameters(:)
   type (MpiTestParameter) :: testParameter
   integer :: iParam 
   integer, allocatable :: cases(:) 
 
   suite = TestSuite('MpiTestCaseB_mod_suite')

   call testParameter%setNumProcessesRequested(1)
   call suite%addTest(makeCustomTest('testA', testA, testParameter))
   call testParameter%setNumProcessesRequested(2)
   call suite%addTest(makeCustomTest('testA', testA, testParameter))

   call testParameter%setNumProcessesRequested(1)
   call suite%addTest(makeCustomTest('testB', testB, testParameter))
   call testParameter%setNumProcessesRequested(3)
   call suite%addTest(makeCustomTest('testB', testB, testParameter))
   call testParameter%setNumProcessesRequested(5)
   call suite%addTest(makeCustomTest('testB', testB, testParameter))


end function MpiTestCaseB_mod_suite

