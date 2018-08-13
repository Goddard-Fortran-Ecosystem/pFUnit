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

   ! First test
   !@test(npes = [1,2])
   subroutine testA(this)
      class (MpiTestCaseB), intent(inout) :: this
   end subroutine testA

   ! Second test
   !@test
   subroutine testB(this)
      class (MpiTestCaseB), intent(inout) :: this
   end subroutine testB

end module MpiTestCaseB_mod



module WrapMpiTestCaseB_mod
   use pFUnit_mod
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
#ifdef INTEL_13
      use pfunit_mod, only: testCase
#endif
      type (WrapUserTestCase) :: aTest
#ifdef INTEL_13
      target :: aTest
      class (WrapUserTestCase), pointer :: p
#endif
      character(len=*), intent(in) :: methodName
      procedure(userTestMethod) :: testMethod
      type (MpiTestParameter), intent(in) :: testParameter
      aTest%testMethodPtr => testMethod
#ifdef INTEL_13
      p => aTest
      call p%setName(methodName)
#else
      call aTest%setName(methodName)
#endif
      call aTest%setTestParameter(testParameter)
   end function makeCustomTest

end module WrapMpiTestCaseB_mod

function MpiTestCaseB_mod_suite() result(suite)
   use pFUnit_mod
   use MpiTestCaseB_mod
   use WrapMpiTestCaseB_mod
   type (TestSuite) :: suite

   type (MpiTestParameter), allocatable :: testParameters(:)
   type (MpiTestParameter) :: testParameter
   integer :: iParam 
   integer, allocatable :: cases(:) 
 
   suite = newTestSuite('MpiTestCaseB_mod_suite')

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

