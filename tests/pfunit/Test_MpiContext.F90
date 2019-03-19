!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_MpiContext
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
module Test_MpiContext
   use PF_ParallelContext
   use PF_TestCase
   use PF_MpiTestCase
   use PF_MpiTestMethod
   use PF_Assert
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite, only: TestSuite

      type (TestSuite) :: suite

      suite = TestSuite('MpiContext')
      call suite%addTest(MpiTestMethod('testNumProcesses1', userMethod=testNumProcesses1,  numProcesses = 1))
      call suite%addTest(MpiTestMethod('testNumProcesses3', userMethod=testNumProcesses3,  numProcesses = 3))

      call suite%addTest(MpiTestMethod('testAllReduce_none', userMethod=testAllReduce_none,  numProcesses = 1))
      call suite%addTest(MpiTestMethod('testAllReduce_none', userMethod=testAllReduce_none,  numProcesses = 2))
      call suite%addTest(MpiTestMethod('testAllReduce_none', userMethod=testAllReduce_none,  numProcesses = 3))

      call suite%addTest(MpiTestMethod('testAllReduce_some', userMethod=testAllReduce_some,  numProcesses = 2))
      call suite%addTest(MpiTestMethod('testAllReduce_some', userMethod=testAllReduce_some,  numProcesses = 3))

   end function suite

   subroutine testNumProcesses1(context)
      class (MpiTestMethod), intent(inout) :: context
      call assertEqual(1, context%getNumProcesses())
   end subroutine testNumProcesses1

   subroutine testNumProcesses3(context)
      class (MpiTestMethod), intent(inout) :: context
      call assertEqual(3, context%getNumProcesses())
   end subroutine testNumProcesses3

   subroutine testAllReduce_none(context)
      class (MpiTestMethod), intent(inout) :: context

      logical :: qIn
      logical :: qOut

      qIn = .false.
      qOut = context%context%allReduce(qIn)

      call assertFalse(qOut)
   end subroutine testAllReduce_none

   subroutine testAllReduce_some(context)
      class (MpiTestMethod), intent(inout) :: context

      logical :: qIn
      logical :: qOut

      qIn = .false.
      if (context%getProcessRank() == 1) qIn = .true.

      qOut = context%context%allReduce(qIn)

      call assertTrue(qOut)
   end subroutine testAllReduce_some

end module Test_MpiContext


