!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_MockCall
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

module Test_MockCall
   use PF_TestSuite
   use PF_MockCall
   implicit none
   private

   public :: suite

contains

!#define ADD(method) call suite%addTest(TestMethod(REFLECT(method)))

   function suite()
      use PF_TestSuite, only: TestSuite
      use PF_TestMethod, only: TestMethod
      type (TestSuite) :: suite

      suite = TestSuite('Test_MockCall')

      call suite%addTest( &
           &   TestMethod('testExpectOneIntegerArgument', &
           &                  testExpectOneIntegerArgument))
      call suite%addTest( &
           &   TestMethod('testFailExpectOneIntegerArgument', &
           &                  testFailExpectOneIntegerArgument))

   end function suite

   subroutine testExpectOneIntegerArgument
      use PF_Assert
      type (MockCall) :: mCall
      class (*), pointer :: q
      integer, target :: one = 1

      mCall = newMockCall('methodName')
      call mCall%expect(one)
      q => mCall%getExpectedValue()

      select type (p => q)
      type is (integer)
         call assertEqual(p, 1)
      end select

   end subroutine testExpectOneIntegerArgument

   subroutine testFailExpectOneIntegerArgument
      use PF_Assert
      type (MockCall) :: mCall
      class (*), pointer :: q

      integer, target :: one

      one = 1

      mCall = newMockCall('methodName')
      call mCall%expect(one)
      q => mCall%getExpectedValue()
      select type (p => q)
      type is (integer)
         call assertEqual(p, 2)
      end select
      call assertExceptionRaised()
   end subroutine testFailExpectOneIntegerArgument

end module Test_MockCall
