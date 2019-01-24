!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_Assert
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 20 Mar 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 20 Mar 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module Test_Assert
   use PF_TestSuite
   use PF_Assert
   use PF_ExceptionList, only: catch
   implicit none
   private

   public :: suite 

contains

   function suite() result(aSuite)
      use PF_Test
      use PF_TestMethod
      use PF_TestSuite
      type (TestSuite) :: aSuite

      aSuite = TestSuite('Assert')

!#define ADD(method) call aSuite%addTest(newTestMethod(REFLECT(method)))

      call aSuite%addTest( &
           &   TestMethod('testAssertEqualStringDiffer1st', &
           &                  testAssertEqualStringDiffer1st))

      call aSuite%addTest( &
           &   TestMethod('testAssertWithLocation', &
           &                  testAssertWithLocation))
   end function suite


   subroutine testAssertEqualStringDiffer1st()
      call assertEqual(expected="a string A", found="string B")
      call assertTrue(catch('String assertion failed:' // new_line('A') // &
           & '    expected: <"a string A">' // new_line('A') // &
           & '   but found: <"string B">' // new_line('A') // &
           & '  first diff:   ^'))
   end subroutine testAssertEqualStringDiffer1st

   subroutine testAssertWithLocation
      use PF_SourceLocation
      call assertTrue(.false., 'intentional fail', &
           & SourceLocation(fileName='nowhere', lineNumber=5))
      call assertTrue(catch('intentional fail'))
   end subroutine testAssertWithLocation

end module Test_Assert
