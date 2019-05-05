!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_MpiParameterizedTestCase
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
module Test_MpiParameterizedTestCase
   use PF_Test
   use PF_TestCase
   use PF_MpiTestCase
   use PF_MpiTestParameter
   implicit none
   private

   public :: suite
   public :: newTest_MpiTestCase
   public :: Test_MpiTestCase

   type, extends(MpiTestCase) :: Test_MpiTestCase
      character(len=20), public :: runLog
      procedure(method), pointer :: testMethod => null()
   contains
      procedure :: runMethod
   end type Test_MpiTestCase

   type, extends(MpiTestParameter) :: ExtendedTestParameter
      integer :: i
   contains
      procedure :: toString
   end type ExtendedTestParameter

   abstract interface
      subroutine method(this)
        import Test_MpiTestCase
        class (Test_MpiTestCase), intent(inout) :: this
      end subroutine method
   end interface
   
contains

   function suite()
     use PF_TestSuite, only: TestSuite
      type (TestSuite) :: suite

      type (ExtendedTestParameter) :: testParameter

      suite = TestSuite('Test_MpiParameterizedTestCase')

      call testParameter%setNumProcessesRequested(2)

      testParameter%i = 1
!      call suite%addTest(newTest_MpiTestCase(REFLECT(testRunOn2PEs), testParameter))
      call suite%addTest(newTest_MpiTestCase('testRunOn2PEs', &
           &                                  testRunOn2PEs,  &
           &                                  testParameter))

      testParameter%i = 2
!      call suite%addTest(newTest_MpiTestCase(REFLECT(testToString), testParameter))
      call suite%addTest(newTest_MpiTestCase('testToString', &
           &                                  testToString,  &
           &                                  testParameter))
      
   end function suite

   function newTest_MpiTestCase(name, userMethod, testParameter) result(this)
      type(Test_MpiTestCase) :: this
      character(len=*), intent(in) :: name
      procedure(method) :: userMethod
      type (ExtendedTestParameter), intent(in) :: testParameter

      call this%setName(name)
      this%testMethod => userMethod
      call this%setTestParameter(testParameter)

    end function newTest_MpiTestCase

    function toString(this) result(string)
       class (ExtendedTestParameter), intent(in) :: this
       character(:), allocatable :: string

       allocate(character(1) :: string)
       write(string,'(i1)') this%i
    end function toString

   subroutine testRunOn2PEs(this)
      use PF_Assert, only: assertEqual
      class (Test_MpiTestCase), intent(inout) :: this

      call assertEqual(2, this%getNumProcesses())

   end subroutine testRunOn2PEs

   ! ensure that the extra parameter is correctly captured in the 
   ! testParameter component of the base class.
   subroutine testToString(this)
      use PF_Assert, only: assertEqual
      class (Test_MpiTestCase), intent(inout) :: this

      call assertEqual('2', this%testParameter%toString())

   end subroutine testToString

   recursive subroutine runMethod(this)
      class(Test_MpiTestCase), intent(inOut) :: this
      call this%testMethod()
   end subroutine runMethod


end module Test_MpiParameterizedTestCase
