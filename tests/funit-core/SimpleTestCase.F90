#include "unused_dummy.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: pf_SimpleTestCase
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
module pf_SimpleTestCase
   use PF_TestCase, only: TestCase
   implicit none
   private

   public :: suite
   public :: newSimpleTestCase
   public :: SimpleTestCase
   public :: method1, method2
   public :: methodWith2Exceptions

   type, extends(TestCase) :: SimpleTestCase
      character(len=20), public :: runLog
      procedure(method), pointer :: testMethod => null()
   contains
      procedure :: runMethod
   end type SimpleTestCase

   abstract interface
      subroutine method(this)
        use PF_Test
        import SimpleTestCase
        class (SimpleTestCase), intent(inOut) :: this
      end subroutine method
   end interface

contains

   function suite()
     use PF_TestSuite, only: TestSuite
      type (TestSuite) :: suite

      suite = TestSuite('SimpleTestCase')

!#define ADD(method) call suite%addTest(newSimpleTestCase(REFLECT(method)))

      call suite%addTest( &
           &   newSimpleTestCase('method1', &
           &                      method1))
      call suite%addTest( &
           &   newSimpleTestCase('method2', &
           &                      method2))
      call suite%addTest( &
           &   newSimpleTestCase('methodWith2Exceptions', &
           &                      methodWith2Exceptions))
      
   end function suite

   function newSimpleTestCase(name, userMethod) result(this)
      type(SimpleTestCase) :: this
      character(len=*), intent(in) :: name
      procedure(method) :: userMethod

      this%testMethod => userMethod
      call this%setName(name)

    end function newSimpleTestCase

   recursive subroutine runMethod(this)
      class(SimpleTestCase), intent(inOut) :: this
      call this%testMethod()
   end subroutine runMethod

   subroutine method1(this)
      class (SimpleTestCase), intent(inOut) :: this
      this%runLog = 'run method1'
   end subroutine method1

   subroutine method2(this)
      class (SimpleTestCase), intent(inOut) :: this
      this%runLog = 'run method2'
   end subroutine method2

   subroutine methodWith2Exceptions(this)
      use PF_ExceptionList, only: throw
      class (SimpleTestCase), intent(inOut) :: this

      _UNUSED_DUMMY(this)
      call throw('failure A')
      call throw('failure B')
   end subroutine methodWith2Exceptions

   subroutine delete_(this)
      type (SimpleTestCase), intent(inOut) :: this
      _UNUSED_DUMMY(this)
   end subroutine delete_

end module Pf_SimpleTestCase
