#include "unused_dummy.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: FixtureTestCase
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
module FixtureTestCase
   use PF_TestCase, only: TestCase
   implicit none
   private

   public :: FixtureTest
   public :: newFixtureTest
   public :: delete
   public :: methodA
   public :: methodB
   public :: simpleTestMethod

   type, extends(TestCase) :: FixtureTest
      private
      character(len=30), public :: runLog
   contains
      procedure :: setUp
      procedure :: runMethod
      procedure :: tearDown
   end type FixtureTest

   interface delete
      module procedure delete_
   end interface
   
contains

   function newFixtureTest() result(this)
      type(FixtureTest) :: this

      call this%setName('FixtureTest')
      this%runLog = ' '

   end function newFixtureTest

   subroutine setUp(this)
      class(FixtureTest), intent(inOut) :: this
      this%runLog = trim(this%runLog) // 'setUp '
   end subroutine setUp

   subroutine tearDown(this)
      class(FixtureTest), intent(inOut) :: this
      this%runLog = trim(this%runLog) // ' tearDown'
   end subroutine tearDown

   subroutine runMethod(this)
      class(FixtureTest), intent(inOut) :: this

      this%runLog = trim(this%runLog) // ' run'

   end subroutine runMethod

   subroutine simpleTestMethod(this)
      class (FixtureTest), intent(inOut) :: this
      this%runLog = trim(this%runLog) // ' run'
   end subroutine simpleTestMethod

   subroutine methodA(this)
      class (FixtureTest), intent(inOut) :: this
      this%runLog = trim(this%runLog) // ' methodA'
   end subroutine methodA

   subroutine methodB(this)
      class (FixtureTest), intent(inOut) :: this
      this%runLog = trim(this%runLog) // ' methodB'
   end subroutine methodB

   subroutine delete_(this)
      type (FixtureTest), intent(inOut) :: this
      _UNUSED_DUMMY(this)
   end subroutine delete_

end module FixtureTestCase
