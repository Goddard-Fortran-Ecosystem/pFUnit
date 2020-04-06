!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: TestMethod
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module PF_TestMethod
   use PF_TestCase, only: TestCase
   use PF_KeywordEnforcer
   implicit none
   private

   public :: TestMethod

   type, extends(TestCase) :: TestMethod
      procedure(empty), nopass, pointer :: userMethod => null()
      procedure(empty), nopass, pointer :: userSetUp => null()
      procedure(empty), nopass, pointer :: userTearDown => null()
   contains
     procedure :: runMethod
     procedure :: setUp
     procedure :: tearDown
   end type TestMethod

   abstract interface
      subroutine empty()
      end subroutine empty
   end interface

   interface TestMethod
      module procedure TestMethod_
      module procedure TestMethod_setUpTearDown
   end interface TestMethod

contains


   function TestMethod_(name, method, unused, setUp, tearDown) result(this)
      type (TestMethod) :: this
      character(len=*), intent(in) :: name
      procedure(empty) :: method
      class(KeywordEnforcer), optional, intent(in) :: unused
      procedure(empty), optional :: setUp
      procedure(empty), optional :: tearDown

      call this%setName(name)
      this%userMethod => method
      this%userSetUp => setUp
      this%userTearDown => tearDown

   end function TestMethod_

   ! Modified dummy arguments to avoid overload conflict with above variant.
   ! This interface is deprecated and should be deleted in 5.0.
   function TestMethod_setUpTearDown(name, method, setUp_, tearDown_) result(this)
      type (TestMethod) :: this
      character(len=*), intent(in) :: name
      procedure(empty) :: method
      procedure(empty) :: setUp_
      procedure(empty) :: tearDown_

      call this%setName(name)
      this%userMethod => method
      this%userSetUp => setUp_
      this%userTearDown => tearDown_

   end function TestMethod_setUpTearDown


   recursive subroutine runMethod(this)
      class (TestMethod), intent(inOut) :: this

      call this%userMethod()

   end subroutine runMethod

   subroutine setUp(this)
      class (TestMethod), intent(inout) :: this
      if (associated(this%userSetUp)) call this%userSetUp()
   end subroutine setUp

   subroutine tearDown(this)
      class (TestMethod), intent(inout) :: this
      if (associated(this%userTearDown)) call this%userTearDown()
   end subroutine tearDown

end module PF_TestMethod
