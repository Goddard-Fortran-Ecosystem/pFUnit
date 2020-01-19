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

  ! The optional dummy arguments have weird names to prevent backward incompatibility
  ! issues.
   function TestMethod_(name, method, unused, opt_setUp, opt_TearDown) result(this)
      type (TestMethod) :: this
      character(len=*), intent(in) :: name
      procedure(empty) :: method
      class(KeywordEnforcer), optional, intent(in) :: unused
      procedure(empty), optional :: opt_setUp
      procedure(empty), optional :: opt_tearDown

      call this%setName(name)
      this%userMethod => method
      this%userSetUp => opt_setUp
      this%userTearDown => opt_tearDown

   end function TestMethod_

   ! This interface (nonoptional dummies)  should be deprecated. (Delete in 5.0)
   function TestMethod_setUpTearDown(name, method, setUp, tearDown) result(this)
      type (TestMethod) :: this
      character(len=*), intent(in) :: name
      procedure(empty) :: method
      procedure(empty) :: setUp
      procedure(empty), optional :: tearDown

      call this%setName(name)
      this%userMethod => method
      this%userSetUp => setUp
      this%userTearDown => tearDown

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
