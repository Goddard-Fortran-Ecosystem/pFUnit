module ESMF_TestMethod_mod
   use MpiTestParameter_mod
   use pFUnit_mod
   use ESMF
   use ESMF_TestCase_mod
   use ESMF_TestParameter_mod
   implicit none
   private

   public :: ESMF_TestMethod
   public :: newESMF_TestMethod

   type, extends(ESMF_TestCase) :: ESMF_TestMethod
      procedure(esmfMethod), pointer :: userMethod => null()
      procedure(esmfMethod), pointer :: userSetUp => null()
      procedure(esmfMethod), pointer :: userTearDown => null()
   contains
      procedure :: runMethod
      procedure :: setUp
      procedure :: tearDown
   end type ESMF_TestMethod

   abstract interface
      subroutine esmfMethod(this)
         import ESMF_TestMethod
         class (ESMF_TestMethod), intent(inout) :: this
      end subroutine esmfMethod
   end interface

   interface newEsmf_TestMethod
      module procedure newEsmf_TestMethod_basic
      module procedure newEsmf_TestMethod_setUpTearDown
   end interface newEsmf_TestMethod

contains

   
   function newEsmf_TestMethod_basic(name, userMethod, numPETs) result(esmf_Test)
      character(len=*), intent(in) :: name
      procedure (runMethod) :: userMethod
      integer, intent(in) :: numPETs
      type (Esmf_TestMethod), target :: esmf_Test

      call esmf_Test%setName(name)
      esmf_Test%userMethod => userMethod
      call esmf_Test%setTestParameter(ESMF_TestParameter(numPETs))


   end function newEsmf_TestMethod_basic

   function newEsmf_TestMethod_setUpTearDown(name, userMethod, numPETs, setUp, tearDown) result(esmf_Test)
      character(len=*), intent(in) :: name
      procedure (runMethod) :: userMethod
      integer, intent(in) :: numPETs
      type (Esmf_TestMethod), target :: esmf_Test
      procedure (runMethod) :: setUp
      procedure (runMethod) :: tearDown

      call esmf_Test%setName(name)
      esmf_Test%userMethod => userMethod
      call esmf_Test%setTestParameter(ESMF_TestParameter(numPETs))

      esmf_Test%userSetUp => setUp
      esmf_Test%userTearDown => tearDown


   end function newEsmf_TestMethod_setUpTearDown


   subroutine setUp(this)
      class (ESMF_TestMethod), intent(inout) :: this

      if (associated(this%userSetUp)) then
         call this%userSetUp()
      end if

   end subroutine setUp


   subroutine runMethod(this)
      class (ESMF_TestMethod), intent(inout) :: this

      call this%userMethod()
   end subroutine runMethod


   subroutine tearDown(this)
      class (ESMF_TestMethod), intent(inout) :: this

      if (associated(this%userTearDown)) then
         call this%userTearDown()
      end if

   end subroutine tearDown

end module ESMF_TestMethod_mod
