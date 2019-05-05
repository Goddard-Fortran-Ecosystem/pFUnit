!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: BrokenSetUpCase
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
module BrokenSetUpCase
   use PF_TestCase, only: TestCase
   use PF_ExceptionList, only: throw
   implicit none
   private
   
   public :: BrokenSetUp
   public :: newBrokenSetUp
   
   type, extends(TestCase) :: BrokenSetUp
      private
      character(len=40), public :: runLog
   contains
      procedure :: setUp
      procedure :: runMethod
      procedure :: tearDown
   end type BrokenSetUp
   
contains

   function newBrokenSetUp() result(this)
      type (BrokenSetUp), pointer :: this
      allocate(this)
      call this%setName('BrokenSetUp')
   end function newBrokenSetUp

   subroutine setUp(this)
      class(BrokenSetUp), intent(inOut) :: this

      this%runLog = 'broken setUp'
      call throw('This setUp() is intentionally broken.')

   end subroutine setUp

   subroutine tearDown(this)
      class(BrokenSetUp), intent(inOut) :: this

      this%runLog = trim(this%runLog)//' tearDown'

   end subroutine tearDown

   subroutine runMethod(this)
      class(BrokenSetUp), intent(inOut) :: this

      this%runLog = trim(this%runLog)//' run'

   end subroutine runMethod

end module BrokenSetUpCase
