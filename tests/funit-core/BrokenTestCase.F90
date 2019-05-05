!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: BrokenTestCase
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
module BrokenTestCase
   use PF_TestCase, only: TestCase
   implicit none
   private
   
   public :: BrokenTest
   
   type, extends(TestCase) :: BrokenTest
      private
      character(len=40), public :: runLog
   contains
      procedure :: setUp
      procedure :: tearDown
      procedure :: runMethod
   end type BrokenTest
   
contains

   subroutine setUp(this)
      class(BrokenTest), intent(inOut) :: this

      this%runLog = 'setUp'

   end subroutine setUp

   subroutine runMethod(this)
      use PF_ExceptionList, only: throw
      class(BrokenTest), intent(inOut) :: this

      this%runLog = trim(this%runLog) // ' broken run'
      call throw('This test is intentionally broken.')

   end subroutine runMethod

   subroutine tearDown(this)
      class(BrokenTest), intent(inOut) :: this

      this%runLog = trim(this%runLog) // ' tearDown'

   end subroutine tearDown

end module BrokenTestCase
