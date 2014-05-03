!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: MockRepository
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC 
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
module MockRepository_mod
   implicit none
   private

   public :: MockRepository
   public :: newMockRepository

   integer, parameter :: MAX_LEN_METHOD_NAME = 32
   type MockRepository
      character(len=MAX_LEN_METHOD_NAME) :: method = ' '
   contains
      procedure :: verifyMocking

      procedure :: expectCall
      procedure :: hasCalled
   end type MockRepository

contains

   function newMockRepository() result(repository)
      type (MockRepository), pointer :: repository
      allocate(repository)
   end function newMockRepository

   subroutine verifyMocking(this, object)
      use Exception_mod
      class (MockRepository), intent(inout) :: this
      class (*) :: object
      
      if (trim(this%method) /= '') then
         call throw('Expected method not called: method1() on object of class MockSUT.')
      end if

   end subroutine verifyMocking

   subroutine expectCall(this, obj, method)
      class (MockRepository), intent(inout) :: this
      class(*), intent(in) :: obj
      character(len=*), intent(in) :: method

      this%method = method
   end subroutine expectCall

   subroutine hasCalled(this, obj, method)
      class (MockRepository), intent(inout) :: this
      class(*), intent(in) :: obj
      character(len=*), intent(in) :: method

      if (trim(method) == trim(this%method)) then
         this%method=''
      end if
   end subroutine hasCalled
   
end module MockRepository_mod
