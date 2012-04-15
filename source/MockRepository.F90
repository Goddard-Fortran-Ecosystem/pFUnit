module MockRepository_mod
   implicit none
   private

   public :: MockRepository
   public :: newMockRepository

   type MockRepository
      logical :: flag = .false.
   contains
      procedure :: verifyMocking
      procedure :: expectCall
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
      
      if (this%flag) then
         call throw('Expected method not called: method1() on object of class MockSUT.')
      end if
      this%flag = .false.

   end subroutine verifyMocking

   subroutine expectCall(this, obj, method)
      class (MockRepository), intent(inout) :: this
      class(*), intent(in) :: obj
      character(len=*), intent(in) :: method

      this%flag = .true.
   end subroutine expectCall

end module MockRepository_mod
