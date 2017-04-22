module PF_ExceptionList_mod
   use PF_SourceLocation_mod
   use PF_Exception_mod, only: Exception
   use PF_ExceptionVector_mod
   implicit none
   private

   public :: ExceptionList

   type, extends(ExceptionVector) :: ExceptionList
      private
   contains
      generic :: throw => throw_exception, throw_message
      generic :: catch => catch_message

      procedure :: throw_exception
      procedure :: throw_message
      procedure :: catch_message
   end type ExceptionList

contains

   subroutine throw_exception(this, anException)
      class (ExceptionList), intent(inout) :: this
      class (Exception), intent(in) :: anException

      call this%push_back(anException)

   end subroutine throw_exception


   subroutine throw_message(this, message, location)
      class (ExceptionList), intent(inOut) :: this
      character(len=*), intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      call this%throw(Exception(message, location))

   end subroutine throw_message


   logical function catch_message(this, message, preserve) result(found)
      class (ExceptionList), intent(inout) :: this
      character(len=*), intent(in) :: message
      logical, optional, intent(in) :: preserve

      type (ExceptionVectorIterator) :: iter
      logical :: preserve_
      class (Exception), pointer :: e

      preserve_ = .false.
      if (present(preserve)) then
         preserve_ = preserve
      end if

      iter = this%begin()
      do while (iter /= this%end())
         e => iter%get()
         if (e%getMessage() == message) then
            found = .true.
            if (.not. preserve_) call this%erase(iter)
            return
         end if
         call iter%next()
      end do

      found = .false.
      
   end function catch_message

end module PF_ExceptionList_mod
