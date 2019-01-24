module PF_RegularExpression
   implicit none
   private

   public :: RegularExpression

   type :: RegularExpression
      private
      character(len=:), allocatable :: pattern
   contains
      procedure :: match
   end type RegularExpression


   interface RegularExpression
      module procedure new_RegularExpression
   end interface RegularExpression

contains

   function new_RegularExpression(pattern) result(regexp)
      type (RegularExpression) :: regexp
      character(len=*), intent(in) :: pattern
      regexp%pattern = pattern
   end function new_RegularExpression


   logical function match(this, string)
      class (RegularExpression), intent(in) :: this
      character(len=*), intent(in) :: string

      integer :: i, j, n
      character(len=1) :: c

      match = .true. ! unless

      j = 0
      n = len(string)
      do i = 1, len(this%pattern)
         if (i > n) then
            match = .false.
            return
         end if

         c = this%pattern(i:i)
         select case (c)
         case ('.') ! match anything
            j = j + 1
            cycle
         case default
            match = (c == string(i:i))
            if (.not. match) return
         end select
         j = j + 1
      end do

      if (j < n) match = .false. ! too long

         
      

   end function match

end module PF_RegularExpression
