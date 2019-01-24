module PF_GlobPattern
   implicit none
   private

   public :: GlobPattern

   type :: GlobPattern
      private
      character(len=:), allocatable :: pattern
   contains
      procedure :: match
   end type GlobPattern


   interface GlobPattern
      module procedure new_GlobPattern
   end interface GlobPattern


contains


   function new_GlobPattern(pattern) result(glob)
      type (GlobPattern) :: glob
      character(len=*), intent(in) :: pattern

      glob%pattern = pattern

   end function new_GlobPattern


   logical function match(this, string)
      class (GlobPattern), intent(in) :: this
      character(len=*), intent(in) :: string


      match = match_(this%pattern, string)
      
   end function match

   recursive logical function match_(pattern, string) result(m)
      character(len=*), intent(in) :: pattern
      character(len=*), intent(in) :: string

      integer :: n_s
      integer :: n_p


      n_s = len(string)
      n_p = len(pattern)

      if (n_p == 0) then
         m = (n_s == 0)
         return
      end if

      select case (pattern(1:1))
      case ('*')
         m = match_(pattern(2:),string)
         if (n_s > 0) then
            m = m .or. match_(pattern,string(2:))
         end if
      case ('?')
         m = (n_s > 0) .and. match_(pattern(2:),string(2:))
      case default
         if (n_s > 0) then
            m = (pattern(1:1) == string(1:1)) .and. match_(pattern(2:),string(2:))
         else
            m = .false.
         end if
      end select

   end function match_

end module PF_GlobPattern
