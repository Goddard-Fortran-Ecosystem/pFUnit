module pf_None
   implicit none
   private

   public :: NONE

   type :: t_None ! private type
   end type t_None

   type (t_None), parameter :: NONE = t_NONE()
   
end module pf_None
