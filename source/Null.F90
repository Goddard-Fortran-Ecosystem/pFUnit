module pf_Null_mod
   implicit none
   private

   public :: NULL

   type :: t_Null ! private type
   end type t_Null

   type (t_Null), parameter :: NULL = t_NULL()
   
end module pf_Null_mod
