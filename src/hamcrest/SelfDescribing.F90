module pf_SelfDescribing_mod
   implicit none
   private

   public :: SelfDescribing

   type, abstract :: SelfDescribing
   contains
      procedure :: describe_to
   end type SelfDescribing

contains

   subroutine describe_to

end module pf_SelfDescribing_mod
