module pf_TextDescription_mod

   use pf_MatcherDescription_mod

   implicit none
   private

   public :: TextDescription

   type, extends(MatcherDescription) :: TextDescription
      private
   contains
      procedure :: append_text
      procedure :: append_description_of
      procedure :: to_string
   end type TextDescription
