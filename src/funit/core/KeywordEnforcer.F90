! This module implements a mechanism that can be used to enforce
! keyword association for dummy arguments in an interface.  The
! concept is to have a derived type for which no actual argument can
! ever be provided.

! The original idea comes (AFAIK) from ESMF which uses a PUBLIC
! derived type that is simply not exported in the main ESMF
! package.  That approach has one weakness, which is that a clever
! user can still access the module that defines the type.  Various
! workarounds for that are possible such as using a truly PRIVATE
! type, but these encounter further issues for type-bound
! procedures which are then overridden in a subclass.

! The approach here, suggested by Dan Nagle, is to use an ABSTRACT
! type which prevents variables from being declared with that type.
! Tom Clune improved upon this by introducing a DEFERRED type-bound
! procedure that prevents extending the type to a non-abstract
! class.  A DEFERRED, PRIVATE type-bound procedure is attached to
! the type and cannot be overridden outside of this module.  Any
! non-abstract extension must implement the method.  (Note that
! ABSTRACT extensions can be created, but do not circumvent the
! keyword enforcement.

module pf_KeywordEnforcer
   implicit none
   private

   public :: KeywordEnforcer

   type, abstract :: KeywordEnforcer
   contains
      procedure (nonimplementable), deferred, nopass, private :: nonimplementable
   end type KeywordEnforcer

   abstract interface
      subroutine nonimplementable()
      end subroutine nonimplementable
   end interface

end module pf_KeywordEnforcer
