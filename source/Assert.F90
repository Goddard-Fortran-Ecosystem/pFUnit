module Assert_mod
   use AssertBasic_mod
   use AssertInteger_mod
   use AssertReal_mod
   use AssertComplex_mod, only : AssertEqual
   implicit none
   private

   public :: assertTrue
   public :: assertFalse
   public :: assertEqual
   public :: assertExceptionRaised
   public :: assertSameShape

   public :: assertAny
   public :: assertAll
   public :: assertNone
   public :: assertNotAll

   public :: assertLessThan, assertLessThanOrEqual
   public :: assertGreaterThan, assertGreaterThanOrEqual

   public :: assertIsNan, assertIsFinite

contains

end module Assert_mod
