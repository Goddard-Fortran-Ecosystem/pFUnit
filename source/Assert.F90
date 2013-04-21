module Assert_mod
   use AssertBasic_mod
   use AssertInteger_mod
   implicit none
   private

   public :: assertTrue
   public :: assertFalse
   public :: assertEqual
   public :: assertExceptionRaised
   public :: assertSameShape

   public :: assertLessThan, assertLessThanOrEqual
   public :: assertGreaterThan, assertGreaterThanOrEqual

contains

end module Assert_mod
