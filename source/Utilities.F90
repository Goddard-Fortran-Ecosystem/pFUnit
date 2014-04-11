module Utilities_mod
   implicit none
   public :: newUnit
contains
   integer function newUnit(unit)
      ! Returns the number of a free/unopened unit.
      integer, intent(out), optional :: unit
      integer, parameter :: LUN_MIN=20, LUN_MAX=1000
      logical :: opened
      integer :: lun
      newunit=-1
      do lun=LUN_MIN,LUN_MAX
         inquire(unit=lun,opened=opened)
         if (.not. opened) then
            newunit=lun
            exit
         end if
      end do
      if (present(unit)) unit=newunit
   end function newUnit
end module Utilities_mod
