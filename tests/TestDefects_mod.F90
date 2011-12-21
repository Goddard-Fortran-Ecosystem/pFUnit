module TestDefects_mod
  use pFUnit
  implicit none
  private

  public :: testMixedTypeAssert

contains

  ! The following test was causing an output statement overflow during an
  ! internal write.   Assertion has to fail to generate the problem.
  subroutine testMixedTypeAssert()
    call assertEqual([1,1,1], [-1.,-1.,-1.])
    call assertFailedAssert()
  end subroutine testMixedTypeAssert

end module TestDefects_mod
