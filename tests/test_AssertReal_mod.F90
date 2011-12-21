! For convenient testing floating point values, we must consider a very large number of cases.
! The variations span the following axes:
!  1) Comparison against integers and floats of lesser or equal precision
!  2) Comparison against same rank or scalar
!  3) Comparison with specified tolerance or default (which can be in either precision)
!  4) Different metrics for the tolerance.

module test_AssertReal_mod
  use Params_mod, only: r32, r64
  use Assert_mod, only: assertTrue, assertFalse, assertFailedAssert
  use AssertReal_mod
  implicit none
  private

  public :: test_vectorNorm
  public :: test_isWithinTolerance
  public :: test_assertEqual_float
  public :: test_assertEqual_tolerance

  public :: test_assertEqual_floatFail
  public :: test_assertEqual_floatMessage

  integer, parameter :: MAX_LEN_FLOAT = 25

  character(len=*), parameter :: indent='       '

contains

  subroutine test_vectorNorm()
    real (kind=r64), parameter :: TOLERANCE = epsilon(1._r64)

    call assertTrue(abs(vectorNorm(x = 1._r64, norm = L_INFINITY_NORM) - 1) <= TOLERANCE)
    call assertTrue(abs(vectorNorm(x = 1._r64, norm = L1_NORM) - 1)         <= TOLERANCE)
    call assertTrue(abs(vectorNorm(x = 1._r64, norm = L2_NORM) - 1)         <= TOLERANCE)

    call assertTrue(abs(vectorNorm(x = (/ 3._r64, 4._r64 /), norm = L_INFINITY_NORM) - 4) <= TOLERANCE)
    call assertTrue(abs(vectorNorm(x = (/ 3._r64,-4._r64 /), norm = L_INFINITY_NORM) - 4) <= TOLERANCE)
    call assertTrue(abs(vectorNorm(x = (/ 3._r64, 4._r64 /), norm = L1_NORM) - 7)         <= TOLERANCE)
    call assertTrue(abs(vectorNorm(x = (/ 3._r64,-4._r64 /), norm = L1_NORM) - 7)         <= TOLERANCE)
    call assertTrue(abs(vectorNorm(x = (/ 3._r64, 4._r64 /), norm = L2_NORM) - 5)         <= TOLERANCE)
    call assertTrue(abs(vectorNorm(x = (/ 3._r64,-4._r64 /), norm = L2_NORM) - 5)         <= TOLERANCE)

  end subroutine test_vectorNorm

  subroutine test_isWithinTolerance()
    call assertTrue(  isWithinTolerance(1._r64, 2._r64,  norm = L2_NORM))
    call assertFalse( isWithinTolerance(1._r64, 0.5_r64, norm = L2_NORM))

    call assertTrue(  isWithinTolerance((/ 3._r64, 4._r64 /), 10._r64, norm = L2_NORM))
    call assertFalse( isWithinTolerance((/ 3._r64, 4._r64 /), 2._r64, norm = L2_NORM))

  end subroutine test_isWithinTolerance

  ! "1" should equal "1" regardless of precision and rank
  subroutine test_assertEqual_float()
    integer, parameter :: N=2
    integer        :: expectInt, expectVecInt(N)
    real(kind=r32) :: expectR32, expectVecR32(N)
    real(kind=r64) :: expectR64, expectVecR64(N)

    real(kind=r32) :: foundR32, foundVecR32(N)
    real(kind=r64) :: foundR64, foundVecR64(N)

    expectInt = 1
    expectR32 = 1
    expectR64 = 1
    expectVecInt = 1
    expectVecR32 = 1
    expectVecR64 = 1

    foundR32 = 1
    foundR64 = 1
    foundVecR32 = 1
    foundVecR64 = 1

    call assertEqual(expectInt, foundR32, message = 'expectInt, foundR32')
    call assertEqual(expectInt, foundR64, message = 'expectInt, foundR64')
    call assertEqual(expectR32, foundR32, message = 'expectR32, foundR32')
    call assertEqual(expectR32, foundR64, message = 'expectR32, foundR64')
    call assertEqual(expectR64, foundR64, message = 'expectR64, foundR64')

    call assertEqual(expectInt, foundVecR32, message = 'expectInt, foundVecR32')
    call assertEqual(expectInt, foundVecR64, message = 'expectInt, foundVecR64')
    call assertEqual(expectR32, foundVecR32, message = 'expectR32, foundVecR32')
    call assertEqual(expectR32, foundVecR64, message = 'expectR32, foundVecR64')
    call assertEqual(expectR64, foundVecR64, message = 'expectR64, foundVecR64')

    call assertEqual(expectVecInt, foundVecR32, message = 'expectVecInt, foundVecR32')
    call assertEqual(expectVecInt, foundVecR64, message = 'expectVecInt, foundVecR64')
    call assertEqual(expectVecR32, foundVecR32, message = 'expectVecR32, foundVecR32')
    call assertEqual(expectVecR32, foundVecR64, message = 'expectVecR32, foundVecR64')
    call assertEqual(expectVecR64, foundVecR64, message = 'expectVecR64, foundVecR64')

  end subroutine test_assertEqual_float

  ! "1.01" should equal "1" within "0.1" regardless of precision and rank
  subroutine test_assertEqual_tolerance()

    real(kind=r32), parameter :: tol32 = 0.1
    real(kind=r64), parameter :: tol64 = 0.1
    
    integer, parameter :: N=2
    integer        :: expectInt, expectVecInt(N)
    real(kind=r32) :: expectR32, expectVecR32(N)
    real(kind=r64) :: expectR64, expectVecR64(N)

    real(kind=r32) :: foundR32, foundVecR32(N)
    real(kind=r64) :: foundR64, foundVecR64(N)

    expectInt = 1
    expectR32 = 1
    expectR64 = 1
    expectVecInt = 1
    expectVecR32 = 1
    expectVecR64 = 1

    foundR32 = 1.01
    foundR64 = 1.01
    foundVecR32 = 1.01
    foundVecR64 = 1.01

    call assertEqual(expectInt, foundR32, tolerance = tol32, message = 'expectInt, foundR32')
    call assertEqual(expectInt, foundR64, tolerance = tol32, message = 'expectInt, foundR64')
    call assertEqual(expectR32, foundR32, tolerance = tol32, message = 'expectR32, foundR32')
    call assertEqual(expectR32, foundR64, tolerance = tol32, message = 'expectR32, foundR64')
    call assertEqual(expectR64, foundR64, tolerance = tol32, message = 'expectR64, foundR64')

    call assertEqual(expectInt, foundVecR32, tolerance = tol32, message = 'expectInt, foundVecR32')
    call assertEqual(expectInt, foundVecR64, tolerance = tol32, message = 'expectInt, foundVecR64')
    call assertEqual(expectR32, foundVecR32, tolerance = tol32, message = 'expectR32, foundVecR32')
    call assertEqual(expectR32, foundVecR64, tolerance = tol32, message = 'expectR32, foundVecR64')
    call assertEqual(expectR64, foundVecR64, tolerance = tol32, message = 'expectR64, foundVecR64')

    call assertEqual(expectVecInt, foundVecR32, tolerance = tol32, message = 'expectVecInt, foundVecR32')
    call assertEqual(expectVecInt, foundVecR64, tolerance = tol32, message = 'expectVecInt, foundVecR64')
    call assertEqual(expectVecR32, foundVecR32, tolerance = tol32, message = 'expectVecR32, foundVecR32')
    call assertEqual(expectVecR32, foundVecR64, tolerance = tol32, message = 'expectVecR32, foundVecR64')
    call assertEqual(expectVecR64, foundVecR64, tolerance = tol32, message = 'expectVecR64, foundVecR64')

    call assertEqual(expectInt, foundR32, tolerance = tol64, message = 'expectInt, foundR32')
    call assertEqual(expectInt, foundR64, tolerance = tol64, message = 'expectInt, foundR64')
    call assertEqual(expectR32, foundR32, tolerance = tol64, message = 'expectR32, foundR32')
    call assertEqual(expectR32, foundR64, tolerance = tol64, message = 'expectR32, foundR64')
    call assertEqual(expectR64, foundR64, tolerance = tol64, message = 'expectR64, foundR64')

    call assertEqual(expectInt, foundVecR32, tolerance = tol64, message = 'expectInt, foundVecR32')
    call assertEqual(expectInt, foundVecR64, tolerance = tol64, message = 'expectInt, foundVecR64')
    call assertEqual(expectR32, foundVecR32, tolerance = tol64, message = 'expectR32, foundVecR32')
    call assertEqual(expectR32, foundVecR64, tolerance = tol64, message = 'expectR32, foundVecR64')
    call assertEqual(expectR64, foundVecR64, tolerance = tol64, message = 'expectR64, foundVecR64')

    call assertEqual(expectVecInt, foundVecR32, tolerance = tol64, message = 'expectVecInt, foundVecR32')
    call assertEqual(expectVecInt, foundVecR64, tolerance = tol64, message = 'expectVecInt, foundVecR64')
    call assertEqual(expectVecR32, foundVecR32, tolerance = tol64, message = 'expectVecR32, foundVecR32')
    call assertEqual(expectVecR32, foundVecR64, tolerance = tol64, message = 'expectVecR32, foundVecR64')
    call assertEqual(expectVecR64, foundVecR64, tolerance = tol64, message = 'expectVecR64, foundVecR64')
  end subroutine test_assertEqual_tolerance

  ! "1" should equal "1" regardless of precision and rank
  subroutine test_assertEqual_floatFail()
    integer, parameter :: N=2
    integer        :: expectInt, expectVecInt(N)
    real(kind=r32) :: expectR32, expectVecR32(N)
    real(kind=r64) :: expectR64, expectVecR64(N)

    real(kind=r32) :: foundR32, foundVecR32(N)
    real(kind=r64) :: foundR64, foundVecR64(N)

    expectInt = 1
    expectR32 = 1
    expectR64 = 1
    expectVecInt = 1
    expectVecR32 = 1
    expectVecR64 = 1

    ! choose nearest (non-equal) floating value
    foundR32 = nearest(1._r32,1._r32)
    foundR64 = nearest(1._r64,1._r32)
    foundVecR32 = foundR32
    foundVecR64 = foundR64

    call assertEqual(expectInt, foundR32, message = 'expectInt, foundR32')
    call assertFailedAssert()
    call assertEqual(expectInt, foundR64, message = 'expectInt, foundR64')
    call assertFailedAssert()
    call assertEqual(expectR32, foundR32, message = 'expectR32, foundR32')
    call assertFailedAssert()
!!$$    call assertEqual(expectR32, foundR64, message = 'expectR32, foundR64')
!!$$    call assertFailedAssert()
!!$$    call assertEqual(expectR64, foundR64, message = 'expectR64, foundR64')
!!$$    call assertFailedAssert()
!!$$
!!$$    call assertEqual(expectInt, foundVecR32, message = 'expectInt, foundVecR32')
!!$$    call assertFailedAssert()
!!$$    call assertEqual(expectInt, foundVecR64, message = 'expectInt, foundVecR64')
!!$$    call assertFailedAssert()
!!$$    call assertEqual(expectR32, foundVecR32, message = 'expectR32, foundVecR32')
!!$$    call assertFailedAssert()
!!$$    call assertEqual(expectR32, foundVecR64, message = 'expectR32, foundVecR64')
!!$$    call assertFailedAssert()
!!$$    call assertEqual(expectR64, foundVecR64, message = 'expectR64, foundVecR64')
!!$$    call assertFailedAssert()
!!$$
!!$$    call assertEqual(expectVecInt, foundVecR32, message = 'expectVecInt, foundVecR32')
!!$$    call assertFailedAssert()
!!$$    call assertEqual(expectVecInt, foundVecR64, message = 'expectVecInt, foundVecR64')
!!$$    call assertFailedAssert()
!!$$    call assertEqual(expectVecR32, foundVecR32, message = 'expectVecR32, foundVecR32')
!!$$    call assertFailedAssert()
!!$$    call assertEqual(expectVecR32, foundVecR64, message = 'expectVecR32, foundVecR64')
!!$$    call assertFailedAssert()
!!$$    call assertEqual(expectVecR64, foundVecR64, message = 'expectVecR64, foundVecR64')
!!$$    call assertFailedAssert()

  end subroutine test_assertEqual_floatFail

  subroutine test_assertEqual_floatMessage()
    character(len=1000) :: expectedMessage

    integer, parameter :: N=2
    integer        :: expectInt, expectVecInt(N)
    real(kind=r32) :: expectR32, expectVecR32(N)
    real(kind=r64) :: expectR64, expectVecR64(N)

    real(kind=r32) :: foundR32, foundVecR32(N)
    real(kind=r64) :: foundR64, foundVecR64(N)

    expectInt = 100
    expectR32 = 100
    expectR64 = 100
    expectVecInt = (/ 100, 200 /)
    expectVecR32 = (/ 100, 200 /)
    expectVecR64 = (/ 100, 200 /)

    ! choose nearest (non-equal) floating value
    foundR32 = nearest(100._r32,100._r32)
    foundR64 = nearest(100._r64,100._r32)
    foundVecR32 = (/ nearest(100._r32,1._r32), nearest(200._r32,1._r32) /)
    foundVecR64 = (/ nearest(100._r64,1._r64), nearest(200._r64,1._r64) /)

    call assertEqual(expectInt, foundR32, message = 'expectInt, foundR32')
    expectedMessage = 'Floating point scalar assertion failed: expectInt, foundR32' // NEW_LINE('a')
    call append(expectedMessage, '       Expected:  ' // trim(toString(expectInt)) // '' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: ' // trim(toString(foundR32)) // '' // NEW_LINE('a'))
    call append(expectedMessage, '       delta:     ' // trim(toString(foundR32-expectInt)))
    call append(expectedMessage, ' > ' // trim(toString(tiny(1._r32))))
    call assertFailedAssert(expectedMessage)

    call assertEqual(expectInt, foundR64, message = 'expectInt, foundR64')
    expectedMessage = 'Floating point scalar assertion failed: expectInt, foundR64' // NEW_LINE('a')
    call append(expectedMessage, '       Expected:  ' // trim(toString(expectInt)) // '' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: ' // trim(toString(foundR64)) // '' // NEW_LINE('a'))
    call append(expectedMessage, '       delta:     ' // trim(toString(foundR64-expectInt)))
    call append(expectedMessage, ' > ' // trim(toString(tiny(1._r64))))
    call assertFailedAssert(expectedMessage)

    call assertEqual(expectR32, foundR32, message = 'expectR32, foundR32')

    expectedMessage = 'Floating point scalar assertion failed: expectR32, foundR32' // NEW_LINE('a')
    call append(expectedMessage, '       Expected:  ' // trim(toString(expectR32)) // '' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: ' // trim(toString(foundR32))  // '' // NEW_LINE('a'))
    call append(expectedMessage, '       delta:     ' // trim(toString(foundR32-expectR32)))
    call append(expectedMessage, ' > ' // trim(toString(tiny(1._r32))))

    call assertFailedAssert(expectedMessage)

    call assertEqual(expectInt, foundVecR32, message = 'expectInt, foundVecR32')

    expectedMessage = 'Floating point scalar assertion failed: expectInt, foundVecR32' // NEW_LINE('a')
    call append(expectedMessage, '       First difference at element 1.' // NEW_LINE('a'))
    call append(expectedMessage, '       Expected:  ' // trim(toString(expectInt)) // '' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: ' // trim(toString(foundVecR32))  // '' // NEW_LINE('a'))
    call append(expectedMessage, '       delta:     ' // trim(toString(foundVecR32-expectInt)))
    call append(expectedMessage, ' > ' // trim(toString(tiny(1._r32))))

    call assertFailedAssert(expectedMessage)

  end subroutine test_assertEqual_floatMessage

  subroutine append(string, suffix)
    character(len=*), intent(inout) :: string
    character(len=*), intent(in)    :: suffix

    string = trim(string) // trim(suffix)

  end subroutine append

end module test_AssertReal_mod
