! This is for testing complex values

module test_AssertComplex_mod
  use Params_mod, only: r32, r64, c32, c64
  use Assert_mod, only : assertFailedAssert
  use AssertComplex_mod
  implicit none
  private

  public :: test_assertEqual_complex
  public :: test_assertEqual_complex_failed
  public :: test_assertEqual_complex_int
  public :: test_assertEqual_complex_real

contains

  subroutine test_assertEqual_complex()
    integer, parameter :: N=2
    complex (kind=c32) :: expectC32, expectVecC32(N)
    complex (kind=c64) :: expectC64, expectVecC64(N)

    complex (kind=c32) :: foundC32, foundVecC32(N)
    complex (kind=c64) :: foundC64, foundVecC64(N)

    expectC32 = (1.0, 1.)
    expectC64 = (1.0, 1.)
    expectVecC32 = (1.0, 1.)
    expectVecC64 = (1.0, 1.)

    foundC32 = (1.0, 1.)
    foundC64 = (1.0, 1.)
    foundVecC32 = (1.0, 1.)
    foundVecC64 = (1.0, 1.)

    call assertEqual(expectC32, foundC32, message = 'expectC32, foundC32')
    call assertEqual(expectC32, foundC64, message = 'expectC32, foundC64')
    call assertEqual(expectC64, foundC32, message = 'expectC64, foundC32')
    call assertEqual(expectC64, foundC64, message = 'expectC64, foundC64')

    call assertEqual(expectVecC32, foundVecC32, &
               &     message = 'expectVecC32, foundVecC32')

    call assertEqual(expectVecC32, foundVecC64, &
               &     message = 'expectVecC32, foundVecC64')

    call assertEqual(expectVecC64, foundVecC32, &
               &     message = 'expectVecC64, foundVecC32')

    call assertEqual(expectVecC64, foundVecC64, &
               &     message = 'expectVecC64, foundVecC64')
                      

  end subroutine test_assertEqual_complex


  subroutine test_assertEqual_complex_failed()
    integer, parameter :: N=2
    complex (kind=c32) :: expectC32, expectVecC32(N)
    complex (kind=c64) :: expectC64, expectVecC64(N)
    complex (kind=c32) :: foundC32, foundVecC32(N)
    complex (kind=c64) :: foundC64, foundVecC64(N)

    expectC32 = (1.0, 0.0)
    foundC32 = (0.0, 1.0)


    expectC32 = (1.0, 0.0)
    expectC64 = (1.0, 0.0)
    expectVecC32 = (1.0, 0.0)
    expectVecC64 = (1.0, 0.0)

    foundC32 = (0.0, 1.0)
    foundC64 = (0.0, 1.0)
    foundVecC32 = (0.0, 1.0)
    foundVecC64 = (0.0, 1.0)

    call assertEqual(expectC32, foundC32, message = 'expectC32, foundC32')
    call assertFailedAssert()
    call assertEqual(expectC32, foundC64, message = 'expectC32, foundC64')
    call assertFailedAssert()
    call assertEqual(expectC64, foundC32, message = 'expectC64, foundC32')
    call assertFailedAssert()
    call assertEqual(expectC64, foundC64, message = 'expectC64, foundC64')
    call assertFailedAssert()

    call assertEqual(expectVecC32, foundVecC32, &
               &     message = 'expectVecC32, foundVecC32')
    call assertFailedAssert()

    call assertEqual(expectVecC32, foundVecC64, &
               &     message = 'expectVecC32, foundVecC64')
    call assertFailedAssert()

    call assertEqual(expectVecC64, foundVecC32, &
               &     message = 'expectVecC64, foundVecC32')
    call assertFailedAssert()

    call assertEqual(expectVecC64, foundVecC64, &
               &     message = 'expectVecC64, foundVecC64')
    call assertFailedAssert()
                      
  end subroutine test_assertEqual_complex_failed

  subroutine test_assertEqual_complex_int()
    integer            :: expectInt
    complex (kind=c32) :: foundC32
    complex (kind=c64) :: foundC64

    expectInt = 1
    foundC32 = (1.0, 0.0)
    foundC64 = (1.0, 0.0)
    
    call assertEqual(expectInt, foundC32, message = 'expectInt, foundC32')
    call assertEqual(expectInt, foundC64, message = 'expectInt, foundC64')

  end subroutine test_assertEqual_complex_int

  subroutine test_assertEqual_complex_real()
    real (kind=r32)    :: expectR32
    real (kind=r64)    :: expectR64
    complex (kind=c32) :: foundC32
    complex (kind=c64) :: foundC64

    expectR32 = 1.0
    expectR64 = 1.0
    foundC32 = (1.0, 0.0)
    foundC64 = (1.0, 0.0)
    
    call assertEqual(expectR32, foundC32, message = 'expectR32, foundC32')
    call assertEqual(expectR64, foundC32, message = 'expectR64, foundC32')
    call assertEqual(expectR32, foundC64, message = 'expectR32, foundC64')
    call assertEqual(expectR64, foundC64, message = 'expectR64, foundC64')

  end subroutine test_assertEqual_complex_real

end module test_AssertComplex_mod
