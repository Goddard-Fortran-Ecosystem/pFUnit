#include "reflection.h"

!
! Test_AssertRealArrays_mod
!    Tests for AssertRealArrays using TestAssertReal.F90 as a model.
!
!    2013-0304 MLR: First version. 
!
!
! Note: `note name' is a mark to remind me of references to code being
!    tested.  I.e. non-boilerplate.


module Test_AssertRealArrays_mod ! note name
  use Exception_mod, only: getNumExceptions, anyExceptions
  use TestSuite_mod
  use Params_mod, only : r32, r64
  use StringUtilities_mod, only: toString
  use AssertBasic_mod
  use AssertReal_mod, only: differenceReport, shapeReport, valuesReport
  use AssertRealArrays_mod, only: assertEqual  ! note name
  use ThrowFundamentalTypes_mod, only: locationFormat
  use SourceLocation_mod

  implicit none
  private

  public :: suite

contains

  function suite()
    use TestSuite_mod, only: TestSuite, newTestSuite 
    use TestMethod_mod, only: newTestMethod

    type (TestSuite) :: suite

    suite = newTestSuite('AssertRealArraysSuite') 

#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))

    ADD(testEquals_0D1D)
    ADD(testEquals_1D_nonConformable1)
    ADD(testEquals_2D_SingleElementDifferent)
    ADD(testEquals_MultiD_SingleElementDifferent)
    ADD(testEquals_MultiD_SingleElementDifferent1)
    ADD(testEquals_MultiD_SingleElementDifferent2)
    ADD(testEquals_MultiD_SingleElementDifferent3)
    ADD(testEquals_MultiD_SingleElementDifferent4)
    ADD(testEquals_MultiD_SingleElementDifferent5)
    ADD(testEquals_MultiDMultiPrec_SingleEltDiff)
    ADD(testEquals_MultiDMultiPrec_SingleEltDiff1)
    ADD(testEquals_MultiDMultiPrec_SingleEltDiff2)
    ADD(testEquals_MultiDMultiPrec_SingleEltDiff3)
    ADD(testEquals_MultiDMultiPrec_SingleEltDiff4)
    ADD(testEquals_MultiDMultiPrec_SingleEltDiff5)
    ADD(testEquals_MultiDMultiPrec_SingleEltDiff6)
    ADD(testEquals_MultiDMultiPrec_SingleEltDiff7)
    ADD(testEquals_MultiDMultiPrec_SingleEltDiff8)
    ADD(testEquals_MultiDWithTolerance)
    ADD(testEquals_MultiDWithTolerance1)
    ADD(testEquals_MultiDWithTolerance64)
    ADD(testEquals_MultiDWithTolerance64_1)
    ADD(testEquals_MultiDWithTolerance64_2)
    ADD(testEquals_MultiDSourceLocation)

  end function suite

  ! Same rank, different shape.
  subroutine testEquals_0D1D()
    use Params_mod
!    use Assert_mod, only: assertEqual
    use AssertRealArrays_mod, only: assertEqual  ! note name

    integer :: I0
    real(kind=r32), dimension(1) :: A1
!    integer, parameter :: MAX_LEN_MSG   = 1000
    real :: expected, found
    real(kind=kind(found)) :: ONE=1
    real(kind=kind(found)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)

    I0 = 1
    A1 = 0

    expected = I0
    ! The location [1] below is the 1 here.
    found = A1(1)

    ! The following should throw an exception...
    
    call assertEqual(I0, A1, 'testEquals_0D1D')

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // '[1]' // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0. )) & 
!         & )
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <[1]>.' &
         & )

    
  end subroutine testEquals_0D1D

  ! Same rank, different shape.
  subroutine testEquals_1D_nonConformable1()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    integer, dimension(2) :: I2
    real(kind=r32), dimension(1) :: A1

    ! The following should throw an exception...
    
    call assertEqual(I2, A1, 'testEquals_2D_nonConformable1')

    call assertCatch( &
          & 'nonconforming arrays - expected shape: ' // &
          & trim(toString(shape(I2))) // ' but found shape: ' // &
          & trim(toString(shape(A1))) &
          & )
    
  end subroutine testEquals_1D_nonConformable1

  subroutine testEquals_2D_SingleElementDifferent()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real, dimension(2,2) :: A22, B22
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80

    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2

    expected = 0.0; found = 1.0

    i1 = 1; i2 = 2
    A22=expected; B22=expected
    B22(i1,i2) = found

    ! The following should throw an exception...
    call assertEqual(A22,B22,'testEquals_2D_SingleElementDifferent')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_2D_SingleElementDifferent

  subroutine testEquals_MultiD_SingleElementDifferent()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r32) :: A0, B0
    real(kind=r32), dimension(:), allocatable :: A1, B1
    real(kind=r32), dimension(:,:), allocatable :: A2, B2
    real(kind=r32), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    ! START 100
    expected = 0.0; found = 1.0

    n1 = 1; n2 = 2; allocate(b2(n1,n2))
    A0=expected; B2=expected
    i1 = 1; i2 = 2; B2(i1,i2) = found

    ! The following should throw an exception...
    call assertEqual(A0,B2,'testEquals_MultiD_SingleElementDifferent:Rank0')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiD_SingleElementDifferent

  subroutine testEquals_MultiD_SingleElementDifferent1
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r32) :: A0, B0
    real(kind=r32), dimension(:), allocatable :: A1, B1
    real(kind=r32), dimension(:,:), allocatable :: A2, B2
    real(kind=r32), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    expected = 0.0; found = 1.0

    n1 = 1; n2 = 2; allocate(a2(n1,n2),b2(n1,n2))
    A2=expected; B2=expected
    i1 = 1; i2 = 2; B2(i1,i2) = found

    ! The following should throw an exception...
    call assertEqual(A2,B2,'testEquals_MultiD_SingleElementDifferent:Rank2')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiD_SingleElementDifferent1

  subroutine testEquals_MultiD_SingleElementDifferent2
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r32) :: A0, B0
    real(kind=r32), dimension(:), allocatable :: A1, B1
    real(kind=r32), dimension(:,:), allocatable :: A2, B2
    real(kind=r32), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    expected = 0.0; found = 1.0

    n1 = 2; n2 = 3; n3 = 1; allocate(a3(n1,n2,n3),b3(n1,n2,n3))
    A3=expected; B3=expected
    i1 = 1; i2 = 2; i3 = 1; B3(i1,i2,i3) = found

    ! The following should throw an exception...
    call assertEqual(A3,B3,'testEquals_MultiD_SingleElementDifferent:Rank3')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3/) )) (/i1, i2, i3/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiD_SingleElementDifferent2

  subroutine testEquals_MultiD_SingleElementDifferent3
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r32) :: A0, B0
    real(kind=r32), dimension(:), allocatable :: A1, B1
    real(kind=r32), dimension(:,:), allocatable :: A2, B2
    real(kind=r32), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    expected = 0.0; found = 1.0

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; 
    allocate(a4(n1,n2,n3,n4),b4(n1,n2,n3,n4))
    A4=expected; B4=expected
    i1 = 1; i2 = 2; i3 = 1; i4 = 2
    B4(i1,i2,i3,i4) = found

    ! The following should throw an exception...
    call assertEqual(A4,B4,'testEquals_MultiD_SingleElementDifferent:Rank4')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4/) )) (/i1, i2, i3, i4/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiD_SingleElementDifferent3

  subroutine testEquals_MultiD_SingleElementDifferent4
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r32) :: A0, B0
    real(kind=r32), dimension(:), allocatable :: A1, B1
    real(kind=r32), dimension(:,:), allocatable :: A2, B2
    real(kind=r32), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(a5(n1,n2,n3,n4,n5),b5(n1,n2,n3,n4,n5))
    A5=expected; B5=expected
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    B5(i1,i2,i3,i4,i5) = found

    ! The following should throw an exception...
    call assertEqual(A5,B5,'testEquals_MultiD_SingleElementDifferent:Rank5')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiD_SingleElementDifferent4

  subroutine testEquals_MultiD_SingleElementDifferent5
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r32) :: A0, B0
    real(kind=r32), dimension(:), allocatable :: A1, B1
    real(kind=r32), dimension(:,:), allocatable :: A2, B2
    real(kind=r32), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5


    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(a5(n1,n2,n3,n4,n5))
    n1 = 2; n2 = 3; n3 = 2; n4 = 3; n5 = 2;
    allocate(b5(n1,n2,n3,n4,n5))
    A5=expected; B5=expected
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    B5(i1,i2,i3,i4,i5) = found

    ! The following should throw an exception...
    call assertEqual(A5,B5, & 
         & 'testEquals_MultiD_SingleElementDifferent:nonConformable')

    ! "locationInArray" is not used in the original AssertEqual code. Not needed for nonconf.
    ! write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

    call assertCatch( &
          & 'nonconforming arrays - expected shape: ' // &
          & trim(toString(shape(A5))) // ' but found shape: ' // &
          & trim(toString(shape(B5))) &
          & )

!         & 'Assertion failed: non-conformable real arrays.' // new_line('$') //&
!         & '    expected shape: <['//trim(toString(shape(A5)))//']>' // new_line('$') //&
!         & '   but found shape: <['//trim(toString(shape(B5)))//']>' &
!         )

  end subroutine testEquals_MultiD_SingleElementDifferent5

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name
    implicit none

    real(kind=r64), dimension(:), allocatable :: A1
    real(kind=r64), dimension(:,:), allocatable :: A2
    real(kind=r64), dimension(:,:,:), allocatable :: A3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5

    real(kind=r64), dimension(:), allocatable :: B1
    real(kind=r64), dimension(:,:), allocatable :: B2
    real(kind=r64), dimension(:,:,:), allocatable :: B3
    real(kind=r64), dimension(:,:,:,:), allocatable :: B4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: B5

    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    expected = 0.0; found = 1.0

    n1 = 1; n2 = 2; allocate(a2(n1,n2),b2(n1,n2))
    A2=expected; B2=expected
    i1 = 1; i2 = 2; B2(i1,i2) = found

    ! The following should throw an exception...
    call assertEqual(A2,B2,'testEquals_MultiD_SingleElementDifferent:Rank2')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )

    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )


  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff1()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name
    implicit none

    real(kind=r64), dimension(:), allocatable :: A1
    real(kind=r64), dimension(:,:), allocatable :: A2
    real(kind=r64), dimension(:,:,:), allocatable :: A3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5

    real(kind=r64), dimension(:), allocatable :: B1
    real(kind=r64), dimension(:,:), allocatable :: B2
    real(kind=r64), dimension(:,:,:), allocatable :: B3
    real(kind=r64), dimension(:,:,:,:), allocatable :: B4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: B5

    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    expected = 0.0; found = 1.0

    n1 = 2; n2 = 3; n3 = 1; allocate(a3(n1,n2,n3),b3(n1,n2,n3))
    A3=expected; B3=expected
    i1 = 1; i2 = 2; i3 = 1; B3(i1,i2,i3) = found

    ! The following should throw an exception...
    call assertEqual(A3,B3,'testEquals_MultiD_SingleElementDifferent:Rank3')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3/) )) (/i1, i2, i3/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )

    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )


  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff1

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff2()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name
    implicit none

    real(kind=r64), dimension(:), allocatable :: A1
    real(kind=r64), dimension(:,:), allocatable :: A2
    real(kind=r64), dimension(:,:,:), allocatable :: A3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5

    real(kind=r64), dimension(:), allocatable :: B1
    real(kind=r64), dimension(:,:), allocatable :: B2
    real(kind=r64), dimension(:,:,:), allocatable :: B3
    real(kind=r64), dimension(:,:,:,:), allocatable :: B4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: B5

    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    expected = 0.0; found = 1.0

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; 
    allocate(a4(n1,n2,n3,n4),b4(n1,n2,n3,n4))
    A4=expected; B4=expected
    i1 = 1; i2 = 2; i3 = 1; i4 = 2
    B4(i1,i2,i3,i4) = found

    ! The following should throw an exception...
    call assertEqual(A4,B4,'testEquals_MultiD_SingleElementDifferent:Rank4')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4/) )) (/i1, i2, i3, i4/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )

    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )


  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff2

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff3()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name
    implicit none

    real(kind=r64), dimension(:), allocatable :: A1
    real(kind=r64), dimension(:,:), allocatable :: A2
    real(kind=r64), dimension(:,:,:), allocatable :: A3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5

    real(kind=r64), dimension(:), allocatable :: B1
    real(kind=r64), dimension(:,:), allocatable :: B2
    real(kind=r64), dimension(:,:,:), allocatable :: B3
    real(kind=r64), dimension(:,:,:,:), allocatable :: B4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: B5

    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5



    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(a5(n1,n2,n3,n4,n5),b5(n1,n2,n3,n4,n5))
    A5=expected; B5=expected
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    B5(i1,i2,i3,i4,i5) = found

    ! The following should throw an exception...
    call assertEqual(A5,B5,'testEquals_MultiD_SingleElementDifferent:Rank5')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )

    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )


  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff3

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff4()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name
    implicit none

    real(kind=r64), dimension(:), allocatable :: A1
    real(kind=r64), dimension(:,:), allocatable :: A2
    real(kind=r64), dimension(:,:,:), allocatable :: A3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5

    real(kind=r64), dimension(:), allocatable :: B1
    real(kind=r64), dimension(:,:), allocatable :: B2
    real(kind=r64), dimension(:,:,:), allocatable :: B3
    real(kind=r64), dimension(:,:,:,:), allocatable :: B4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: B5

    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

!    deallocate(a5,b5)
    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(a5(n1,n2,n3,n4,n5))
    n1 = 2; n2 = 3; n3 = 2; n4 = 3; n5 = 2;
    allocate(b5(n1,n2,n3,n4,n5))
    A5=expected; B5=expected
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    B5(i1,i2,i3,i4,i5) = found

    ! The following should throw an exception...
    call assertEqual(A5,B5, & 
         & 'testEquals_MultiD_SingleElementDifferent:Rank5:NonConformable')

    ! "locationInArray" is not used in the original AssertEqual code. Not needed for nonconf.
    ! write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

    call assertCatch( &
          & 'nonconforming arrays - expected shape: ' // &
          & trim(toString(shape(A5))) // ' but found shape: ' // &
          & trim(toString(shape(B5))) &
          & )

!         & 'Assertion failed: non-conformable real arrays.' // new_line('$') //&
!         & '    expected shape: <['//trim(toString(shape(A5)))//']>' // new_line('$') //&
!         & '   but found shape: <['//trim(toString(shape(B5)))//']>' &
!         )

  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff4

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff5()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name
    implicit none

    real(kind=r64), dimension(:), allocatable :: A1
    real(kind=r64), dimension(:,:), allocatable :: A2
    real(kind=r64), dimension(:,:,:), allocatable :: A3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5

    real(kind=r32), dimension(:), allocatable :: B1
    real(kind=r32), dimension(:,:), allocatable :: B2
    real(kind=r32), dimension(:,:,:), allocatable :: B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: B5

    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    expected = 0.0; found = 1.0

    n1 = 1; n2 = 2; allocate(a2(n1,n2),b2(n1,n2))
    A2=expected; B2=expected
    i1 = 1; i2 = 2; B2(i1,i2) = found

    ! The following should throw an exception...
    call assertEqual(A2,B2,'testEquals_MultiD_SingleElementDifferent:Rank2')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )

    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )


  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff5

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff6()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name
    implicit none

    real(kind=r64), dimension(:), allocatable :: A1
    real(kind=r64), dimension(:,:), allocatable :: A2
    real(kind=r64), dimension(:,:,:), allocatable :: A3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5

    real(kind=r32), dimension(:), allocatable :: B1
    real(kind=r32), dimension(:,:), allocatable :: B2
    real(kind=r32), dimension(:,:,:), allocatable :: B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: B5

    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    expected = 1.0; found = -1.0

    n1 = 2; n2 = 3; n3 = 1; allocate(a3(n1,n2,n3),b3(n1,n2,n3))
    A3=expected; B3=expected
    i1 = 1; i2 = 2; i3 = 1; B3(i1,i2,i3) = found 

    ! The following should throw an exception...
    call assertEqual(A3,B3,'testEquals_MultiD_SingleElementDifferent:Rank3')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3/) )) (/i1, i2, i3/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )

    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )



  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff6

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff7()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name
    implicit none

    real(kind=r64), dimension(:), allocatable :: A1
    real(kind=r64), dimension(:,:), allocatable :: A2
    real(kind=r64), dimension(:,:,:), allocatable :: A3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5

    real(kind=r32), dimension(:), allocatable :: B1
    real(kind=r32), dimension(:,:), allocatable :: B2
    real(kind=r32), dimension(:,:,:), allocatable :: B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: B5

    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    expected = 1.0; found = -1.0

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; 
    allocate(a4(n1,n2,n3,n4),b4(n1,n2,n3,n4))
    A4=expected; B4=expected
    i1 = 1; i2 = 2; i3 = 1; i4 = 2
    B4(i1,i2,i3,i4) = found

    ! The following should throw an exception...
    call assertEqual(A4,B4,'testEquals_MultiD_SingleElementDifferent:Rank4')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4/) )) (/i1, i2, i3, i4/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff7

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff8()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name
    implicit none

    real(kind=r64), dimension(:), allocatable :: A1
    real(kind=r64), dimension(:,:), allocatable :: A2
    real(kind=r64), dimension(:,:,:), allocatable :: A3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5

    real(kind=r32), dimension(:), allocatable :: B1
    real(kind=r32), dimension(:,:), allocatable :: B2
    real(kind=r32), dimension(:,:,:), allocatable :: B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: B5

    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    expected = 1.0; found = -1.0

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(a5(n1,n2,n3,n4,n5),b5(n1,n2,n3,n4,n5))
    A5=expected; B5=expected
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    B5(i1,i2,i3,i4,i5) = found

    ! The following should throw an exception...
    call assertEqual(A5,B5,'testEquals_MultiD_SingleElementDifferent:Rank5')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0.)) &
!         & )
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )


    deallocate(a5,b5)
    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(a5(n1,n2,n3,n4,n5))
    n1 = 2; n2 = 3; n3 = 2; n4 = 3; n5 = 2;
    allocate(b5(n1,n2,n3,n4,n5))
    A5=expected; B5=expected
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    B5(i1,i2,i3,i4,i5) = found

    ! The following should throw an exception...
    call assertEqual(A5,B5, & 
         & 'testEquals_MultiD_SingleElementDifferent:Rank5:NonConformable')

    ! "locationInArray" is not used in the original AssertEqual code. Not needed for nonconf.
    ! write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

    call assertCatch( &
          & 'nonconforming arrays - expected shape: ' // &
          & trim(toString(shape(A5))) // ' but found shape: ' // &
          & trim(toString(shape(B5))) &
          & )

!         & 'Assertion failed: non-conformable real arrays.' // new_line('$') //&
!         & '    expected shape: <['//trim(toString(shape(A5)))//']>' // new_line('$') //&
!         & '   but found shape: <['//trim(toString(shape(B5)))//']>' &
!         )

  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff8

  subroutine testEquals_MultiDWithTolerance()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r32), dimension(:), allocatable :: A1, B1
    real(kind=r32), dimension(:,:), allocatable :: A2, B2
    real(kind=r32), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5
    real(kind=r32)    :: tolerance32

    expected = 0.0; 

    n1 = 1; n2 = 2; allocate(a2(n1,n2),b2(n1,n2))
    A2=expected; B2=expected
    i1 = 1; i2 = 2; 

    tolerance32 = 0.01
    found = expected + tolerance32*2.0
    B2(i1,i2) = found

    ! The following should throw an exception...
    call assertEqual(A2,B2,tolerance = tolerance32, message = &
         & 'testEquals_MultiDSingleEltTol32-Throw:Rank2,Tolerance32')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0. )) &
!         & )
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, tolerance32 )) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )


  end subroutine testEquals_MultiDWithTolerance

  subroutine testEquals_MultiDWithTolerance1()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r32), dimension(:), allocatable :: A1, B1
    real(kind=r32), dimension(:,:), allocatable :: A2, B2
    real(kind=r32), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r32), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r32), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5
    real(kind=r32)    :: tolerance32

    expected = 0.0; 

    n1 = 1; n2 = 2; allocate(a2(n1,n2),b2(n1,n2))
    A2=expected; B2=expected
    i1 = 1; i2 = 2; 

    tolerance32 = 0.01
    found = expected + tolerance32*2.0
    B2(i1,i2) = found
    tolerance32 = 0.01
    found = expected + tolerance32/2.0
    B2(i1,i2) = found

    ! The following should not throw an exception...
    call assertEqual(A2,B2,tolerance = tolerance32, message = &
         & 'testEquals_MultiDSingleEltTol32-NoThrow:Rank2,Tolerance32')

    call assertCatch( "" )

  end subroutine testEquals_MultiDWithTolerance1

  subroutine testEquals_MultiDWithTolerance64()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r64), dimension(:), allocatable :: A1, B1
    real(kind=r64), dimension(:,:), allocatable :: A2, B2
    real(kind=r64), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5
    real(kind=r64)    :: tolerance64

    expected = 0.0; 

    n1 = 1; n2 = 2; allocate(a2(n1,n2),b2(n1,n2))
    A2=expected; B2=expected
    i1 = 1; i2 = 2; 

    tolerance64 = 0.01
    found = expected + tolerance64*2.0
    B2(i1,i2) = found

    ! The following should throw an exception...
    call assertEqual(A2,B2,tolerance = tolerance64, message = &
         & 'testEquals_MultiDSingleEltTol64-Throw:Rank2,Tolerance64')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0. )) &
!         & )

! Fix the need for the real below.  Note we're just reporting at this stage, not calculating.
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, real(tolerance64))) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

end subroutine testEquals_MultiDWithTolerance64

  subroutine testEquals_MultiDWithTolerance64_1()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r64), dimension(:), allocatable :: A1, B1
    real(kind=r64), dimension(:,:), allocatable :: A2, B2
    real(kind=r64), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5
    real(kind=r64)    :: tolerance64

    expected = 0.0; 

    n1 = 1; n2 = 2; allocate(a2(n1,n2),b2(n1,n2))
    A2=expected; B2=expected
    i1 = 1; i2 = 2; 

    tolerance64 = 0.01
    found = expected + tolerance64/2.0
    B2(i1,i2) = found

    ! The following should not throw an exception...
    call assertEqual(A2,B2,tolerance = tolerance64, message = &
         & 'testEquals_MultiDSingleEltTol64-NoThrow:Rank2,Tolerance64')

    call assertCatch( "" )

  end subroutine testEquals_MultiDWithTolerance64_1


  subroutine testEquals_MultiDWithTolerance64_2()
    use Params_mod
!    use Assert_mod, only: assertEqual
!    use AssertRealArrays_mod, only: assertEqual  ! note name
    use AssertRealArrays_mod, only: assertEqual  ! note name

    real(kind=r64), dimension(:), allocatable :: A1, B1
    real(kind=r64), dimension(:,:), allocatable :: A2, B2
    real(kind=r64), dimension(:,:,:), allocatable :: A3, B3
    real(kind=r64), dimension(:,:,:,:), allocatable :: A4, B4
    real(kind=r64), dimension(:,:,:,:,:), allocatable :: A5, B5
    real :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5
    real(kind=r64)    :: tolerance64


    n1 = 1; n2 = 2; n3 = 2; allocate(a3(n1,n2,n3),b3(n1,n2,n3))
    A3=expected; B3=expected
    i1 = 1; i2 = 2; i3 = 1

    tolerance64 = 0.01
    found = expected + tolerance64*2.0
    B3(i1,i2,i3) = found

    ! The following should throw an exception...
    call assertEqual(A3,B3,tolerance=tolerance64, message= &
         & 'testEquals_MultiDSingleEltTol64-Throw:Rank3,Tolerance64')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3/) )) (/i1, i2, i3/)

!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, 0. )) &
!         & )
    call assertCatch( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(found - expected, real(tolerance64))) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiDWithTolerance64_2

  subroutine testEquals_MultiDSourceLocation()
    use Params_mod
    use AssertRealArrays_mod, only: AssertEqual

    real(kind=r64), dimension(:,:), allocatable :: A2, B2
    real(kind=r64) :: expected, found, tolerance64

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2
    integer :: n1, n2
    type (SourceLocation) :: location
    
    expected = 0.0; found = 1.0; tolerance64 = 0.0

    n1 = 2; n2 = 3; allocate(A2(n1,n2),B2(n1,n2))
    A2 = expected; B2 = expected

    i1 = 2; i2 = 3; B2(i1,i2) = found

    location = SourceLocation(lineNumber=999,fileName='AFileName')

    ! The following should throw an exception...
    call assertEqual(A2,B2,tolerance = tolerance64, message = &
         & 'testEquals_MultiDSourceLocation', &
         & location=location)

    ! location = SourceLocation(lineNumber=998,fileName='AFileName2')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

! Note use of real...  Consider overloading the reporting functions...
!    call assertCatch( &
!         & 'Assertion failed: unequal arrays.' // new_line('$') // &
!         & '  First difference at element <' // trim(locationInArray) // '>' // &
!         & trim(valuesReport(real(expected), real(found))) // &
!         & trim(differenceReport(real(found - expected), 0. )), &
!         & location=location &
!         & )
    call assertCatch( &
         & trim(valuesReport(real(expected), real(found))) // &
         & '; ' // trim(differenceReport(real(found - expected), 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.', &
         & location=location &
         & )

  end subroutine testEquals_MultiDSourceLocation

  ! Check to see that the test result is as expected...
  subroutine assertCatch(string,location)
    use Params_mod
    use Exception_mod, only: getNumExceptions, Exception, catchAny
    use Assert_mod, only: assertEqual
    character(len=*), intent(in) :: string
    type (SourceLocation), optional, intent(in) :: location
    type (Exception) :: anException

    if (getNumExceptions() > 0) then
       anException = catchAny()

       !, 'exceptions do not match')
       call assertEqual(string,anException%getMessage()) ! ,message='Exception message test')
       if(present(location))then
          call assertEqual( &
               & location%lineNumber,anException%getLineNumber(), &
               & message='Source line number test')
          call assertEqual(location%fileName,anException%getFileName(), &
               & message='Source file name test')
       end if
    else
       !, 'missing exception')
       call assertEqual(string, ' ')
    end if
  end subroutine assertCatch

  
end module Test_AssertRealArrays_mod

  
