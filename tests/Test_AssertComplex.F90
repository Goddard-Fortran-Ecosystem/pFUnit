#include "reflection.h"

!
! Test_AssertComplex_mod
!    Were tests for AssertReal using TestAssertReal.F90 as a model.
!
!    2013-0425 MLR: First version. 
!
!
! Note: `note name' is a mark to remind me of references to code being
!    tested.  I.e. non-boilerplate.

module Test_AssertComplex_mod ! note name
  use Exception_mod, only: getNumExceptions, anyExceptions
  use TestSuite_mod
  use Params_mod, only : r32, r64
  use StringUtilities_mod, only: toString
  use AssertBasic_mod
  use AssertReal_mod, only: assertEqual, differenceReport, valuesReport
  use AssertComplex_mod, only: assertEqual
  use ThrowFundamentalTypes_mod, only: locationFormat
! , differenceReport, valuesReport
  use SourceLocation_mod

  implicit none
  private

  public :: suite

  complex(kind=r32), parameter :: good = (42.0,24.0)
  complex(kind=r32), parameter :: bad  = (-666,-999)

contains

  function suite()
    use TestSuite_mod, only: TestSuite, newTestSuite 
    use TestMethod_mod, only: newTestMethod

    type (TestSuite) :: suite

    suite = newTestSuite('AssertComplexSuite') 

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

    integer :: expected
    integer, parameter :: good = 42
    complex(kind=r32), parameter :: z_good = good
    complex(kind=r32), dimension(1) :: found

    expected = good
    ! The location [1] below is the 1 here.
    found = bad

    ! The following should throw an exception...
    
    call assertEqual(expected, found, 'testEquals_0D1D')

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - z_good, 0.)) // &
         & ';  first difference at element <[1]>.' &
         & )
    
  end subroutine testEquals_0D1D

  ! Same rank, different shape.
  subroutine testEquals_1D_nonConformable1()
    use Params_mod
!    use Assert_mod, only: assertEqual


    integer, dimension(2) :: expected
    complex(kind=r32), dimension(1) :: found

    ! The following should throw an exception...

    expected = good; found = good
    
    call assertEqual(expected, found, 'testEquals_2D_nonConformable1')

    call assertCatch( &
          & 'nonconforming arrays - expected shape: ' // &
          & trim(toString(shape(expected))) // ' but found shape: ' // &
          & trim(toString(shape(found))) &
          & )
    
  end subroutine testEquals_1D_nonConformable1

  subroutine testEquals_2D_SingleElementDifferent()
    use Params_mod
!    use Assert_mod, only: assertEqual


    complex, dimension(2,2) :: expected, found

    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2

    i1 = 1; i2 = 2; expected=good; found=good; found(i1,i2) = bad

    !dbg1 print *,'1000'

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_2D_SingleElementDifferent')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

    call assertCatch( &
         & trim(valuesReport(good,bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_2D_SingleElementDifferent

  subroutine testEquals_MultiD_SingleElementDifferent()
    use Params_mod
!    use Assert_mod, only: assertEqual

    real(kind=r32) :: expected
    complex(kind=r32), dimension(:,:), allocatable :: found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2
    integer :: n1, n2

    !dbg1 print *,'2000'

    n1 = 1; n2 = 2; allocate(found(n1,n2))
    expected=good; found=expected
    i1 = 1; i2 = 2; found(i1,i2) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank0')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

    call assertCatch( &
         & trim(valuesReport(real(good), bad)) // &
         & '; ' // trim(differenceReport(bad - real(good), 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiD_SingleElementDifferent

  subroutine testEquals_MultiD_SingleElementDifferent1
    use Params_mod
!    use Assert_mod, only: assertEqual

    real(kind=r32), dimension(:,:), allocatable :: found
    complex(kind=r32), dimension(:,:), allocatable :: expected

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2
    integer :: n1, n2

    !dbg1 print *,'3000'

    n1 = 1; n2 = 2; allocate(expected(n1,n2),found(n1,n2))
    expected = real(good); found = expected; i1 = 1; i2 = 2; found(i1,i2) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank2')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

    call assertCatch( &
         & trim(valuesReport(real(good),real(bad))) // &
         & '; ' // trim(differenceReport(cmplx(real(bad - good)), 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiD_SingleElementDifferent1

  subroutine testEquals_MultiD_SingleElementDifferent2
    use Params_mod
!    use Assert_mod, only: assertEqual

    complex(kind=r32), dimension(:,:,:), allocatable :: expected
    real(kind=r32), dimension(:,:,:), allocatable :: found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3
    integer :: n1, n2, n3

    !dbg1 print *,'4000'

    n1 = 2; n2 = 3; n3 = 1; allocate(expected(n1,n2,n3),found(n1,n2,n3))
    expected = good; found = real(good);
    i1 = 1; i2 = 1; i3 = 1; found(i1,i2,i3) = real(good)

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank3')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3/) )) (/i1, i2, i3/)

    call assertCatch( &
         & trim(valuesReport(good, real(good))) // &
         & '; ' // trim(differenceReport(real(good) - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiD_SingleElementDifferent2

  subroutine testEquals_MultiD_SingleElementDifferent3
    use Params_mod
!    use Assert_mod, only: assertEqual

    complex(kind=r32), dimension(:,:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4
    integer :: n1, n2, n3, n4

    !dbg1 print *,'5000'

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; 
    allocate(expected(n1,n2,n3,n4),found(n1,n2,n3,n4))
    expected = good; found = good;
    i1 = 1; i2 = 2; i3 = 1; i4 = 2
    found(i1,i2,i3,i4) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank4')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4/) )) (/i1, i2, i3, i4/)

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiD_SingleElementDifferent3

  subroutine testEquals_MultiD_SingleElementDifferent4
    use Params_mod
!    use Assert_mod, only: assertEqual

    complex(kind=r32), dimension(:,:,:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(expected(n1,n2,n3,n4,n5),found(n1,n2,n3,n4,n5))
    expected=good; found=good
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    found(i1,i2,i3,i4,i5) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank5')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiD_SingleElementDifferent4

  subroutine testEquals_MultiD_SingleElementDifferent5
    use Params_mod
!    use Assert_mod, only: assertEqual

    complex(kind=r32), dimension(:,:,:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(expected(n1,n2,n3,n4,n5))
    n1 = 2; n2 = 3; n3 = 2; n4 = 3; n5 = 2;
    allocate(found(n1,n2,n3,n4,n5))
    expected=good; found=good
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    found(i1,i2,i3,i4,i5) = bad

    !dbg2 print *,'10000'

    ! The following should throw an exception...
    call assertEqual(expected,found, &
         & 'testEquals_MultiD_SingleElementDifferent:nonConformable')

    ! "locationInArray" is not used in the original AssertEqual code. Not needed for nonconf.
    ! write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

    call assertCatch( &
          & 'nonconforming arrays - expected shape: ' // &
          & trim(toString(shape(expected))) // ' but found shape: ' // &
          & trim(toString(shape(found))) &
          & )

  end subroutine testEquals_MultiD_SingleElementDifferent5

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    !dbg3 print *,'11000'

    n1 = 1; n2 = 2; allocate(expected(n1,n2),found(n1,n2))
    expected = good; found = good
    i1 = 1; i2 = 2; found(i1,i2) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank2')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )


  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff1()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    !dbg3 print *,'12000'

    n1 = 2; n2 = 3; n3 = 1; allocate(expected(n1,n2,n3),found(n1,n2,n3))
    expected = good; found = good
    i1 = 1; i2 = 2; i3 = 1; found(i1,i2,i3) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank3')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3/) )) (/i1, i2, i3/)

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff1

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff2()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4
    integer :: n1, n2, n3, n4

    !dbg3 print *,'13000'

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; 
    allocate(expected(n1,n2,n3,n4),found(n1,n2,n3,n4))
    expected = good; found = good;
    i1 = 1; i2 = 2; i3 = 1; i4 = 2
    found(i1,i2,i3,i4) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank4')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4/) )) (/i1, i2, i3, i4/)

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )


  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff2

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff3()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:,:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(expected(n1,n2,n3,n4,n5),found(n1,n2,n3,n4,n5))
    expected = good; found = good
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    found(i1,i2,i3,i4,i5) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank5')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff3

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff4()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:,:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5


    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(expected(n1,n2,n3,n4,n5))
    n1 = 2; n2 = 3; n3 = 2; n4 = 3; n5 = 2;
    allocate(found(n1,n2,n3,n4,n5))
    expected = good; found = good
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    found(i1,i2,i3,i4,i5) = bad

    ! The following should throw an exception...
    call assertEqual(expected, found, &
         & 'testEquals_MultiD_SingleElementDifferent:Rank5:NonConformable')

    ! "locationInArray" is not used in the original AssertEqual code. Not needed for nonconf.
    ! write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

    call assertCatch( &
          & 'nonconforming arrays - expected shape: ' // &
          & trim(toString(shape(expected))) // ' but found shape: ' // &
          & trim(toString(shape(found))) &
          & )

  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff4

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff5()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2
    integer :: n1, n2


    n1 = 1; n2 = 2; allocate(expected(n1,n2),found(n1,n2))
    expected = good; found = good
    i1 = 1; i2 = 2; found(i1,i2) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank2')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff5

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff6()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3
    integer :: n1, n2, n3

    n1 = 2; n2 = 3; n3 = 1; allocate(expected(n1,n2,n3),found(n1,n2,n3))
    expected = good; found = good
    i1 = 1; i2 = 2; i3 = 1; found(i1,i2,i3) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank3')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3/) )) (/i1, i2, i3/)

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff6

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff7()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4
    integer :: n1, n2, n3, n4

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; 
    allocate(expected(n1,n2,n3,n4),found(n1,n2,n3,n4))
    expected = good; found = good
    i1 = 1; i2 = 2; i3 = 1; i4 = 2
    found(i1,i2,i3,i4) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank4')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4/) )) (/i1, i2, i3, i4/)

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff7

  subroutine testEquals_MultiDMultiPrec_SingleEltDiff8()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:,:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3, i4, i5
    integer :: n1, n2, n3, n4, n5

    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(expected(n1,n2,n3,n4,n5),found(n1,n2,n3,n4,n5))
    expected=good; found=good
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    found(i1,i2,i3,i4,i5) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found,'testEquals_MultiD_SingleElementDifferent:Rank5')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

    call assertCatch( &
         & trim(valuesReport(good, bad)) // &
         & '; ' // trim(differenceReport(bad - good, 0.)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

    deallocate(expected,found)
    n1 = 2; n2 = 3; n3 = 2; n4 = 2; n5 = 2;
    allocate(expected(n1,n2,n3,n4,n5))
    n1 = 2; n2 = 3; n3 = 2; n4 = 3; n5 = 2;
    allocate(found(n1,n2,n3,n4,n5))
    expected=good; found=good
    i1 = 1; i2 = 2; i3 = 1; i4 = 2; i5 = 1
    found(i1,i2,i3,i4,i5) = bad

    ! The following should throw an exception...
    call assertEqual(expected,found, & 
         & 'testEquals_MultiD_SingleElementDifferent:Rank5:NonConformable')

    ! "locationInArray" is not used in the original AssertEqual code. Not needed for nonconf.
    ! write(locationInArray,locationFormat( (/i1,i2,i3,i4,i5/) )) (/i1, i2, i3, i4, i5/)

    call assertCatch( &
          & 'nonconforming arrays - expected shape: ' // &
          & trim(toString(shape(expected))) // ' but found shape: ' // &
          & trim(toString(shape(found))) &
          & )


  end subroutine testEquals_MultiDMultiPrec_SingleEltDiff8

  subroutine testEquals_MultiDWithTolerance()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r32), dimension(:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2
    integer :: n1, n2
    real(kind=r32) :: tolerance32, bad32

    n1 = 1; n2 = 2; allocate(expected(n1,n2),found(n1,n2))
    expected = good; found = good;

    tolerance32 = 0.01
    bad32 = good + tolerance32*2.0

    i1 = 1; i2 = 2; 
    found(i1,i2) = bad32

    ! The following should throw an exception...
    call assertEqual(expected,found,tolerance = tolerance32, message = &
         & 'testEquals_MultiDSingleEltTol32-Throw:Rank2,Tolerance32')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

    call assertCatch( &
         & trim(valuesReport(good, cmplx(bad32))) // &
         & '; ' // trim(differenceReport(bad32 - good, tolerance32)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiDWithTolerance

  subroutine testEquals_MultiDWithTolerance1()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r32), dimension(:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2
    integer :: n1, n2
    complex(kind=r32)    :: bad32
    real(kind=r32)       :: tolerance32

    n1 = 1; n2 = 2; allocate(expected(n1,n2),found(n1,n2))
    expected = good; found = good;

    tolerance32 = 0.01
    bad32 = good + tolerance32/2.0

    i1 = 1; i2 = 2; 
    found(i1,i2) = bad32

    ! The following should not throw an exception...
    call assertEqual(expected,found,tolerance = tolerance32, message = &
         & 'testEquals_MultiDSingleEltTol32-NoThrow:Rank2,Tolerance32')

    call assertCatch( "" )

  end subroutine testEquals_MultiDWithTolerance1

  subroutine testEquals_MultiDWithTolerance64()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:), allocatable :: expected, found
    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2
    integer :: n1, n2
    real(kind=r64)    :: tolerance64
    complex(kind=r64) :: good64, bad64

    good64 = good

    n1 = 1; n2 = 2; allocate(expected(n1,n2),found(n1,n2))
    expected = good; found = good;

    tolerance64 = 0.01
    bad64 = good64 + tolerance64*2.0

    i1 = 1; i2 = 2; 
    found(i1,i2) = bad64

    ! The following should throw an exception...
    call assertEqual(expected,found,tolerance = tolerance64, message = &
         & 'testEquals_MultiDSingleEltTol64-Throw:Rank2,Tolerance64')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

! Fix the need for the real below.  Note we're just reporting at this stage, not calculating.
    call assertCatch( &
         & trim(valuesReport(good64, bad64)) // &
         & '; ' // trim(differenceReport(bad64 - good64, tolerance64)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

end subroutine testEquals_MultiDWithTolerance64

  subroutine testEquals_MultiDWithTolerance64_1()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2
    integer :: n1, n2
    real(kind=r64)    :: tolerance64, good64, bad64

    good64 = good

    n1 = 1; n2 = 2; allocate(expected(n1,n2),found(n1,n2))
    expected = good64; found = good64;

    tolerance64 = 0.01
    bad64 = good64 + tolerance64/2.0

    i1 = 1; i2 = 2; 
    found(i1,i2) = bad64

    ! The following should not throw an exception...
    call assertEqual(expected,found,tolerance = tolerance64, message = &
         & 'testEquals_MultiDSingleEltTol64-NoThrow:Rank2,Tolerance64')

    call assertCatch( "" )

  end subroutine testEquals_MultiDWithTolerance64_1


  subroutine testEquals_MultiDWithTolerance64_2()
    use Params_mod
!    use Assert_mod, only: assertEqual
    implicit none

    complex(kind=r64), dimension(:,:,:), allocatable :: expected, found

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2, i3
    integer :: n1, n2, n3
    real(kind=r64)    :: tolerance64
    complex(kind=r64) :: good64, bad64

    good64 = good

    n1 = 1; n2 = 2; n3 = 2; allocate(expected(n1,n2,n3),found(n1,n2,n3))
    expected = good64; found = good64

    tolerance64 = 0.01
    bad64 = good64 + tolerance64*2.0

    i1 = 1; i2 = 2; i3 = 1
    found(i1,i2,i3) = bad64

    ! The following should throw an exception...
    call assertEqual(expected,found,tolerance=tolerance64, message= &
         & 'testEquals_MultiDSingleEltTol64-Throw:Rank3,Tolerance64')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2,i3/) )) (/i1, i2, i3/)

    call assertCatch( &
         & trim(valuesReport(good64, bad64)) // &
         & '; ' // trim(differenceReport(bad64 - good64, tolerance64)) // &
         & ';  first difference at element <' // trim(locationInArray) // '>.' &
         & )

  end subroutine testEquals_MultiDWithTolerance64_2

  subroutine testEquals_MultiDSourceLocation()
    use Params_mod
    implicit none

    complex(kind=r64), dimension(:,:), allocatable :: expected, found
    complex(kind=r64) :: good64, bad64
    real(kind=r64) :: tolerance64

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80
    character(len=MAXLEN_SHAPE) :: locationInArray
    integer :: i1, i2
    integer :: n1, n2
    type (SourceLocation) :: location

    good64 = good
    
    n1 = 2; n2 = 3; allocate(expected(n1,n2),found(n1,n2))
    expected = good64; found = good64

    tolerance64 = 0.01
    bad64 = good64 + tolerance64*2.0

    i1 = 2; i2 = 3; found(i1,i2) = bad64

    location = SourceLocation(lineNumber=999,fileName='AFileName')

    ! The following should throw an exception...
    call assertEqual(expected,found,tolerance = tolerance64, message = &
         & 'testEquals_MultiDSourceLocation', &
         & location=location)

    ! location = SourceLocation(lineNumber=998,fileName='AFileName2')

    ! "locationInArray" is not used in the original AssertEqual code.
    write(locationInArray,locationFormat( (/i1,i2/) )) (/i1, i2/)

! Note use of real...  Consider overloading the reporting functions...
    call assertCatch( &
         & trim(valuesReport(good64, bad64)) // &
         & '; ' // trim(differenceReport(bad64 - good64, tolerance64)) // &
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

  
end module Test_AssertComplex_mod

  
