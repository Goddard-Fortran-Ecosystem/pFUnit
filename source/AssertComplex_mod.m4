changequote([,])

define([FULLTYPE],[ifelse($1,int,integer,
                          $1,char,character,$1)])
define([TOLOWER],[translit($1,'A-Z','a-z')])
define([TOUPPER],[translit($1,'a-z','A-Z')])
define([KINDATTRIBUTE],[ifelse(TOLOWER($1),complex,(kind=c$2),
                        ifelse(TOLOWER($1),real,(kind=r$2)))])

define([DIMS],[ifelse($1,0,,
                      $1,1,(:),
                      $1,2,(:,:),
                      $1,3,(:,:,:),
                      $1,4,(:,:,:,:),
                      $1,5,(:,:,:,:,:))])

define([DECLARE],[FULLTYPE($2) KINDATTRIBUTE($2,$3), intent(in) :: $1[]DIMS($4)])
define([OVERLOAD],[$1[]TOLOWER(_$2_$3_$4)D])
define([DECLAREPOINTER],[FULLTYPE($1) KINDATTRIBUTE($1,$2), pointer :: OVERLOAD(p,$1,$2,$3)[]DIMS($3) => null()])

define([ALLCASES],[
$1(int,default,0)
$1(int,default,1)
$1(int,default,2)
$1(int,default,3)
$1(real,sp,0)
$1(real,sp,1)
$1(real,sp,2)
$1(real,sp,3)
$1(real,dp,0)
$1(real,dp,1)
$1(real,dp,2)
$1(real,dp,3)
])

define([FIRST],[$1])
define([SECOND],[$2])
define([THIRD],[$3])
define([TYPE],[FIRST($1)])
define([KIND],[SECOND($1)])
define([RANK],[THIRD($1)])

define([NAME],[ifelse([$1],complex,c,
               ifelse([$1],real,r,int))[]ifelse([$2],default,,$2)_[]$3[]D])

! assert (tkr_expected, tkr_found)
define([assert],[
  !---------------------------------------------------------------------------
  !> Asserts that two complex numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected complex numbers
  !! @param found -  found complex numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two complex numbers are not equal.
  !---------------------------------------------------------------------------
  subroutine assertEqual_[]NAME($1)_[]NAME($2)_tol$3(expected, found, ifelse($3,0,,tolerance[,]) message)
[]  DECLARE(expected, TYPE($1),KIND([$1]),RANK([$1]))
[]  DECLARE(found,    TYPE($2),KIND([$2]),RANK([$2]))
    ifelse($3,0,,[real (kind=r$3), intent(in) :: tolerance])
[]  character(LEN=*), optional, intent(in) :: message

ifelse($3,0,[
[]  real (kind=kind(found)) :: tolerance
[]  real (kind=kind(found)) :: ONE=1
[]  real (kind=kind(found)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)])

[]  character(LEN=MAX_LEN_MSG) :: message_
[]  logical :: conformable
[]  integer :: first
[]  complex(kind=c64) :: delta ifelse(RANK([$2]),1,(size(found)),
RANK([$2]),2,(size(found,1),size(found,2)),
RANK([$2]),3,(size(found,1),size(found,2),size(found,3)),
RANK([$2]),4,(size(found,1),size(found,2),size(found,3),size(found,4)),
RANK([$2]),5,(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5)))
[]  integer :: shapeExpected(RANK([$1]))
[]  integer :: shapeFound(RANK([$2]))

  ifelse($3,0,tolerance = DEFAULT_TOLERANCE,)

    shapeExpected = shape(expected)
    shapeFound = shape(found)
    if (.not. areConformable(shapeFound, shapeExpected)) then
       message_ = 'Arrays are not conformable.'
       call throw(Exception(message_))
       return
    end if 

    delta = expected - found
    if (isWithinTolerance(delta, real(tolerance,kind=r64), L_INFINITY_NORM)) return

    ifelse(RANK([$2]),0,
first = firstDifference(abs(expected-found) < tolerance),
first = firstDifference(reshape(abs(expected-found) < tolerance, (/ size(found) /))))

[]   message_ = 'Floating pointing (complex) scalar assertion failed:'
[]   if (present(message)) call append(message_, ' ' // trim(message))
[]   call append(message_, '' // NEW_LINE('a'))
     ifelse(RANK([$2]),1,call append(message_, '       First difference at element ' // trim(toString(first)) // '.' // NEW_LINE('a')))
     ifelse(RANK([$2]),2,,RANK([$2]),3,,RANK([$2]),4,,RANK([$2]),5,,
[]   call append(message_, '       Expected:  ' // trim(toString(expected)) //'' // NEW_LINE('a'))
[]   call append(message_, '       but found: ' // trim(toString(found)) // '' // NEW_LINE('a'))
[]   call append(message_, '       delta:     ' // trim(toString(found-expected)))
[]   call append(message_, ' > ' // trim(toString(tolerance))))


   call throw(Exception(message_))

[] end subroutine assertEqual_[]NAME($1)_[]NAME($2)_tol$3

])

!------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!------------------------------------------------------------------------------
!  MODULE: AssertComplex
!
!> @brief
!! Handles a set of assertion methods useful for writing tests in complex 
!!
!! @author
!! Joey Gurganus,  NASA/GSFC SIVO
!!
!! @date
!! 16 Apr 2008
!!
! REVISION HISTORY:
! 16 Apr 2008 - Initial Version
! 12 Jul 2010 - Added prologue for Doxygen
!------------------------------------------------------------------------------
module AssertComplex_mod
  use pFUnitException_mod
  use AssertArray_mod
  use Params_mod, only: r32, r64, c32, c64
  implicit none
  private

  public :: assertEqual
  public :: vectorNorm
  public :: isWithinTolerance
!!  public :: toString

  public :: L_INFINITY_NORM
  public :: L1_NORM
  public :: L2_NORM

  interface assertEqual

     ! Scalar
    module procedure assertEqual_int_0d_c32_0d_tol0
    module procedure assertEqual_r32_0d_c32_0d_tol0
    module procedure assertEqual_r64_0d_c32_0d_tol0
    module procedure assertEqual_c32_0d_c32_0d_tol0
    module procedure assertEqual_c64_0d_c32_0d_tol0
    module procedure assertEqual_int_0d_c64_0d_tol0
    module procedure assertEqual_r32_0d_c64_0d_tol0
    module procedure assertEqual_r64_0d_c64_0d_tol0
    module procedure assertEqual_c32_0d_c64_0d_tol0
    module procedure assertEqual_c64_0d_c64_0d_tol0

    module procedure assertEqual_int_0d_c32_0d_tol32
    module procedure assertEqual_r32_0d_c32_0d_tol32
    module procedure assertEqual_r64_0d_c32_0d_tol32
    module procedure assertEqual_c32_0d_c32_0d_tol32
    module procedure assertEqual_c64_0d_c32_0d_tol32
    module procedure assertEqual_int_0d_c64_0d_tol32
    module procedure assertEqual_r32_0d_c64_0d_tol32
    module procedure assertEqual_r64_0d_c64_0d_tol32
    module procedure assertEqual_c32_0d_c64_0d_tol32
    module procedure assertEqual_c64_0d_c64_0d_tol32

    module procedure assertEqual_int_0d_c32_0d_tol64
    module procedure assertEqual_r32_0d_c32_0d_tol64
    module procedure assertEqual_r64_0d_c32_0d_tol64
    module procedure assertEqual_c32_0d_c32_0d_tol64
    module procedure assertEqual_c64_0d_c32_0d_tol64
    module procedure assertEqual_int_0d_c64_0d_tol64
    module procedure assertEqual_r32_0d_c64_0d_tol64
    module procedure assertEqual_r64_0d_c64_0d_tol64
    module procedure assertEqual_c32_0d_c64_0d_tol64
    module procedure assertEqual_c64_0d_c64_0d_tol64

     ! 1D
    module procedure assertEqual_int_0d_c32_1d_tol0
    module procedure assertEqual_r32_0d_c32_1d_tol0
    module procedure assertEqual_r64_0d_c32_1d_tol0
    module procedure assertEqual_c32_0d_c32_1d_tol0
    module procedure assertEqual_c64_0d_c32_1d_tol0
    module procedure assertEqual_int_0d_c64_1d_tol0
    module procedure assertEqual_r32_0d_c64_1d_tol0
    module procedure assertEqual_r64_0d_c64_1d_tol0
    module procedure assertEqual_c32_0d_c64_1d_tol0
    module procedure assertEqual_c64_0d_c64_1d_tol0

    module procedure assertEqual_int_0d_c32_1d_tol32
    module procedure assertEqual_r32_0d_c32_1d_tol32
    module procedure assertEqual_r64_0d_c32_1d_tol32
    module procedure assertEqual_c32_0d_c32_1d_tol32
    module procedure assertEqual_c64_0d_c32_1d_tol32
    module procedure assertEqual_int_0d_c64_1d_tol32
    module procedure assertEqual_r32_0d_c64_1d_tol32
    module procedure assertEqual_r64_0d_c64_1d_tol32
    module procedure assertEqual_c32_0d_c64_1d_tol32
    module procedure assertEqual_c64_0d_c64_1d_tol32

    module procedure assertEqual_int_0d_c32_1d_tol64
    module procedure assertEqual_r32_0d_c32_1d_tol64
    module procedure assertEqual_r64_0d_c32_1d_tol64
    module procedure assertEqual_c32_0d_c32_1d_tol64
    module procedure assertEqual_c64_0d_c32_1d_tol64
    module procedure assertEqual_int_0d_c64_1d_tol64
    module procedure assertEqual_r32_0d_c64_1d_tol64
    module procedure assertEqual_r64_0d_c64_1d_tol64
    module procedure assertEqual_c32_0d_c64_1d_tol64
    module procedure assertEqual_c64_0d_c64_1d_tol64

    module procedure assertEqual_int_1d_c32_1d_tol0
    module procedure assertEqual_r32_1d_c32_1d_tol0
    module procedure assertEqual_r64_1d_c32_1d_tol0
    module procedure assertEqual_c32_1d_c32_1d_tol0
    module procedure assertEqual_c64_1d_c32_1d_tol0
    module procedure assertEqual_int_1d_c64_1d_tol0
    module procedure assertEqual_r32_1d_c64_1d_tol0
    module procedure assertEqual_r64_1d_c64_1d_tol0
    module procedure assertEqual_c32_1d_c64_1d_tol0
    module procedure assertEqual_c64_1d_c64_1d_tol0

    module procedure assertEqual_int_1d_c32_1d_tol32
    module procedure assertEqual_r32_1d_c32_1d_tol32
    module procedure assertEqual_r64_1d_c32_1d_tol32
    module procedure assertEqual_c32_1d_c32_1d_tol32
    module procedure assertEqual_c64_1d_c32_1d_tol32
    module procedure assertEqual_int_1d_c64_1d_tol32
    module procedure assertEqual_r32_1d_c64_1d_tol32
    module procedure assertEqual_r64_1d_c64_1d_tol32
    module procedure assertEqual_c32_1d_c64_1d_tol32
    module procedure assertEqual_c64_1d_c64_1d_tol32

    module procedure assertEqual_int_1d_c32_1d_tol64
    module procedure assertEqual_r32_1d_c32_1d_tol64
    module procedure assertEqual_r64_1d_c32_1d_tol64
    module procedure assertEqual_c32_1d_c32_1d_tol64
    module procedure assertEqual_c64_1d_c32_1d_tol64
    module procedure assertEqual_int_1d_c64_1d_tol64
    module procedure assertEqual_r32_1d_c64_1d_tol64
    module procedure assertEqual_r64_1d_c64_1d_tol64
    module procedure assertEqual_c32_1d_c64_1d_tol64
    module procedure assertEqual_c64_1d_c64_1d_tol64

     ! 2D
    module procedure assertEqual_int_0d_c32_2d_tol0
    module procedure assertEqual_r32_0d_c32_2d_tol0
    module procedure assertEqual_r64_0d_c32_2d_tol0
    module procedure assertEqual_c32_0d_c32_2d_tol0
    module procedure assertEqual_c64_0d_c32_2d_tol0
    module procedure assertEqual_int_0d_c64_2d_tol0
    module procedure assertEqual_r32_0d_c64_2d_tol0
    module procedure assertEqual_r64_0d_c64_2d_tol0
    module procedure assertEqual_c32_0d_c64_2d_tol0
    module procedure assertEqual_c64_0d_c64_2d_tol0

    module procedure assertEqual_int_0d_c32_2d_tol32
    module procedure assertEqual_r32_0d_c32_2d_tol32
    module procedure assertEqual_r64_0d_c32_2d_tol32
    module procedure assertEqual_c32_0d_c32_2d_tol32
    module procedure assertEqual_c64_0d_c32_2d_tol32
    module procedure assertEqual_int_0d_c64_2d_tol32
    module procedure assertEqual_r32_0d_c64_2d_tol32
    module procedure assertEqual_r64_0d_c64_2d_tol32
    module procedure assertEqual_c32_0d_c64_2d_tol32
    module procedure assertEqual_c64_0d_c64_2d_tol32

    module procedure assertEqual_int_0d_c32_2d_tol64
    module procedure assertEqual_r32_0d_c32_2d_tol64
    module procedure assertEqual_r64_0d_c32_2d_tol64
    module procedure assertEqual_c32_0d_c32_2d_tol64
    module procedure assertEqual_c64_0d_c32_2d_tol64
    module procedure assertEqual_int_0d_c64_2d_tol64
    module procedure assertEqual_r32_0d_c64_2d_tol64
    module procedure assertEqual_r64_0d_c64_2d_tol64
    module procedure assertEqual_c32_0d_c64_2d_tol64
    module procedure assertEqual_c64_0d_c64_2d_tol64

    module procedure assertEqual_int_2d_c32_2d_tol0
    module procedure assertEqual_r32_2d_c32_2d_tol0
    module procedure assertEqual_r64_2d_c32_2d_tol0
    module procedure assertEqual_c32_2d_c32_2d_tol0
    module procedure assertEqual_c64_2d_c32_2d_tol0
    module procedure assertEqual_int_2d_c64_2d_tol0
    module procedure assertEqual_r32_2d_c64_2d_tol0
    module procedure assertEqual_r64_2d_c64_2d_tol0
    module procedure assertEqual_c32_2d_c64_2d_tol0
    module procedure assertEqual_c64_2d_c64_2d_tol0

    module procedure assertEqual_int_2d_c32_2d_tol32
    module procedure assertEqual_r32_2d_c32_2d_tol32
    module procedure assertEqual_r64_2d_c32_2d_tol32
    module procedure assertEqual_c32_2d_c32_2d_tol32
    module procedure assertEqual_c64_2d_c32_2d_tol32
    module procedure assertEqual_int_2d_c64_2d_tol32
    module procedure assertEqual_r32_2d_c64_2d_tol32
    module procedure assertEqual_r64_2d_c64_2d_tol32
    module procedure assertEqual_c32_2d_c64_2d_tol32
    module procedure assertEqual_c64_2d_c64_2d_tol32

    module procedure assertEqual_int_2d_c32_2d_tol64
    module procedure assertEqual_r32_2d_c32_2d_tol64
    module procedure assertEqual_r64_2d_c32_2d_tol64
    module procedure assertEqual_c32_2d_c32_2d_tol64
    module procedure assertEqual_c64_2d_c32_2d_tol64
    module procedure assertEqual_int_2d_c64_2d_tol64
    module procedure assertEqual_r32_2d_c64_2d_tol64
    module procedure assertEqual_r64_2d_c64_2d_tol64
    module procedure assertEqual_c32_2d_c64_2d_tol64
    module procedure assertEqual_c64_2d_c64_2d_tol64

     ! 3D
    module procedure assertEqual_int_0d_c32_3d_tol0
    module procedure assertEqual_r32_0d_c32_3d_tol0
    module procedure assertEqual_r64_0d_c32_3d_tol0
    module procedure assertEqual_c32_0d_c32_3d_tol0
    module procedure assertEqual_c64_0d_c32_3d_tol0
    module procedure assertEqual_int_0d_c64_3d_tol0
    module procedure assertEqual_r32_0d_c64_3d_tol0
    module procedure assertEqual_r64_0d_c64_3d_tol0
    module procedure assertEqual_c32_0d_c64_3d_tol0
    module procedure assertEqual_c64_0d_c64_3d_tol0

    module procedure assertEqual_int_0d_c32_3d_tol32
    module procedure assertEqual_r32_0d_c32_3d_tol32
    module procedure assertEqual_r64_0d_c32_3d_tol32
    module procedure assertEqual_c32_0d_c32_3d_tol32
    module procedure assertEqual_c64_0d_c32_3d_tol32
    module procedure assertEqual_int_0d_c64_3d_tol32
    module procedure assertEqual_r32_0d_c64_3d_tol32
    module procedure assertEqual_r64_0d_c64_3d_tol32
    module procedure assertEqual_c32_0d_c64_3d_tol32
    module procedure assertEqual_c64_0d_c64_3d_tol32

    module procedure assertEqual_int_0d_c32_3d_tol64
    module procedure assertEqual_r32_0d_c32_3d_tol64
    module procedure assertEqual_r64_0d_c32_3d_tol64
    module procedure assertEqual_c32_0d_c32_3d_tol64
    module procedure assertEqual_c64_0d_c32_3d_tol64
    module procedure assertEqual_int_0d_c64_3d_tol64
    module procedure assertEqual_r32_0d_c64_3d_tol64
    module procedure assertEqual_r64_0d_c64_3d_tol64
    module procedure assertEqual_c32_0d_c64_3d_tol64
    module procedure assertEqual_c64_0d_c64_3d_tol64

    module procedure assertEqual_int_3d_c32_3d_tol0
    module procedure assertEqual_r32_3d_c32_3d_tol0
    module procedure assertEqual_r64_3d_c32_3d_tol0
    module procedure assertEqual_c32_3d_c32_3d_tol0
    module procedure assertEqual_c64_3d_c32_3d_tol0
    module procedure assertEqual_int_3d_c64_3d_tol0
    module procedure assertEqual_r32_3d_c64_3d_tol0
    module procedure assertEqual_r64_3d_c64_3d_tol0
    module procedure assertEqual_c32_3d_c64_3d_tol0
    module procedure assertEqual_c64_3d_c64_3d_tol0

    module procedure assertEqual_int_3d_c32_3d_tol32
    module procedure assertEqual_r32_3d_c32_3d_tol32
    module procedure assertEqual_r64_3d_c32_3d_tol32
    module procedure assertEqual_c32_3d_c32_3d_tol32
    module procedure assertEqual_c64_3d_c32_3d_tol32
    module procedure assertEqual_int_3d_c64_3d_tol32
    module procedure assertEqual_r32_3d_c64_3d_tol32
    module procedure assertEqual_r64_3d_c64_3d_tol32
    module procedure assertEqual_c32_3d_c64_3d_tol32
    module procedure assertEqual_c64_3d_c64_3d_tol32

    module procedure assertEqual_int_3d_c32_3d_tol64
    module procedure assertEqual_r32_3d_c32_3d_tol64
    module procedure assertEqual_r64_3d_c32_3d_tol64
    module procedure assertEqual_c32_3d_c32_3d_tol64
    module procedure assertEqual_c64_3d_c32_3d_tol64
    module procedure assertEqual_int_3d_c64_3d_tol64
    module procedure assertEqual_r32_3d_c64_3d_tol64
    module procedure assertEqual_r64_3d_c64_3d_tol64
    module procedure assertEqual_c32_3d_c64_3d_tol64
    module procedure assertEqual_c64_3d_c64_3d_tol64

     ! 4D
    module procedure assertEqual_int_0d_c32_4d_tol0
    module procedure assertEqual_r32_0d_c32_4d_tol0
    module procedure assertEqual_r64_0d_c32_4d_tol0
    module procedure assertEqual_c32_0d_c32_4d_tol0
    module procedure assertEqual_c64_0d_c32_4d_tol0
    module procedure assertEqual_int_0d_c64_4d_tol0
    module procedure assertEqual_r32_0d_c64_4d_tol0
    module procedure assertEqual_r64_0d_c64_4d_tol0
    module procedure assertEqual_c32_0d_c64_4d_tol0
    module procedure assertEqual_c64_0d_c64_4d_tol0

    module procedure assertEqual_int_0d_c32_4d_tol32
    module procedure assertEqual_r32_0d_c32_4d_tol32
    module procedure assertEqual_r64_0d_c32_4d_tol32
    module procedure assertEqual_c32_0d_c32_4d_tol32
    module procedure assertEqual_c64_0d_c32_4d_tol32
    module procedure assertEqual_int_0d_c64_4d_tol32
    module procedure assertEqual_r32_0d_c64_4d_tol32
    module procedure assertEqual_r64_0d_c64_4d_tol32
    module procedure assertEqual_c32_0d_c64_4d_tol32
    module procedure assertEqual_c64_0d_c64_4d_tol32

    module procedure assertEqual_int_0d_c32_4d_tol64
    module procedure assertEqual_r32_0d_c32_4d_tol64
    module procedure assertEqual_r64_0d_c32_4d_tol64
    module procedure assertEqual_c32_0d_c32_4d_tol64
    module procedure assertEqual_c64_0d_c32_4d_tol64
    module procedure assertEqual_int_0d_c64_4d_tol64
    module procedure assertEqual_r32_0d_c64_4d_tol64
    module procedure assertEqual_r64_0d_c64_4d_tol64
    module procedure assertEqual_c32_0d_c64_4d_tol64
    module procedure assertEqual_c64_0d_c64_4d_tol64

    module procedure assertEqual_int_4d_c32_4d_tol0
    module procedure assertEqual_r32_4d_c32_4d_tol0
    module procedure assertEqual_r64_4d_c32_4d_tol0
    module procedure assertEqual_c32_4d_c32_4d_tol0
    module procedure assertEqual_c64_4d_c32_4d_tol0
    module procedure assertEqual_int_4d_c64_4d_tol0
    module procedure assertEqual_r32_4d_c64_4d_tol0
    module procedure assertEqual_r64_4d_c64_4d_tol0
    module procedure assertEqual_c32_4d_c64_4d_tol0
    module procedure assertEqual_c64_4d_c64_4d_tol0

    module procedure assertEqual_int_4d_c32_4d_tol32
    module procedure assertEqual_r32_4d_c32_4d_tol32
    module procedure assertEqual_r64_4d_c32_4d_tol32
    module procedure assertEqual_c32_4d_c32_4d_tol32
    module procedure assertEqual_c64_4d_c32_4d_tol32
    module procedure assertEqual_int_4d_c64_4d_tol32
    module procedure assertEqual_r32_4d_c64_4d_tol32
    module procedure assertEqual_r64_4d_c64_4d_tol32
    module procedure assertEqual_c32_4d_c64_4d_tol32
    module procedure assertEqual_c64_4d_c64_4d_tol32

    module procedure assertEqual_int_4d_c32_4d_tol64
    module procedure assertEqual_r32_4d_c32_4d_tol64
    module procedure assertEqual_r64_4d_c32_4d_tol64
    module procedure assertEqual_c32_4d_c32_4d_tol64
    module procedure assertEqual_c64_4d_c32_4d_tol64
    module procedure assertEqual_int_4d_c64_4d_tol64
    module procedure assertEqual_r32_4d_c64_4d_tol64
    module procedure assertEqual_r64_4d_c64_4d_tol64
    module procedure assertEqual_c32_4d_c64_4d_tol64
    module procedure assertEqual_c64_4d_c64_4d_tol64

     ! 5D
    module procedure assertEqual_int_0d_c32_5d_tol0
    module procedure assertEqual_r32_0d_c32_5d_tol0
    module procedure assertEqual_r64_0d_c32_5d_tol0
    module procedure assertEqual_c32_0d_c32_5d_tol0
    module procedure assertEqual_c64_0d_c32_5d_tol0
    module procedure assertEqual_int_0d_c64_5d_tol0
    module procedure assertEqual_r32_0d_c64_5d_tol0
    module procedure assertEqual_r64_0d_c64_5d_tol0
    module procedure assertEqual_c32_0d_c64_5d_tol0
    module procedure assertEqual_c64_0d_c64_5d_tol0

    module procedure assertEqual_int_0d_c32_5d_tol32
    module procedure assertEqual_r32_0d_c32_5d_tol32
    module procedure assertEqual_r64_0d_c32_5d_tol32
    module procedure assertEqual_c32_0d_c32_5d_tol32
    module procedure assertEqual_c64_0d_c32_5d_tol32
    module procedure assertEqual_int_0d_c64_5d_tol32
    module procedure assertEqual_r32_0d_c64_5d_tol32
    module procedure assertEqual_r64_0d_c64_5d_tol32
    module procedure assertEqual_c32_0d_c64_5d_tol32
    module procedure assertEqual_c64_0d_c64_5d_tol32

    module procedure assertEqual_int_0d_c32_5d_tol64
    module procedure assertEqual_r32_0d_c32_5d_tol64
    module procedure assertEqual_r64_0d_c32_5d_tol64
    module procedure assertEqual_c32_0d_c32_5d_tol64
    module procedure assertEqual_c64_0d_c32_5d_tol64
    module procedure assertEqual_int_0d_c64_5d_tol64
    module procedure assertEqual_r32_0d_c64_5d_tol64
    module procedure assertEqual_r64_0d_c64_5d_tol64
    module procedure assertEqual_c32_0d_c64_5d_tol64
    module procedure assertEqual_c64_0d_c64_5d_tol64

    module procedure assertEqual_int_5d_c32_5d_tol0
    module procedure assertEqual_r32_5d_c32_5d_tol0
    module procedure assertEqual_r64_5d_c32_5d_tol0
    module procedure assertEqual_c32_5d_c32_5d_tol0
    module procedure assertEqual_c64_5d_c32_5d_tol0
    module procedure assertEqual_int_5d_c64_5d_tol0
    module procedure assertEqual_r32_5d_c64_5d_tol0
    module procedure assertEqual_r64_5d_c64_5d_tol0
    module procedure assertEqual_c32_5d_c64_5d_tol0
    module procedure assertEqual_c64_5d_c64_5d_tol0

    module procedure assertEqual_int_5d_c32_5d_tol32
    module procedure assertEqual_r32_5d_c32_5d_tol32
    module procedure assertEqual_r64_5d_c32_5d_tol32
    module procedure assertEqual_c32_5d_c32_5d_tol32
    module procedure assertEqual_c64_5d_c32_5d_tol32
    module procedure assertEqual_int_5d_c64_5d_tol32
    module procedure assertEqual_r32_5d_c64_5d_tol32
    module procedure assertEqual_r64_5d_c64_5d_tol32
    module procedure assertEqual_c32_5d_c64_5d_tol32
    module procedure assertEqual_c64_5d_c64_5d_tol32

    module procedure assertEqual_int_5d_c32_5d_tol64
    module procedure assertEqual_r32_5d_c32_5d_tol64
    module procedure assertEqual_r64_5d_c32_5d_tol64
    module procedure assertEqual_c32_5d_c32_5d_tol64
    module procedure assertEqual_c64_5d_c32_5d_tol64
    module procedure assertEqual_int_5d_c64_5d_tol64
    module procedure assertEqual_r32_5d_c64_5d_tol64
    module procedure assertEqual_r64_5d_c64_5d_tol64
    module procedure assertEqual_c32_5d_c64_5d_tol64
    module procedure assertEqual_c64_5d_c64_5d_tol64


 end interface

 integer, parameter :: MAX_LEN_MSG     = 1000
 integer, parameter :: MAX_LEN_FLOAT   = 25
 integer, parameter :: MAX_LEN_COMPLEX = 50
 integer, parameter :: MAX_LEN_INT     = 10

 integer, parameter :: L_INFINITY_NORM = 0
 integer, parameter :: L1_NORM         = 1
 integer, parameter :: L2_NORM         = 2


 interface vectorNorm
    module procedure vectorNorm_scalar
    module procedure vectorNorm_1D
    module procedure vectorNorm_2D
    module procedure vectorNorm_3D
    module procedure vectorNorm_4D
    module procedure vectorNorm_5D
 end interface

 interface isWithinTolerance
    module procedure isWithinTolerance_scalar
    module procedure isWithinTolerance_1D
    module procedure isWithinTolerance_2D
    module procedure isWithinTolerance_3D
    module procedure isWithinTolerance_4D
    module procedure isWithinTolerance_5D
 end interface

 interface toString
    module procedure toString_int
    module procedure toString_sp
    module procedure toString_dp
    module procedure toString_sp_cmplx
    module procedure toString_dp_cmplx
    module procedure toString_int_1d
    module procedure toString_sp_1d
    module procedure toString_dp_1d
    module procedure toString_sp_1d_cmplx
    module procedure toString_dp_1d_cmplx
 end interface

 interface firstDifference
    module procedure firstDifference_scalar
    module procedure firstDifference_vector
 end interface

contains

  assert([int,default,0],[complex,32,0],0)
  assert([real,32,0],    [complex,32,0],0)
  assert([real,64,0],    [complex,32,0],0)
  assert([complex,32,0], [complex,32,0],0)
  assert([complex,64,0], [complex,32,0],0)
  assert([int,default,0],[complex,64,0],0)
  assert([real,32,0],    [complex,64,0],0)
  assert([real,64,0],    [complex,64,0],0)
  assert([complex,32,0], [complex,64,0],0)
  assert([complex,64,0], [complex,64,0],0)

  assert([int,default,0],[complex,32,0],32)
  assert([real,32,0],    [complex,32,0],32)
  assert([real,64,0],    [complex,32,0],32)
  assert([complex,32,0], [complex,32,0],32)
  assert([complex,64,0], [complex,32,0],32)
  assert([int,default,0],[complex,64,0],32)
  assert([real,32,0],    [complex,64,0],32)
  assert([real,64,0],    [complex,64,0],32)
  assert([complex,32,0], [complex,64,0],32)
  assert([complex,64,0], [complex,64,0],32)

  assert([int,default,0],[complex,32,0],64)
  assert([real,32,0],    [complex,32,0],64)
  assert([real,64,0],    [complex,32,0],64)
  assert([complex,32,0], [complex,32,0],64)
  assert([complex,64,0], [complex,32,0],64)
  assert([int,default,0],[complex,64,0],64)
  assert([real,32,0],    [complex,64,0],64)
  assert([real,64,0],    [complex,64,0],64)
  assert([complex,32,0], [complex,64,0],64)
  assert([complex,64,0], [complex,64,0],64)

  ! Vector 1D
  assert([int,default,0],[complex,32,1],0)
  assert([real,32,0],    [complex,32,1],0)
  assert([real,64,0],    [complex,32,1],0)
  assert([complex,32,0], [complex,32,1],0)
  assert([complex,64,0], [complex,32,1],0)
  assert([int,default,0],[complex,64,1],0)
  assert([real,32,0],    [complex,64,1],0)
  assert([real,64,0],    [complex,64,1],0)
  assert([complex,32,0], [complex,64,1],0)
  assert([complex,64,0], [complex,64,1],0)

  assert([int,default,0],[complex,32,1],32)
  assert([real,32,0],    [complex,32,1],32)
  assert([real,64,0],    [complex,32,1],32)
  assert([complex,32,0], [complex,32,1],32)
  assert([complex,64,0], [complex,32,1],32)
  assert([int,default,0],[complex,64,1],32)
  assert([real,32,0],    [complex,64,1],32)
  assert([real,64,0],    [complex,64,1],32)
  assert([complex,32,0], [complex,64,1],32)
  assert([complex,64,0], [complex,64,1],32)

  assert([int,default,0],[complex,32,1],64)
  assert([real,32,0],    [complex,32,1],64)
  assert([real,64,0],    [complex,32,1],64)
  assert([complex,32,0], [complex,32,1],64)
  assert([complex,64,0], [complex,32,1],64)
  assert([int,default,0],[complex,64,1],64)
  assert([real,32,0],    [complex,64,1],64)
  assert([real,64,0],    [complex,64,1],64)
  assert([complex,32,0], [complex,64,1],64)
  assert([complex,64,0], [complex,64,1],64)

  assert([int,default,1],[complex,32,1],0)
  assert([real,32,1],    [complex,32,1],0)
  assert([real,64,1],    [complex,32,1],0)
  assert([complex,32,1], [complex,32,1],0)
  assert([complex,64,1], [complex,32,1],0)
  assert([int,default,1],[complex,64,1],0)
  assert([real,32,1],    [complex,64,1],0)
  assert([real,64,1],    [complex,64,1],0)
  assert([complex,32,1], [complex,64,1],0)
  assert([complex,64,1], [complex,64,1],0)

  assert([int,default,1],[complex,32,1],32)
  assert([real,32,1],    [complex,32,1],32)
  assert([real,64,1],    [complex,32,1],32)
  assert([complex,32,1], [complex,32,1],32)
  assert([complex,64,1], [complex,32,1],32)
  assert([int,default,1],[complex,64,1],32)
  assert([real,32,1],    [complex,64,1],32)
  assert([real,64,1],    [complex,64,1],32)
  assert([complex,32,1], [complex,64,1],32)
  assert([complex,64,1], [complex,64,1],32)

  assert([int,default,1],[complex,32,1],64)
  assert([real,32,1],    [complex,32,1],64)
  assert([real,64,1],    [complex,32,1],64)
  assert([complex,32,1], [complex,32,1],64)
  assert([complex,64,1], [complex,32,1],64)
  assert([int,default,1],[complex,64,1],64)
  assert([real,32,1],    [complex,64,1],64)
  assert([real,64,1],    [complex,64,1],64)
  assert([complex,32,1], [complex,64,1],64)
  assert([complex,64,1], [complex,64,1],64)

  ! Array 2D
  assert([int,default,0],[complex,32,2],0)
  assert([real,32,0],    [complex,32,2],0)
  assert([real,64,0],    [complex,32,2],0)
  assert([complex,32,0], [complex,32,2],0)
  assert([complex,64,0], [complex,32,2],0)
  assert([int,default,0],[complex,64,2],0)
  assert([real,32,0],    [complex,64,2],0)
  assert([real,64,0],    [complex,64,2],0)
  assert([complex,32,0], [complex,64,2],0)
  assert([complex,64,0], [complex,64,2],0)

  assert([int,default,0],[complex,32,2],32)
  assert([real,32,0],    [complex,32,2],32)
  assert([real,64,0],    [complex,32,2],32)
  assert([complex,32,0], [complex,32,2],32)
  assert([complex,64,0], [complex,32,2],32)
  assert([int,default,0],[complex,64,2],32)
  assert([real,32,0],    [complex,64,2],32)
  assert([real,64,0],    [complex,64,2],32)
  assert([complex,32,0], [complex,64,2],32)
  assert([complex,64,0], [complex,64,2],32)

  assert([int,default,0],[complex,32,2],64)
  assert([real,32,0],    [complex,32,2],64)
  assert([real,64,0],    [complex,32,2],64)
  assert([complex,32,0], [complex,32,2],64)
  assert([complex,64,0], [complex,32,2],64)
  assert([int,default,0],[complex,64,2],64)
  assert([real,32,0],    [complex,64,2],64)
  assert([real,64,0],    [complex,64,2],64)
  assert([complex,32,0], [complex,64,2],64)
  assert([complex,64,0], [complex,64,2],64)

  assert([int,default,2],[complex,32,2],0)
  assert([real,32,2],    [complex,32,2],0)
  assert([real,64,2],    [complex,32,2],0)
  assert([complex,32,2], [complex,32,2],0)
  assert([complex,64,2], [complex,32,2],0)
  assert([int,default,2],[complex,64,2],0)
  assert([real,32,2],    [complex,64,2],0)
  assert([real,64,2],    [complex,64,2],0)
  assert([complex,32,2], [complex,64,2],0)
  assert([complex,64,2], [complex,64,2],0)

  assert([int,default,2],[complex,32,2],32)
  assert([real,32,2],    [complex,32,2],32)
  assert([real,64,2],    [complex,32,2],32)
  assert([complex,32,2], [complex,32,2],32)
  assert([complex,64,2], [complex,32,2],32)
  assert([int,default,2],[complex,64,2],32)
  assert([real,32,2],    [complex,64,2],32)
  assert([real,64,2],    [complex,64,2],32)
  assert([complex,32,2], [complex,64,2],32)
  assert([complex,64,2], [complex,64,2],32)

  assert([int,default,2],[complex,32,2],64)
  assert([real,32,2],    [complex,32,2],64)
  assert([real,64,2],    [complex,32,2],64)
  assert([complex,32,2], [complex,32,2],64)
  assert([complex,64,2], [complex,32,2],64)
  assert([int,default,2],[complex,64,2],64)
  assert([real,32,2],    [complex,64,2],64)
  assert([real,64,2],    [complex,64,2],64)
  assert([complex,32,2], [complex,64,2],64)
  assert([complex,64,2], [complex,64,2],64)

  ! Array 3D
  assert([int,default,0],[complex,32,3],0)
  assert([real,32,0],    [complex,32,3],0)
  assert([real,64,0],    [complex,32,3],0)
  assert([complex,32,0], [complex,32,3],0)
  assert([complex,64,0], [complex,32,3],0)
  assert([int,default,0],[complex,64,3],0)
  assert([real,32,0],    [complex,64,3],0)
  assert([real,64,0],    [complex,64,3],0)
  assert([complex,32,0], [complex,64,3],0)
  assert([complex,64,0], [complex,64,3],0)

  assert([int,default,0],[complex,32,3],32)
  assert([real,32,0],    [complex,32,3],32)
  assert([real,64,0],    [complex,32,3],32)
  assert([complex,32,0], [complex,32,3],32)
  assert([complex,64,0], [complex,32,3],32)
  assert([int,default,0],[complex,64,3],32)
  assert([real,32,0],    [complex,64,3],32)
  assert([real,64,0],    [complex,64,3],32)
  assert([complex,32,0], [complex,64,3],32)
  assert([complex,64,0], [complex,64,3],32)

  assert([int,default,0],[complex,32,3],64)
  assert([real,32,0],    [complex,32,3],64)
  assert([real,64,0],    [complex,32,3],64)
  assert([complex,32,0], [complex,32,3],64)
  assert([complex,64,0], [complex,32,3],64)
  assert([int,default,0],[complex,64,3],64)
  assert([real,32,0],    [complex,64,3],64)
  assert([real,64,0],    [complex,64,3],64)
  assert([complex,32,0], [complex,64,3],64)
  assert([complex,64,0], [complex,64,3],64)

  assert([int,default,3],[complex,32,3],0)
  assert([real,32,3],    [complex,32,3],0)
  assert([real,64,3],    [complex,32,3],0)
  assert([complex,32,3], [complex,32,3],0)
  assert([complex,64,3], [complex,32,3],0)
  assert([int,default,3],[complex,64,3],0)
  assert([real,32,3],    [complex,64,3],0)
  assert([real,64,3],    [complex,64,3],0)
  assert([complex,32,3], [complex,64,3],0)
  assert([complex,64,3], [complex,64,3],0)

  assert([int,default,3],[complex,32,3],32)
  assert([real,32,3],    [complex,32,3],32)
  assert([real,64,3],    [complex,32,3],32)
  assert([complex,32,3], [complex,32,3],32)
  assert([complex,64,3], [complex,32,3],32)
  assert([int,default,3],[complex,64,3],32)
  assert([real,32,3],    [complex,64,3],32)
  assert([real,64,3],    [complex,64,3],32)
  assert([complex,32,3], [complex,64,3],32)
  assert([complex,64,3], [complex,64,3],32)

  assert([int,default,3],[complex,32,3],64)
  assert([real,32,3],    [complex,32,3],64)
  assert([real,64,3],    [complex,32,3],64)
  assert([complex,32,3], [complex,32,3],64)
  assert([complex,64,3], [complex,32,3],64)
  assert([int,default,3],[complex,64,3],64)
  assert([real,32,3],    [complex,64,3],64)
  assert([real,64,3],    [complex,64,3],64)
  assert([complex,32,3], [complex,64,3],64)
  assert([complex,64,3], [complex,64,3],64)

  ! Array 4D
  assert([int,default,0],[complex,32,4],0)
  assert([real,32,0],    [complex,32,4],0)
  assert([real,64,0],    [complex,32,4],0)
  assert([complex,32,0], [complex,32,4],0)
  assert([complex,64,0], [complex,32,4],0)
  assert([int,default,0],[complex,64,4],0)
  assert([real,32,0],    [complex,64,4],0)
  assert([real,64,0],    [complex,64,4],0)
  assert([complex,32,0], [complex,64,4],0)
  assert([complex,64,0], [complex,64,4],0)

  assert([int,default,0],[complex,32,4],32)
  assert([real,32,0],    [complex,32,4],32)
  assert([real,64,0],    [complex,32,4],32)
  assert([complex,32,0], [complex,32,4],32)
  assert([complex,64,0], [complex,32,4],32)
  assert([int,default,0],[complex,64,4],32)
  assert([real,32,0],    [complex,64,4],32)
  assert([real,64,0],    [complex,64,4],32)
  assert([complex,32,0], [complex,64,4],32)
  assert([complex,64,0], [complex,64,4],32)

  assert([int,default,0],[complex,32,4],64)
  assert([real,32,0],    [complex,32,4],64)
  assert([real,64,0],    [complex,32,4],64)
  assert([complex,32,0], [complex,32,4],64)
  assert([complex,64,0], [complex,32,4],64)
  assert([int,default,0],[complex,64,4],64)
  assert([real,32,0],    [complex,64,4],64)
  assert([real,64,0],    [complex,64,4],64)
  assert([complex,32,0], [complex,64,4],64)
  assert([complex,64,0], [complex,64,4],64)

  assert([int,default,4],[complex,32,4],0)
  assert([real,32,4],    [complex,32,4],0)
  assert([real,64,4],    [complex,32,4],0)
  assert([complex,32,4], [complex,32,4],0)
  assert([complex,64,4], [complex,32,4],0)
  assert([int,default,4],[complex,64,4],0)
  assert([real,32,4],    [complex,64,4],0)
  assert([real,64,4],    [complex,64,4],0)
  assert([complex,32,4], [complex,64,4],0)
  assert([complex,64,4], [complex,64,4],0)

  assert([int,default,4],[complex,32,4],32)
  assert([real,32,4],    [complex,32,4],32)
  assert([real,64,4],    [complex,32,4],32)
  assert([complex,32,4], [complex,32,4],32)
  assert([complex,64,4], [complex,32,4],32)
  assert([int,default,4],[complex,64,4],32)
  assert([real,32,4],    [complex,64,4],32)
  assert([real,64,4],    [complex,64,4],32)
  assert([complex,32,4], [complex,64,4],32)
  assert([complex,64,4], [complex,64,4],32)

  assert([int,default,4],[complex,32,4],64)
  assert([real,32,4],    [complex,32,4],64)
  assert([real,64,4],    [complex,32,4],64)
  assert([complex,32,4], [complex,32,4],64)
  assert([complex,64,4], [complex,32,4],64)
  assert([int,default,4],[complex,64,4],64)
  assert([real,32,4],    [complex,64,4],64)
  assert([real,64,4],    [complex,64,4],64)
  assert([complex,32,4], [complex,64,4],64)
  assert([complex,64,4], [complex,64,4],64)

  ! Array 5D
  assert([int,default,0],[complex,32,5],0)
  assert([real,32,0],    [complex,32,5],0)
  assert([real,64,0],    [complex,32,5],0)
  assert([complex,32,0], [complex,32,5],0)
  assert([complex,64,0], [complex,32,5],0)
  assert([int,default,0],[complex,64,5],0)
  assert([real,32,0],    [complex,64,5],0)
  assert([real,64,0],    [complex,64,5],0)
  assert([complex,32,0], [complex,64,5],0)
  assert([complex,64,0], [complex,64,5],0)

  assert([int,default,0],[complex,32,5],32)
  assert([real,32,0],    [complex,32,5],32)
  assert([real,64,0],    [complex,32,5],32)
  assert([complex,32,0], [complex,32,5],32)
  assert([complex,64,0], [complex,32,5],32)
  assert([int,default,0],[complex,64,5],32)
  assert([real,32,0],    [complex,64,5],32)
  assert([real,64,0],    [complex,64,5],32)
  assert([complex,32,0], [complex,64,5],32)
  assert([complex,64,0], [complex,64,5],32)

  assert([int,default,0],[complex,32,5],64)
  assert([real,32,0],    [complex,32,5],64)
  assert([real,64,0],    [complex,32,5],64)
  assert([complex,32,0], [complex,32,5],64)
  assert([complex,64,0], [complex,32,5],64)
  assert([int,default,0],[complex,64,5],64)
  assert([real,32,0],    [complex,64,5],64)
  assert([real,64,0],    [complex,64,5],64)
  assert([complex,32,0], [complex,64,5],64)
  assert([complex,64,0], [complex,64,5],64)

  assert([int,default,5],[complex,32,5],0)
  assert([real,32,5],    [complex,32,5],0)
  assert([real,64,5],    [complex,32,5],0)
  assert([complex,32,5], [complex,32,5],0)
  assert([complex,64,5], [complex,32,5],0)
  assert([int,default,5],[complex,64,5],0)
  assert([real,32,5],    [complex,64,5],0)
  assert([real,64,5],    [complex,64,5],0)
  assert([complex,32,5], [complex,64,5],0)
  assert([complex,64,5], [complex,64,5],0)

  assert([int,default,5],[complex,32,5],32)
  assert([real,32,5],    [complex,32,5],32)
  assert([real,64,5],    [complex,32,5],32)
  assert([complex,32,5], [complex,32,5],32)
  assert([complex,64,5], [complex,32,5],32)
  assert([int,default,5],[complex,64,5],32)
  assert([real,32,5],    [complex,64,5],32)
  assert([real,64,5],    [complex,64,5],32)
  assert([complex,32,5], [complex,64,5],32)
  assert([complex,64,5], [complex,64,5],32)

  assert([int,default,5],[complex,32,5],64)
  assert([real,32,5],    [complex,32,5],64)
  assert([real,64,5],    [complex,32,5],64)
  assert([complex,32,5], [complex,32,5],64)
  assert([complex,64,5], [complex,32,5],64)
  assert([int,default,5],[complex,64,5],64)
  assert([real,32,5],    [complex,64,5],64)
  assert([real,64,5],    [complex,64,5],64)
  assert([complex,32,5], [complex,64,5],64)
  assert([complex,64,5], [complex,64,5],64)


  !---------------------------------------------------------------------------
  !> Converts integer to string 
  !!
  !! @param i - given integer number 
  !!
  !! @return string converted from integer number
  !---------------------------------------------------------------------------
  function toString_int(i) result(string)
    integer, intent(in)   :: i
    character(LEN=MAX_LEN_INT) :: string

    write(string,'(i0)') i
  end function toString_int

  !---------------------------------------------------------------------------
  !> Converts real number in single precision to string 
  !!
  !! @param x - given real number in single precision
  !!
  !! @return string converted from real number 
  !---------------------------------------------------------------------------
  function toString_sp(x) result(string)
    real(kind=r32), intent(in)   :: x
    character(LEN=MAX_LEN_FLOAT) :: string

    write(string,'(G20.10)') x
  end function toString_sp

  !---------------------------------------------------------------------------
  !> Converts real number in double precision to string 
  !!
  !! @param x - given real number in double precision
  !!
  !! @return string converted from real number 
  !---------------------------------------------------------------------------
  function toString_dp(x) result(string)
    real(kind=r64), intent(in)   :: x
    character(LEN=MAX_LEN_FLOAT) :: string

    write(string,'(G20.10)') x
  end function toString_dp

  !---------------------------------------------------------------------------
  !> Converts complex number in single precision to string 
  !!
  !! @param x - given real number in single precision
  !!
  !! @return string converted from complex number 
  !---------------------------------------------------------------------------
  function toString_sp_cmplx(x) result(string)
    complex(kind=c32), intent(in)   :: x
    character(LEN=MAX_LEN_COMPLEX) :: string

    write(string,'(2G20.10)') x
  end function toString_sp_cmplx

  !---------------------------------------------------------------------------
  !> Converts complex number in double precision to string 
  !!
  !! @param x - given real number in double precision
  !!
  !! @return string converted from complex number 
  !---------------------------------------------------------------------------
  function toString_dp_cmplx(x) result(string)
    complex(kind=c64), intent(in)   :: x
    character(LEN=MAX_LEN_COMPLEX) :: string

    write(string,'(2G20.10)') x
  end function toString_dp_cmplx

  !---------------------------------------------------------------------------
  !> Converts one-dimensional integer array to string
  !!
  !! @param i - given one-dimensional integer number
  !!
  !! @return string converted from one-dimensional integer number
  !---------------------------------------------------------------------------
  function toString_int_1D(i) result(string)
    integer, intent(in)   :: i(:)
    character(LEN=MAX_LEN_INT) :: string
    integer, parameter :: MAX_LEN_LINE=80
    character(LEN=MAX_LEN_LINE) :: frmt
    integer :: n

    n = size(i)
    select case (n)
    case (1)
       write(string,'("(/ ", i0, " /)")') i
    case (2)
       write(string,'("(/ ", i0, ", ", i0, " /)")') i
    case (3:)
       write(frmt,'("(",a,i0,a,")")') '"(/ ",',n-1,'(i0,", "),i0," /)"'
       write(string, frmt) i
    end select

  end function toString_int_1D

  !----------------------------------------------------------------------------
  !> Converts one-dimensional single-precision real array in string
  !!
  !! @param x - given one-dimensional single-precision real number
  !!
  !! @return string converted from one-dimensional single-precision real number
  !----------------------------------------------------------------------------
  function toString_sp_1D(x) result(string)
    real(kind=r32), intent(in)   :: x(:)
    character(LEN=1000) :: string

    integer, parameter :: MAX_LEN_LINE=80
    character(LEN=MAX_LEN_LINE) :: frmt
    integer :: n

    n = size(x)
    select case (n)
    case (1)
       write(string,'("(/ ", G20.10, " /)")') x
    case (2)
       write(string,'("(/ ", G20.10, ", ", G20.10, " /)")') x
    case (3:)
       write(frmt,'("(",a,G20.10,a,")")') '"(/ ",',n-1,'(G20.10,", "),G20.10," /)"'
       write(string, frmt) x
    end select

  end function toString_sp_1D

  !----------------------------------------------------------------------------
  !> Converts one-dimensional double-precision real array in string
  !!
  !! @param x - given one-dimensional double-precision real number
  !!
  !! @return string converted from one-dimensional double-precision real number
  !----------------------------------------------------------------------------
  function toString_dp_1D(x) result(string)
    real(kind=r64), intent(in)   :: x(:)
    character(LEN=1000) :: string

    integer, parameter :: MAX_LEN_LINE=80
    character(LEN=MAX_LEN_LINE) :: frmt
    integer :: n

    n = size(x)
    select case (n)
    case (1)
       write(string,'("(/ ", G20.10, " /)")') x
    case (2)
       write(string,'("(/ ", G20.10, ", ", G20.10, " /)")') x
    case (3:)
       write(frmt,'("(",a,G20.10,a,")")') '"(/ ",',n-1,'(G20.10,", "),G20.10," /)"'
       write(string, frmt) x
    end select

  end function toString_dp_1D

  !----------------------------------------------------------------------------
  !> Converts one-dimensional single-precision complex array in string
  !!
  !! @param x - given one-dimensional single-precision complex number
  !!
  !! @return string converted from one-dimensional single-precision complex 
  !! number
  !----------------------------------------------------------------------------
  function toString_sp_1D_cmplx(x) result(string)
    complex(kind=c32), intent(in)   :: x(:)
    character(LEN=1000) :: string

    integer, parameter :: MAX_LEN_LINE=80
    character(LEN=MAX_LEN_LINE) :: frmt
    integer :: n

    n = size(x)
    select case (n)
    case (1)
       write(string,'("(/ ", 2G20.10, " /)")') x
    case (2)
       write(string,'("(/ ", 2G27.15, ", ", 2G27.15, " /)")') x
    case (3:)
       write(frmt,'("(",a,2G27.15,a,")")') '"(/ ",',n-1,'(2G27.15,", "),2G27.15," /)"'
       write(string, frmt) x
    end select

  end function toString_sp_1D_cmplx

  !----------------------------------------------------------------------------
  !> Converts one-dimensional double-precision complex array in string
  !!
  !! @param x - given one-dimensional double-precision complex number
  !!
  !! @return string converted from one-dimensional double-precision complex 
  !! number
  !----------------------------------------------------------------------------
  function toString_dp_1D_cmplx(x) result(string)
    complex(kind=c64), intent(in)   :: x(:)
    character(LEN=1000) :: string

    integer, parameter :: MAX_LEN_LINE=80
    character(LEN=MAX_LEN_LINE) :: frmt
    integer :: n

    n = size(x)
    select case (n)
    case (1)
       write(string,'("(/ ", 2G20.10, " /)")') x
    case (2)
       write(string,'("(/ ", 2G20.10, ", ", 2G20.10, " /)")') x
    case (3:)
       write(frmt,'("(",a,2G20.10,a,")")') '"(/ ",',n-1,'(2G20.10,", "),2G20.10," /)"'
       write(string, frmt) x
    end select

  end function toString_dp_1D_cmplx

  !---------------------------------------------------------------------------
  !> Appends string with suffix
  !!
  !! @param string - given string and then appends with suffix
  !! @param suffix -  given suffix in string 
  !---------------------------------------------------------------------------
  subroutine append(string, suffix)
    character(LEN=*), intent(inout) :: string
    character(LEN=*), intent(in)    :: suffix

    string = trim(string) // trim(suffix)

  end subroutine append

  !---------------------------------------------------------------------------
  !> Returns the first indicator by the given flag
  !! (0 - all same, 1 - difference)
  !!
  !! @param flag - given flag indicator
  !!
  !! @return 0 if flag is .true.  Otherwise, it is 1
  !---------------------------------------------------------------------------
  integer function firstDifference_scalar(flag) result(first)
    logical, intent(in) :: flag
    if (flag) then
       first = 0 ! all same
    else
       first = 1
    end if
  end function firstDifference_scalar

  !---------------------------------------------------------------------------
  !> Returns the first indicator by the given flag in vector
  !! (0 - all same, 1 - difference)
  !!
  !! @param flag - given flag indicator in vector
  !!
  !! @return 0 if flag is .true.  Otherwise, it is 1
  !---------------------------------------------------------------------------
  integer function firstDifference_vector(flag) result(first)
    logical, intent(in) :: flag(:)
    integer :: i, n

    n = size(flag)
    first = 0 ! assume all same
    do i = 1, n
       if (.not. flag(i)) then
          first = i
          exit
       end if
    end do

  end function firstDifference_vector

  !---------------------------------------------------------------------------
  !> Returns the first indicator by the given flag in array
  !! (0 - all same, 1 - difference)
  !!
  !! @param flag - given flag indicator in array
  !!
  !! @return 0 if flag is .true.  Otherwise, it is 1
  !---------------------------------------------------------------------------
  integer function firstDifference_array(flag) result(first)
    logical, intent(in) :: flag(:,:)
    integer :: i, j, m, n

    m = size(flag,1)
    n = size(flag,2)
    first = 0 ! assume all same
    outer:    do j = 1, n
      do i = 1, m	
        if (.not. flag(i,j)) then
          first = i + (j-1) * m
          exit outer
        end if
      end do
    end do outer

  end function firstDifference_array

  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given double-precission 
  !! complex numbers and given integer norm
  !!
  !! @param x - given double-precision complex number
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_scalar(x, norm) result(y)
    complex (kind=c64), intent(in) :: x
    integer :: norm
    real (kind=r64) :: y

    y = abs(x) ! independent of norm
    
  end function vectorNorm_scalar

define([VECTOR_NORM],[
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional 
  !! double-precission complex numbers and given integer norm
  !!
  !! @param x - given diminsional double-precision complex numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_$1D(x, norm) result(y)
    complex (kind=c64), intent(in) :: x DIMS($1)
    integer :: norm
    real (kind=r64) :: y

    select case (norm)
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
       y = sqrt(sum(x**2))
    end select
    
  end function vectorNorm_$1D
])

VECTOR_NORM(1)
VECTOR_NORM(2)
VECTOR_NORM(3)
VECTOR_NORM(4)
VECTOR_NORM(5)

   !---------------------------------------------------------------------------
   !> Determines if the double-precision complex number in scalar is within 
   !! the tolerance 
   !!
   !! @param x - given complex number in double precision
   !! @param tolerance - given tolerance in double precision
   !! @param norm - given norm 
   !!
   !! @return .true. if the given number is within the given tolerance. 
   !! Otherwise, it is .false. 
   !---------------------------------------------------------------------------
   logical function isWithinTolerance_scalar(x, tolerance, norm)
     complex (kind=c64), intent(in) :: x
     real (kind=r64),    intent(in) :: tolerance
     integer,            intent(in) :: norm

     isWithinTolerance_scalar = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_scalar

   !---------------------------------------------------------------------------
   !> Determines if the one-diminsional array double-precission complex numbers
   !! are within the tolerance
   !!
   !! @param x - given one-diminsional array double-precision complex numbers
   !! @param tolerance - given tolerance in double precision
   !! @param norm - given norm
   !!
   !! @return .true. if the given number are within the given tolerance. 
   !! Otherwise, it is .false.
   !---------------------------------------------------------------------------
   logical function isWithinTolerance_1D(x, tolerance, norm)
     complex (kind=c64), intent(in) :: x(:)
     real (kind=r64),    intent(in) :: tolerance
     integer,            intent(in) :: norm

     isWithinTolerance_1D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_1D

   !---------------------------------------------------------------------------
   !> Determines if the two-diminsional array double-precission complex numbers 
   !! are within the tolerance
   !!
   !! @param x - given two-diminsional array double-precision complex numbers
   !! @param tolerance - given tolerance in double precision
   !! @param norm - given norm
   !!
   !! @return .true. if the given numbers are within the given tolerance.
   !! Otherwise, it is .false.
   !---------------------------------------------------------------------------
   logical function isWithinTolerance_2D(x, tolerance, norm)
     complex  (kind=c64), intent(in) :: x(:,:)
     real (kind=r64),     intent(in) :: tolerance
     integer,             intent(in) :: norm

     isWithinTolerance_2D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_2D

   !---------------------------------------------------------------------------
   !> Determines if the three-diminsional array double-precission complex numbers
   !! are within the tolerance
   !!
   !! @param x - given three-diminsional array double-precision complex numbers
   !! @param tolerance - given tolerance in double precision
   !! @param norm - given norm
   !!
   !! @return .true. if the given numbers are within the given tolerance.
   !! Otherwise, it is .false.
   !---------------------------------------------------------------------------
   logical function isWithinTolerance_3D(x, tolerance, norm)
     complex (kind=c64), intent(in) :: x(:,:,:)
     real (kind=r64),    intent(in) :: tolerance
     integer,            intent(in) :: norm

     isWithinTolerance_3D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_3D

   !---------------------------------------------------------------------------
   !> Determines if the four-diminsional array double-precission complex numbers
   !! are within the tolerance
   !!
   !! @param x - given four-diminsional array double-precision complex numbers
   !! @param tolerance - given tolerance in double precision
   !! @param norm - given norm
   !!
   !! @return .true. if the given numbers are within the given tolerance.
   !! Otherwise, it is .false.
   !---------------------------------------------------------------------------
   logical function isWithinTolerance_4D(x, tolerance, norm)
     complex (kind=c64), intent(in) :: x(:,:,:,:)
     real (kind=r64),    intent(in) :: tolerance
     integer,            intent(in) :: norm

     isWithinTolerance_4D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_4D

   !---------------------------------------------------------------------------
   !> Determines if the five-diminsional array double-precission complex numbers
   !! are within the tolerance
   !!
   !! @param x - given five-diminsional array double-precision complex numbers
   !! @param tolerance - given tolerance in double precision
   !! @param norm - given norm
   !!
   !! @return .true. if the given numbers are within the given tolerance.
   !! Otherwise, it is .false.
   !---------------------------------------------------------------------------
   logical function isWithinTolerance_5D(x, tolerance, norm)
     complex (kind=c64), intent(in) :: x(:,:,:,:,:)
     real (kind=r64),    intent(in) :: tolerance
     integer,            intent(in) :: norm

     isWithinTolerance_5D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_5D

end module AssertComplex_mod
