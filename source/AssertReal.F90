module AssertReal_mod

   use Params_mod
   use AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   use ThrowFundamentalTypes_mod, only : throwNonConformable
   ! , differenceReport, valuesReport
   use StringUtilities_mod


   implicit none
   private


   public :: assertEqual

   public :: vectorNorm
   public :: isWithinTolerance
 
   public :: L_INFINITY_NORM
   public :: L1_NORM
   public :: L2_NORM

   public :: valuesReport
   public :: differenceReport


   integer, parameter :: L_INFINITY_NORM = 0
   integer, parameter :: L1_NORM         = 1
   integer, parameter :: L2_NORM         = 2

   integer, parameter :: MAXLEN_SHAPE = 80


interface vectorNorm

   module procedure vectorNorm_0D_real32
   module procedure vectorNorm_0D_real64
   module procedure vectorNorm_0D_complex32
   module procedure vectorNorm_0D_complex64
   module procedure vectorNorm_1D_real32
   module procedure vectorNorm_1D_real64
   module procedure vectorNorm_1D_complex32
   module procedure vectorNorm_1D_complex64
   module procedure vectorNorm_2D_real32
   module procedure vectorNorm_2D_real64
   module procedure vectorNorm_2D_complex32
   module procedure vectorNorm_2D_complex64
   module procedure vectorNorm_3D_real32
   module procedure vectorNorm_3D_real64
   module procedure vectorNorm_3D_complex32
   module procedure vectorNorm_3D_complex64
   module procedure vectorNorm_4D_real32
   module procedure vectorNorm_4D_real64
   module procedure vectorNorm_4D_complex32
   module procedure vectorNorm_4D_complex64
   module procedure vectorNorm_5D_real32
   module procedure vectorNorm_5D_real64
   module procedure vectorNorm_5D_complex32
   module procedure vectorNorm_5D_complex64

end interface vectorNorm


interface isWithinTolerance

   module procedure isWithinTolerance_0D_real32
   module procedure isWithinTolerance_0D_real64
   module procedure isWithinTolerance_0D_complex32
   module procedure isWithinTolerance_0D_complex64
   module procedure isWithinTolerance_1D_real32
   module procedure isWithinTolerance_1D_real64
   module procedure isWithinTolerance_1D_complex32
   module procedure isWithinTolerance_1D_complex64
   module procedure isWithinTolerance_2D_real32
   module procedure isWithinTolerance_2D_real64
   module procedure isWithinTolerance_2D_complex32
   module procedure isWithinTolerance_2D_complex64
   module procedure isWithinTolerance_3D_real32
   module procedure isWithinTolerance_3D_real64
   module procedure isWithinTolerance_3D_complex32
   module procedure isWithinTolerance_3D_complex64
   module procedure isWithinTolerance_4D_real32
   module procedure isWithinTolerance_4D_real64
   module procedure isWithinTolerance_4D_complex32
   module procedure isWithinTolerance_4D_complex64
   module procedure isWithinTolerance_5D_real32
   module procedure isWithinTolerance_5D_real64
   module procedure isWithinTolerance_5D_complex32
   module procedure isWithinTolerance_5D_complex64

end interface isWithinTolerance


interface assertEqual

   module procedure assertEqual_int_0D_r32_0D_tol32
   module procedure assertEqual_r32_0D_r32_0D_tol32
   module procedure assertEqual_int_0D_r64_0D_tol64
   module procedure assertEqual_r32_0D_r64_0D_tol64
   module procedure assertEqual_r64_0D_r64_0D_tol64
   module procedure assertEqual_int_0D_r32_1D_tol32
   module procedure assertEqual_r32_0D_r32_1D_tol32
   module procedure assertEqual_int_1D_r32_1D_tol32
   module procedure assertEqual_r32_1D_r32_1D_tol32
   module procedure assertEqual_int_0D_r64_1D_tol64
   module procedure assertEqual_r32_0D_r64_1D_tol64
   module procedure assertEqual_r64_0D_r64_1D_tol64
   module procedure assertEqual_int_1D_r64_1D_tol64
   module procedure assertEqual_r32_1D_r64_1D_tol64
   module procedure assertEqual_r64_1D_r64_1D_tol64
   module procedure assertEqual_int_0D_r32_2D_tol32
   module procedure assertEqual_r32_0D_r32_2D_tol32
   module procedure assertEqual_int_2D_r32_2D_tol32
   module procedure assertEqual_r32_2D_r32_2D_tol32
   module procedure assertEqual_int_0D_r64_2D_tol64
   module procedure assertEqual_r32_0D_r64_2D_tol64
   module procedure assertEqual_r64_0D_r64_2D_tol64
   module procedure assertEqual_int_2D_r64_2D_tol64
   module procedure assertEqual_r32_2D_r64_2D_tol64
   module procedure assertEqual_r64_2D_r64_2D_tol64
   module procedure assertEqual_int_0D_r32_3D_tol32
   module procedure assertEqual_r32_0D_r32_3D_tol32
   module procedure assertEqual_int_3D_r32_3D_tol32
   module procedure assertEqual_r32_3D_r32_3D_tol32
   module procedure assertEqual_int_0D_r64_3D_tol64
   module procedure assertEqual_r32_0D_r64_3D_tol64
   module procedure assertEqual_r64_0D_r64_3D_tol64
   module procedure assertEqual_int_3D_r64_3D_tol64
   module procedure assertEqual_r32_3D_r64_3D_tol64
   module procedure assertEqual_r64_3D_r64_3D_tol64
   module procedure assertEqual_int_0D_r32_4D_tol32
   module procedure assertEqual_r32_0D_r32_4D_tol32
   module procedure assertEqual_int_4D_r32_4D_tol32
   module procedure assertEqual_r32_4D_r32_4D_tol32
   module procedure assertEqual_int_0D_r64_4D_tol64
   module procedure assertEqual_r32_0D_r64_4D_tol64
   module procedure assertEqual_r64_0D_r64_4D_tol64
   module procedure assertEqual_int_4D_r64_4D_tol64
   module procedure assertEqual_r32_4D_r64_4D_tol64
   module procedure assertEqual_r64_4D_r64_4D_tol64
   module procedure assertEqual_int_0D_r32_5D_tol32
   module procedure assertEqual_r32_0D_r32_5D_tol32
   module procedure assertEqual_int_5D_r32_5D_tol32
   module procedure assertEqual_r32_5D_r32_5D_tol32
   module procedure assertEqual_int_0D_r64_5D_tol64
   module procedure assertEqual_r32_0D_r64_5D_tol64
   module procedure assertEqual_r64_0D_r64_5D_tol64
   module procedure assertEqual_int_5D_r64_5D_tol64
   module procedure assertEqual_r32_5D_r64_5D_tol64
   module procedure assertEqual_r64_5D_r64_5D_tol64

end interface assertEqual

public throwDifferentValuesString64

interface differenceReport

   module procedure differenceReport_real3232
   module procedure differenceReport_real3264
   module procedure differenceReport_real6432
   module procedure differenceReport_real6464
   module procedure differenceReport_complex3232
   module procedure differenceReport_complex3264
   module procedure differenceReport_complex6432
   module procedure differenceReport_complex6464

end interface differenceReport


interface valuesReport

   module procedure valuesReport_integerintegerdefaultdefault
   module procedure valuesReport_integerrealdefault32
   module procedure valuesReport_integerrealdefault64
   module procedure valuesReport_integercomplexdefault32
   module procedure valuesReport_integercomplexdefault64
   module procedure valuesReport_realinteger32default
   module procedure valuesReport_realinteger64default
   module procedure valuesReport_realreal3232
   module procedure valuesReport_realreal6432
   module procedure valuesReport_realreal3264
   module procedure valuesReport_realreal6464
   module procedure valuesReport_realcomplex3232
   module procedure valuesReport_realcomplex6432
   module procedure valuesReport_realcomplex3264
   module procedure valuesReport_realcomplex6464
   module procedure valuesReport_complexinteger32default
   module procedure valuesReport_complexinteger64default
   module procedure valuesReport_complexreal3232
   module procedure valuesReport_complexreal6432
   module procedure valuesReport_complexreal3264
   module procedure valuesReport_complexreal6464
   module procedure valuesReport_complexcomplex3232
   module procedure valuesReport_complexcomplex6432
   module procedure valuesReport_complexcomplex3264
   module procedure valuesReport_complexcomplex6464

end interface valuesReport

contains
! interface vectorNorm implementations

  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 0.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_0D_real32(x, norm) result(y)
    real(kind=r32), intent(in) :: x
    integer :: norm
    real (kind=r64) :: y

    y = abs(x) ! independent of norm for rank=0 (scalar) case.

  end function vectorNorm_0D_real32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 0.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_0D_real64(x, norm) result(y)
    real(kind=r64), intent(in) :: x
    integer :: norm
    real (kind=r64) :: y

    y = abs(x) ! independent of norm for rank=0 (scalar) case.

  end function vectorNorm_0D_real64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 0.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_0D_complex32(x, norm) result(y)
    complex(kind=r32), intent(in) :: x
    integer :: norm
    real (kind=r64) :: y

    y = abs(x) ! independent of norm for rank=0 (scalar) case.

  end function vectorNorm_0D_complex32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 0.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_0D_complex64(x, norm) result(y)
    complex(kind=r64), intent(in) :: x
    integer :: norm
    real (kind=r64) :: y

    y = abs(x) ! independent of norm for rank=0 (scalar) case.

  end function vectorNorm_0D_complex64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 1.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_1D_real32(x, norm) result(y)
    real(kind=r32), intent(in) :: x(:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*x))
    end select

  end function vectorNorm_1D_real32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 1.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_1D_real64(x, norm) result(y)
    real(kind=r64), intent(in) :: x(:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*x))
    end select

  end function vectorNorm_1D_real64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 1.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_1D_complex32(x, norm) result(y)
    complex(kind=r32), intent(in) :: x(:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*conjg(x)))
    end select

  end function vectorNorm_1D_complex32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 1.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_1D_complex64(x, norm) result(y)
    complex(kind=r64), intent(in) :: x(:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*conjg(x)))
    end select

  end function vectorNorm_1D_complex64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 2.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_2D_real32(x, norm) result(y)
    real(kind=r32), intent(in) :: x(:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*x))
    end select

  end function vectorNorm_2D_real32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 2.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_2D_real64(x, norm) result(y)
    real(kind=r64), intent(in) :: x(:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*x))
    end select

  end function vectorNorm_2D_real64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 2.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_2D_complex32(x, norm) result(y)
    complex(kind=r32), intent(in) :: x(:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*conjg(x)))
    end select

  end function vectorNorm_2D_complex32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 2.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_2D_complex64(x, norm) result(y)
    complex(kind=r64), intent(in) :: x(:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*conjg(x)))
    end select

  end function vectorNorm_2D_complex64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 3.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_3D_real32(x, norm) result(y)
    real(kind=r32), intent(in) :: x(:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*x))
    end select

  end function vectorNorm_3D_real32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 3.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_3D_real64(x, norm) result(y)
    real(kind=r64), intent(in) :: x(:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*x))
    end select

  end function vectorNorm_3D_real64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 3.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_3D_complex32(x, norm) result(y)
    complex(kind=r32), intent(in) :: x(:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*conjg(x)))
    end select

  end function vectorNorm_3D_complex32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 3.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_3D_complex64(x, norm) result(y)
    complex(kind=r64), intent(in) :: x(:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*conjg(x)))
    end select

  end function vectorNorm_3D_complex64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 4.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_4D_real32(x, norm) result(y)
    real(kind=r32), intent(in) :: x(:,:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*x))
    end select

  end function vectorNorm_4D_real32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 4.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_4D_real64(x, norm) result(y)
    real(kind=r64), intent(in) :: x(:,:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*x))
    end select

  end function vectorNorm_4D_real64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 4.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_4D_complex32(x, norm) result(y)
    complex(kind=r32), intent(in) :: x(:,:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*conjg(x)))
    end select

  end function vectorNorm_4D_complex32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 4.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_4D_complex64(x, norm) result(y)
    complex(kind=r64), intent(in) :: x(:,:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*conjg(x)))
    end select

  end function vectorNorm_4D_complex64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 5.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_5D_real32(x, norm) result(y)
    real(kind=r32), intent(in) :: x(:,:,:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*x))
    end select

  end function vectorNorm_5D_real32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 5.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_5D_real64(x, norm) result(y)
    real(kind=r64), intent(in) :: x(:,:,:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*x))
    end select

  end function vectorNorm_5D_real64

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 5.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_5D_complex32(x, norm) result(y)
    complex(kind=r32), intent(in) :: x(:,:,:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*conjg(x)))
    end select

  end function vectorNorm_5D_complex32

   
  !---------------------------------------------------------------------------
  !> Returns the independent of norm in vector by the given diminsional
  !! double-precission real numbers and given integer norm
  !!
  !! The following is for rank = 5.
  !!
  !! @param x - given dimensional double-precision real numbers
  !! @param norm - given norm
  !!
  !! @return independent of norm
  !---------------------------------------------------------------------------
  function vectorNorm_5D_complex64(x, norm) result(y)
    complex(kind=r64), intent(in) :: x(:,:,:,:,:)
    integer :: norm
    real (kind=r64) :: y

! Note that abs(complex) is like the L2_NORM unless care is taken *here*.  Fix later...
    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
!       y = sqrt(sum(x**2))
!       y = sqrt(sum(x*conjg(x)))
       y = sqrt(sum(x*conjg(x)))
    end select

  end function vectorNorm_5D_complex64

! end interface vectorNorm implementations
! interface isWithinTolerance implementations

   logical function isWithinTolerance_0D_real32(x, tolerance, norm)
     real (kind=r32), intent(in) :: x
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_0D_real32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_0D_real32

   
   logical function isWithinTolerance_0D_real64(x, tolerance, norm)
     real (kind=r64), intent(in) :: x
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_0D_real64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_0D_real64

   
   logical function isWithinTolerance_0D_complex32(x, tolerance, norm)
     complex (kind=r32), intent(in) :: x
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_0D_complex32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_0D_complex32

   
   logical function isWithinTolerance_0D_complex64(x, tolerance, norm)
     complex (kind=r64), intent(in) :: x
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_0D_complex64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_0D_complex64

   
   logical function isWithinTolerance_1D_real32(x, tolerance, norm)
     real (kind=r32), intent(in) :: x(:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_1D_real32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_1D_real32

   
   logical function isWithinTolerance_1D_real64(x, tolerance, norm)
     real (kind=r64), intent(in) :: x(:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_1D_real64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_1D_real64

   
   logical function isWithinTolerance_1D_complex32(x, tolerance, norm)
     complex (kind=r32), intent(in) :: x(:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_1D_complex32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_1D_complex32

   
   logical function isWithinTolerance_1D_complex64(x, tolerance, norm)
     complex (kind=r64), intent(in) :: x(:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_1D_complex64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_1D_complex64

   
   logical function isWithinTolerance_2D_real32(x, tolerance, norm)
     real (kind=r32), intent(in) :: x(:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_2D_real32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_2D_real32

   
   logical function isWithinTolerance_2D_real64(x, tolerance, norm)
     real (kind=r64), intent(in) :: x(:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_2D_real64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_2D_real64

   
   logical function isWithinTolerance_2D_complex32(x, tolerance, norm)
     complex (kind=r32), intent(in) :: x(:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_2D_complex32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_2D_complex32

   
   logical function isWithinTolerance_2D_complex64(x, tolerance, norm)
     complex (kind=r64), intent(in) :: x(:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_2D_complex64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_2D_complex64

   
   logical function isWithinTolerance_3D_real32(x, tolerance, norm)
     real (kind=r32), intent(in) :: x(:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_3D_real32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_3D_real32

   
   logical function isWithinTolerance_3D_real64(x, tolerance, norm)
     real (kind=r64), intent(in) :: x(:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_3D_real64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_3D_real64

   
   logical function isWithinTolerance_3D_complex32(x, tolerance, norm)
     complex (kind=r32), intent(in) :: x(:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_3D_complex32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_3D_complex32

   
   logical function isWithinTolerance_3D_complex64(x, tolerance, norm)
     complex (kind=r64), intent(in) :: x(:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_3D_complex64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_3D_complex64

   
   logical function isWithinTolerance_4D_real32(x, tolerance, norm)
     real (kind=r32), intent(in) :: x(:,:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_4D_real32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_4D_real32

   
   logical function isWithinTolerance_4D_real64(x, tolerance, norm)
     real (kind=r64), intent(in) :: x(:,:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_4D_real64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_4D_real64

   
   logical function isWithinTolerance_4D_complex32(x, tolerance, norm)
     complex (kind=r32), intent(in) :: x(:,:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_4D_complex32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_4D_complex32

   
   logical function isWithinTolerance_4D_complex64(x, tolerance, norm)
     complex (kind=r64), intent(in) :: x(:,:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_4D_complex64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_4D_complex64

   
   logical function isWithinTolerance_5D_real32(x, tolerance, norm)
     real (kind=r32), intent(in) :: x(:,:,:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_5D_real32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_5D_real32

   
   logical function isWithinTolerance_5D_real64(x, tolerance, norm)
     real (kind=r64), intent(in) :: x(:,:,:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_5D_real64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_5D_real64

   
   logical function isWithinTolerance_5D_complex32(x, tolerance, norm)
     complex (kind=r32), intent(in) :: x(:,:,:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_5D_complex32 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_5D_complex32

   
   logical function isWithinTolerance_5D_complex64(x, tolerance, norm)
     complex (kind=r64), intent(in) :: x(:,:,:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_5D_complex64 = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_5D_complex64

! end interface isWithinTolerance implementations
! interface assertEqual implementations

  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r32_0D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r32_0D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r32_0D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(0)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference


      expected0 = expected
      found0 = found
      if (expected0 /= found0 ) then
         idxLocation = (/ 0 /)
   !???      tolerance_ = 0.0
         call throwDifferentValuesWithLocation( &
         &       expected0, &
         &       found0, &
         &       idxLocation, &
         &       tolerance_, &
         &       location )
         return ! bail
      end if


contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r32_0D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r32_0D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r32_0D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r32_0D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(0)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference


      expected0 = expected
      found0 = found
      if (expected0 /= found0 ) then
         idxLocation = (/ 0 /)
   !???      tolerance_ = 0.0
         call throwDifferentValuesWithLocation( &
         &       expected0, &
         &       found0, &
         &       idxLocation, &
         &       tolerance_, &
         &       location )
         return ! bail
      end if


contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r32_0D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r64_0D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r64_0D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r64_0D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(0)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference


      expected0 = expected
      found0 = found
      if (expected0 /= found0 ) then
         idxLocation = (/ 0 /)
   !???      tolerance_ = 0.0
         call throwDifferentValuesWithLocation( &
         &       expected0, &
         &       found0, &
         &       idxLocation, &
         &       tolerance_, &
         &       location )
         return ! bail
      end if


contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r64_0D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r64_0D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r64_0D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r64_0D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(0)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference


      expected0 = expected
      found0 = found
      if (expected0 /= found0 ) then
         idxLocation = (/ 0 /)
   !???      tolerance_ = 0.0
         call throwDifferentValuesWithLocation( &
         &       expected0, &
         &       found0, &
         &       idxLocation, &
         &       tolerance_, &
         &       location )
         return ! bail
      end if


contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r64_0D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_0D_r64_0D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_0D_r64_0D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r64_0D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(0)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference


      expected0 = expected
      found0 = found
      if (expected0 /= found0 ) then
         idxLocation = (/ 0 /)
   !???      tolerance_ = 0.0
         call throwDifferentValuesWithLocation( &
         &       expected0, &
         &       found0, &
         &       idxLocation, &
         &       tolerance_, &
         &       location )
         return ! bail
      end if


contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r64_0D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r32_1D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r32_1D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r32_1D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx1= 1,foundShape(1)

         expected0 = expected
         found0 = found(idx1)
         if (expected0 /= found0 ) then
            idxLocation = (/ idx1 /)
      !???      tolerance_ = 0.0
            call throwDifferentValuesWithLocation( &
            &       expected0, &
            &       found0, &
            &       idxLocation, &
            &       tolerance_, &
            &       location )
            return ! bail
         end if

   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r32_1D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r32_1D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r32_1D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r32_1D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx1= 1,foundShape(1)

         expected0 = expected
         found0 = found(idx1)
         if (expected0 /= found0 ) then
            idxLocation = (/ idx1 /)
      !???      tolerance_ = 0.0
            call throwDifferentValuesWithLocation( &
            &       expected0, &
            &       found0, &
            &       idxLocation, &
            &       tolerance_, &
            &       location )
            return ! bail
         end if

   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r32_1D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_1D_r32_1D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected(:)
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_1D_r32_1D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_1D_r32_1D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected(:)
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(1)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx1= 1,foundShape(1)

         expected0 = expected(idx1)
         found0 = found(idx1)
         if (expected0 /= found0 ) then
            idxLocation = (/ idx1 /)
      !???      tolerance_ = 0.0
            call throwDifferentValuesWithLocation( &
            &       expected0, &
            &       found0, &
            &       idxLocation, &
            &       tolerance_, &
            &       location )
            return ! bail
         end if

   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_1D_r32_1D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_1D_r32_1D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:)
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_1D_r32_1D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_1D_r32_1D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:)
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(1)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx1= 1,foundShape(1)

         expected0 = expected(idx1)
         found0 = found(idx1)
         if (expected0 /= found0 ) then
            idxLocation = (/ idx1 /)
      !???      tolerance_ = 0.0
            call throwDifferentValuesWithLocation( &
            &       expected0, &
            &       found0, &
            &       idxLocation, &
            &       tolerance_, &
            &       location )
            return ! bail
         end if

   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_1D_r32_1D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r64_1D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r64_1D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r64_1D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx1= 1,foundShape(1)

         expected0 = expected
         found0 = found(idx1)
         if (expected0 /= found0 ) then
            idxLocation = (/ idx1 /)
      !???      tolerance_ = 0.0
            call throwDifferentValuesWithLocation( &
            &       expected0, &
            &       found0, &
            &       idxLocation, &
            &       tolerance_, &
            &       location )
            return ! bail
         end if

   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r64_1D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r64_1D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r64_1D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r64_1D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx1= 1,foundShape(1)

         expected0 = expected
         found0 = found(idx1)
         if (expected0 /= found0 ) then
            idxLocation = (/ idx1 /)
      !???      tolerance_ = 0.0
            call throwDifferentValuesWithLocation( &
            &       expected0, &
            &       found0, &
            &       idxLocation, &
            &       tolerance_, &
            &       location )
            return ! bail
         end if

   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r64_1D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_0D_r64_1D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_0D_r64_1D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r64_1D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx1= 1,foundShape(1)

         expected0 = expected
         found0 = found(idx1)
         if (expected0 /= found0 ) then
            idxLocation = (/ idx1 /)
      !???      tolerance_ = 0.0
            call throwDifferentValuesWithLocation( &
            &       expected0, &
            &       found0, &
            &       idxLocation, &
            &       tolerance_, &
            &       location )
            return ! bail
         end if

   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r64_1D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_1D_r64_1D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected(:)
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_1D_r64_1D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_1D_r64_1D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected(:)
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(1)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx1= 1,foundShape(1)

         expected0 = expected(idx1)
         found0 = found(idx1)
         if (expected0 /= found0 ) then
            idxLocation = (/ idx1 /)
      !???      tolerance_ = 0.0
            call throwDifferentValuesWithLocation( &
            &       expected0, &
            &       found0, &
            &       idxLocation, &
            &       tolerance_, &
            &       location )
            return ! bail
         end if

   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_1D_r64_1D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_1D_r64_1D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:)
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_1D_r64_1D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_1D_r64_1D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:)
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(1)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx1= 1,foundShape(1)

         expected0 = expected(idx1)
         found0 = found(idx1)
         if (expected0 /= found0 ) then
            idxLocation = (/ idx1 /)
      !???      tolerance_ = 0.0
            call throwDifferentValuesWithLocation( &
            &       expected0, &
            &       found0, &
            &       idxLocation, &
            &       tolerance_, &
            &       location )
            return ! bail
         end if

   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_1D_r64_1D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_1D_r64_1D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:)
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_1D_r64_1D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_1D_r64_1D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:)
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(1)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(1)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx1= 1,foundShape(1)

         expected0 = expected(idx1)
         found0 = found(idx1)
         if (expected0 /= found0 ) then
            idxLocation = (/ idx1 /)
      !???      tolerance_ = 0.0
            call throwDifferentValuesWithLocation( &
            &       expected0, &
            &       found0, &
            &       idxLocation, &
            &       tolerance_, &
            &       location )
            return ! bail
         end if

   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_1D_r64_1D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r32_2D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r32_2D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r32_2D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(2)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx2= 1,foundShape(2)
      do idx1= 1,foundShape(1)

            expected0 = expected
            found0 = found(idx1,idx2)
            if (expected0 /= found0 ) then
               idxLocation = (/ idx1,idx2 /)
         !???      tolerance_ = 0.0
               call throwDifferentValuesWithLocation( &
               &       expected0, &
               &       found0, &
               &       idxLocation, &
               &       tolerance_, &
               &       location )
               return ! bail
            end if

      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r32_2D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r32_2D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r32_2D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r32_2D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(2)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx2= 1,foundShape(2)
      do idx1= 1,foundShape(1)

            expected0 = expected
            found0 = found(idx1,idx2)
            if (expected0 /= found0 ) then
               idxLocation = (/ idx1,idx2 /)
         !???      tolerance_ = 0.0
               call throwDifferentValuesWithLocation( &
               &       expected0, &
               &       found0, &
               &       idxLocation, &
               &       tolerance_, &
               &       location )
               return ! bail
            end if

      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r32_2D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_2D_r32_2D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected(:,:)
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_2D_r32_2D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_2D_r32_2D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected(:,:)
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(2)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(2)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx2= 1,foundShape(2)
      do idx1= 1,foundShape(1)

            expected0 = expected(idx1,idx2)
            found0 = found(idx1,idx2)
            if (expected0 /= found0 ) then
               idxLocation = (/ idx1,idx2 /)
         !???      tolerance_ = 0.0
               call throwDifferentValuesWithLocation( &
               &       expected0, &
               &       found0, &
               &       idxLocation, &
               &       tolerance_, &
               &       location )
               return ! bail
            end if

      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_2D_r32_2D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_2D_r32_2D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:)
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_2D_r32_2D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_2D_r32_2D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:)
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(2)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(2)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx2= 1,foundShape(2)
      do idx1= 1,foundShape(1)

            expected0 = expected(idx1,idx2)
            found0 = found(idx1,idx2)
            if (expected0 /= found0 ) then
               idxLocation = (/ idx1,idx2 /)
         !???      tolerance_ = 0.0
               call throwDifferentValuesWithLocation( &
               &       expected0, &
               &       found0, &
               &       idxLocation, &
               &       tolerance_, &
               &       location )
               return ! bail
            end if

      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_2D_r32_2D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r64_2D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r64_2D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r64_2D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(2)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx2= 1,foundShape(2)
      do idx1= 1,foundShape(1)

            expected0 = expected
            found0 = found(idx1,idx2)
            if (expected0 /= found0 ) then
               idxLocation = (/ idx1,idx2 /)
         !???      tolerance_ = 0.0
               call throwDifferentValuesWithLocation( &
               &       expected0, &
               &       found0, &
               &       idxLocation, &
               &       tolerance_, &
               &       location )
               return ! bail
            end if

      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r64_2D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r64_2D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r64_2D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r64_2D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(2)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx2= 1,foundShape(2)
      do idx1= 1,foundShape(1)

            expected0 = expected
            found0 = found(idx1,idx2)
            if (expected0 /= found0 ) then
               idxLocation = (/ idx1,idx2 /)
         !???      tolerance_ = 0.0
               call throwDifferentValuesWithLocation( &
               &       expected0, &
               &       found0, &
               &       idxLocation, &
               &       tolerance_, &
               &       location )
               return ! bail
            end if

      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r64_2D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_0D_r64_2D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_0D_r64_2D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r64_2D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(2)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx2= 1,foundShape(2)
      do idx1= 1,foundShape(1)

            expected0 = expected
            found0 = found(idx1,idx2)
            if (expected0 /= found0 ) then
               idxLocation = (/ idx1,idx2 /)
         !???      tolerance_ = 0.0
               call throwDifferentValuesWithLocation( &
               &       expected0, &
               &       found0, &
               &       idxLocation, &
               &       tolerance_, &
               &       location )
               return ! bail
            end if

      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r64_2D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_2D_r64_2D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected(:,:)
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_2D_r64_2D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_2D_r64_2D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected(:,:)
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(2)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(2)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx2= 1,foundShape(2)
      do idx1= 1,foundShape(1)

            expected0 = expected(idx1,idx2)
            found0 = found(idx1,idx2)
            if (expected0 /= found0 ) then
               idxLocation = (/ idx1,idx2 /)
         !???      tolerance_ = 0.0
               call throwDifferentValuesWithLocation( &
               &       expected0, &
               &       found0, &
               &       idxLocation, &
               &       tolerance_, &
               &       location )
               return ! bail
            end if

      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_2D_r64_2D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_2D_r64_2D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:)
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_2D_r64_2D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_2D_r64_2D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:)
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(2)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(2)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx2= 1,foundShape(2)
      do idx1= 1,foundShape(1)

            expected0 = expected(idx1,idx2)
            found0 = found(idx1,idx2)
            if (expected0 /= found0 ) then
               idxLocation = (/ idx1,idx2 /)
         !???      tolerance_ = 0.0
               call throwDifferentValuesWithLocation( &
               &       expected0, &
               &       found0, &
               &       idxLocation, &
               &       tolerance_, &
               &       location )
               return ! bail
            end if

      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_2D_r64_2D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_2D_r64_2D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:)
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_2D_r64_2D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_2D_r64_2D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:)
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(2)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(2)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx2= 1,foundShape(2)
      do idx1= 1,foundShape(1)

            expected0 = expected(idx1,idx2)
            found0 = found(idx1,idx2)
            if (expected0 /= found0 ) then
               idxLocation = (/ idx1,idx2 /)
         !???      tolerance_ = 0.0
               call throwDifferentValuesWithLocation( &
               &       expected0, &
               &       found0, &
               &       idxLocation, &
               &       tolerance_, &
               &       location )
               return ! bail
            end if

      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_2D_r64_2D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r32_3D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r32_3D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r32_3D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(3)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx3= 1,foundShape(3)
      do idx2= 1,foundShape(2)
         do idx1= 1,foundShape(1)

               expected0 = expected
               found0 = found(idx1,idx2,idx3)
               if (expected0 /= found0 ) then
                  idxLocation = (/ idx1,idx2,idx3 /)
            !???      tolerance_ = 0.0
                  call throwDifferentValuesWithLocation( &
                  &       expected0, &
                  &       found0, &
                  &       idxLocation, &
                  &       tolerance_, &
                  &       location )
                  return ! bail
               end if

         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r32_3D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r32_3D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r32_3D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r32_3D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(3)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx3= 1,foundShape(3)
      do idx2= 1,foundShape(2)
         do idx1= 1,foundShape(1)

               expected0 = expected
               found0 = found(idx1,idx2,idx3)
               if (expected0 /= found0 ) then
                  idxLocation = (/ idx1,idx2,idx3 /)
            !???      tolerance_ = 0.0
                  call throwDifferentValuesWithLocation( &
                  &       expected0, &
                  &       found0, &
                  &       idxLocation, &
                  &       tolerance_, &
                  &       location )
                  return ! bail
               end if

         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r32_3D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_3D_r32_3D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected(:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_3D_r32_3D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_3D_r32_3D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected(:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(3)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(3)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx3= 1,foundShape(3)
      do idx2= 1,foundShape(2)
         do idx1= 1,foundShape(1)

               expected0 = expected(idx1,idx2,idx3)
               found0 = found(idx1,idx2,idx3)
               if (expected0 /= found0 ) then
                  idxLocation = (/ idx1,idx2,idx3 /)
            !???      tolerance_ = 0.0
                  call throwDifferentValuesWithLocation( &
                  &       expected0, &
                  &       found0, &
                  &       idxLocation, &
                  &       tolerance_, &
                  &       location )
                  return ! bail
               end if

         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_3D_r32_3D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_3D_r32_3D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_3D_r32_3D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_3D_r32_3D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(3)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(3)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx3= 1,foundShape(3)
      do idx2= 1,foundShape(2)
         do idx1= 1,foundShape(1)

               expected0 = expected(idx1,idx2,idx3)
               found0 = found(idx1,idx2,idx3)
               if (expected0 /= found0 ) then
                  idxLocation = (/ idx1,idx2,idx3 /)
            !???      tolerance_ = 0.0
                  call throwDifferentValuesWithLocation( &
                  &       expected0, &
                  &       found0, &
                  &       idxLocation, &
                  &       tolerance_, &
                  &       location )
                  return ! bail
               end if

         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_3D_r32_3D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r64_3D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r64_3D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r64_3D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(3)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx3= 1,foundShape(3)
      do idx2= 1,foundShape(2)
         do idx1= 1,foundShape(1)

               expected0 = expected
               found0 = found(idx1,idx2,idx3)
               if (expected0 /= found0 ) then
                  idxLocation = (/ idx1,idx2,idx3 /)
            !???      tolerance_ = 0.0
                  call throwDifferentValuesWithLocation( &
                  &       expected0, &
                  &       found0, &
                  &       idxLocation, &
                  &       tolerance_, &
                  &       location )
                  return ! bail
               end if

         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r64_3D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r64_3D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r64_3D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r64_3D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(3)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx3= 1,foundShape(3)
      do idx2= 1,foundShape(2)
         do idx1= 1,foundShape(1)

               expected0 = expected
               found0 = found(idx1,idx2,idx3)
               if (expected0 /= found0 ) then
                  idxLocation = (/ idx1,idx2,idx3 /)
            !???      tolerance_ = 0.0
                  call throwDifferentValuesWithLocation( &
                  &       expected0, &
                  &       found0, &
                  &       idxLocation, &
                  &       tolerance_, &
                  &       location )
                  return ! bail
               end if

         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r64_3D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_0D_r64_3D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_0D_r64_3D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r64_3D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(3)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx3= 1,foundShape(3)
      do idx2= 1,foundShape(2)
         do idx1= 1,foundShape(1)

               expected0 = expected
               found0 = found(idx1,idx2,idx3)
               if (expected0 /= found0 ) then
                  idxLocation = (/ idx1,idx2,idx3 /)
            !???      tolerance_ = 0.0
                  call throwDifferentValuesWithLocation( &
                  &       expected0, &
                  &       found0, &
                  &       idxLocation, &
                  &       tolerance_, &
                  &       location )
                  return ! bail
               end if

         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r64_3D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_3D_r64_3D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected(:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_3D_r64_3D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_3D_r64_3D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected(:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(3)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(3)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx3= 1,foundShape(3)
      do idx2= 1,foundShape(2)
         do idx1= 1,foundShape(1)

               expected0 = expected(idx1,idx2,idx3)
               found0 = found(idx1,idx2,idx3)
               if (expected0 /= found0 ) then
                  idxLocation = (/ idx1,idx2,idx3 /)
            !???      tolerance_ = 0.0
                  call throwDifferentValuesWithLocation( &
                  &       expected0, &
                  &       found0, &
                  &       idxLocation, &
                  &       tolerance_, &
                  &       location )
                  return ! bail
               end if

         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_3D_r64_3D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_3D_r64_3D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_3D_r64_3D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_3D_r64_3D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(3)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(3)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx3= 1,foundShape(3)
      do idx2= 1,foundShape(2)
         do idx1= 1,foundShape(1)

               expected0 = expected(idx1,idx2,idx3)
               found0 = found(idx1,idx2,idx3)
               if (expected0 /= found0 ) then
                  idxLocation = (/ idx1,idx2,idx3 /)
            !???      tolerance_ = 0.0
                  call throwDifferentValuesWithLocation( &
                  &       expected0, &
                  &       found0, &
                  &       idxLocation, &
                  &       tolerance_, &
                  &       location )
                  return ! bail
               end if

         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_3D_r64_3D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_3D_r64_3D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_3D_r64_3D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_3D_r64_3D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(3)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(3)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx3= 1,foundShape(3)
      do idx2= 1,foundShape(2)
         do idx1= 1,foundShape(1)

               expected0 = expected(idx1,idx2,idx3)
               found0 = found(idx1,idx2,idx3)
               if (expected0 /= found0 ) then
                  idxLocation = (/ idx1,idx2,idx3 /)
            !???      tolerance_ = 0.0
                  call throwDifferentValuesWithLocation( &
                  &       expected0, &
                  &       found0, &
                  &       idxLocation, &
                  &       tolerance_, &
                  &       location )
                  return ! bail
               end if

         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_3D_r64_3D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r32_4D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r32_4D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r32_4D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(4)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx4= 1,foundShape(4)
      do idx3= 1,foundShape(3)
         do idx2= 1,foundShape(2)
            do idx1= 1,foundShape(1)

                  expected0 = expected
                  found0 = found(idx1,idx2,idx3,idx4)
                  if (expected0 /= found0 ) then
                     idxLocation = (/ idx1,idx2,idx3,idx4 /)
               !???      tolerance_ = 0.0
                     call throwDifferentValuesWithLocation( &
                     &       expected0, &
                     &       found0, &
                     &       idxLocation, &
                     &       tolerance_, &
                     &       location )
                     return ! bail
                  end if

            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r32_4D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r32_4D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r32_4D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r32_4D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(4)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx4= 1,foundShape(4)
      do idx3= 1,foundShape(3)
         do idx2= 1,foundShape(2)
            do idx1= 1,foundShape(1)

                  expected0 = expected
                  found0 = found(idx1,idx2,idx3,idx4)
                  if (expected0 /= found0 ) then
                     idxLocation = (/ idx1,idx2,idx3,idx4 /)
               !???      tolerance_ = 0.0
                     call throwDifferentValuesWithLocation( &
                     &       expected0, &
                     &       found0, &
                     &       idxLocation, &
                     &       tolerance_, &
                     &       location )
                     return ! bail
                  end if

            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r32_4D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_4D_r32_4D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected(:,:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_4D_r32_4D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_4D_r32_4D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected(:,:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(4)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(4)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx4= 1,foundShape(4)
      do idx3= 1,foundShape(3)
         do idx2= 1,foundShape(2)
            do idx1= 1,foundShape(1)

                  expected0 = expected(idx1,idx2,idx3,idx4)
                  found0 = found(idx1,idx2,idx3,idx4)
                  if (expected0 /= found0 ) then
                     idxLocation = (/ idx1,idx2,idx3,idx4 /)
               !???      tolerance_ = 0.0
                     call throwDifferentValuesWithLocation( &
                     &       expected0, &
                     &       found0, &
                     &       idxLocation, &
                     &       tolerance_, &
                     &       location )
                     return ! bail
                  end if

            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_4D_r32_4D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_4D_r32_4D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_4D_r32_4D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_4D_r32_4D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(4)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(4)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx4= 1,foundShape(4)
      do idx3= 1,foundShape(3)
         do idx2= 1,foundShape(2)
            do idx1= 1,foundShape(1)

                  expected0 = expected(idx1,idx2,idx3,idx4)
                  found0 = found(idx1,idx2,idx3,idx4)
                  if (expected0 /= found0 ) then
                     idxLocation = (/ idx1,idx2,idx3,idx4 /)
               !???      tolerance_ = 0.0
                     call throwDifferentValuesWithLocation( &
                     &       expected0, &
                     &       found0, &
                     &       idxLocation, &
                     &       tolerance_, &
                     &       location )
                     return ! bail
                  end if

            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_4D_r32_4D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r64_4D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r64_4D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r64_4D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(4)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx4= 1,foundShape(4)
      do idx3= 1,foundShape(3)
         do idx2= 1,foundShape(2)
            do idx1= 1,foundShape(1)

                  expected0 = expected
                  found0 = found(idx1,idx2,idx3,idx4)
                  if (expected0 /= found0 ) then
                     idxLocation = (/ idx1,idx2,idx3,idx4 /)
               !???      tolerance_ = 0.0
                     call throwDifferentValuesWithLocation( &
                     &       expected0, &
                     &       found0, &
                     &       idxLocation, &
                     &       tolerance_, &
                     &       location )
                     return ! bail
                  end if

            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r64_4D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r64_4D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r64_4D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r64_4D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(4)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx4= 1,foundShape(4)
      do idx3= 1,foundShape(3)
         do idx2= 1,foundShape(2)
            do idx1= 1,foundShape(1)

                  expected0 = expected
                  found0 = found(idx1,idx2,idx3,idx4)
                  if (expected0 /= found0 ) then
                     idxLocation = (/ idx1,idx2,idx3,idx4 /)
               !???      tolerance_ = 0.0
                     call throwDifferentValuesWithLocation( &
                     &       expected0, &
                     &       found0, &
                     &       idxLocation, &
                     &       tolerance_, &
                     &       location )
                     return ! bail
                  end if

            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r64_4D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_0D_r64_4D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_0D_r64_4D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r64_4D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(4)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx4= 1,foundShape(4)
      do idx3= 1,foundShape(3)
         do idx2= 1,foundShape(2)
            do idx1= 1,foundShape(1)

                  expected0 = expected
                  found0 = found(idx1,idx2,idx3,idx4)
                  if (expected0 /= found0 ) then
                     idxLocation = (/ idx1,idx2,idx3,idx4 /)
               !???      tolerance_ = 0.0
                     call throwDifferentValuesWithLocation( &
                     &       expected0, &
                     &       found0, &
                     &       idxLocation, &
                     &       tolerance_, &
                     &       location )
                     return ! bail
                  end if

            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r64_4D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_4D_r64_4D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected(:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_4D_r64_4D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_4D_r64_4D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected(:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(4)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(4)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx4= 1,foundShape(4)
      do idx3= 1,foundShape(3)
         do idx2= 1,foundShape(2)
            do idx1= 1,foundShape(1)

                  expected0 = expected(idx1,idx2,idx3,idx4)
                  found0 = found(idx1,idx2,idx3,idx4)
                  if (expected0 /= found0 ) then
                     idxLocation = (/ idx1,idx2,idx3,idx4 /)
               !???      tolerance_ = 0.0
                     call throwDifferentValuesWithLocation( &
                     &       expected0, &
                     &       found0, &
                     &       idxLocation, &
                     &       tolerance_, &
                     &       location )
                     return ! bail
                  end if

            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_4D_r64_4D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_4D_r64_4D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_4D_r64_4D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_4D_r64_4D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(4)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(4)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx4= 1,foundShape(4)
      do idx3= 1,foundShape(3)
         do idx2= 1,foundShape(2)
            do idx1= 1,foundShape(1)

                  expected0 = expected(idx1,idx2,idx3,idx4)
                  found0 = found(idx1,idx2,idx3,idx4)
                  if (expected0 /= found0 ) then
                     idxLocation = (/ idx1,idx2,idx3,idx4 /)
               !???      tolerance_ = 0.0
                     call throwDifferentValuesWithLocation( &
                     &       expected0, &
                     &       found0, &
                     &       idxLocation, &
                     &       tolerance_, &
                     &       location )
                     return ! bail
                  end if

            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_4D_r64_4D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_4D_r64_4D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_4D_r64_4D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_4D_r64_4D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(4)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(4)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx4= 1,foundShape(4)
      do idx3= 1,foundShape(3)
         do idx2= 1,foundShape(2)
            do idx1= 1,foundShape(1)

                  expected0 = expected(idx1,idx2,idx3,idx4)
                  found0 = found(idx1,idx2,idx3,idx4)
                  if (expected0 /= found0 ) then
                     idxLocation = (/ idx1,idx2,idx3,idx4 /)
               !???      tolerance_ = 0.0
                     call throwDifferentValuesWithLocation( &
                     &       expected0, &
                     &       found0, &
                     &       idxLocation, &
                     &       tolerance_, &
                     &       location )
                     return ! bail
                  end if

            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_4D_r64_4D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r32_5D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r32_5D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r32_5D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(5)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx5= 1,foundShape(5)
      do idx4= 1,foundShape(4)
         do idx3= 1,foundShape(3)
            do idx2= 1,foundShape(2)
               do idx1= 1,foundShape(1)

                     expected0 = expected
                     found0 = found(idx1,idx2,idx3,idx4,idx5)
                     if (expected0 /= found0 ) then
                        idxLocation = (/ idx1,idx2,idx3,idx4,idx5 /)
                  !???      tolerance_ = 0.0
                        call throwDifferentValuesWithLocation( &
                        &       expected0, &
                        &       found0, &
                        &       idxLocation, &
                        &       tolerance_, &
                        &       location )
                        return ! bail
                     end if

               end do
            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r32_5D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r32_5D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r32_5D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r32_5D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(5)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx5= 1,foundShape(5)
      do idx4= 1,foundShape(4)
         do idx3= 1,foundShape(3)
            do idx2= 1,foundShape(2)
               do idx1= 1,foundShape(1)

                     expected0 = expected
                     found0 = found(idx1,idx2,idx3,idx4,idx5)
                     if (expected0 /= found0 ) then
                        idxLocation = (/ idx1,idx2,idx3,idx4,idx5 /)
                  !???      tolerance_ = 0.0
                        call throwDifferentValuesWithLocation( &
                        &       expected0, &
                        &       found0, &
                        &       idxLocation, &
                        &       tolerance_, &
                        &       location )
                        return ! bail
                     end if

               end do
            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r32_5D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_5D_r32_5D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected(:,:,:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_5D_r32_5D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_5D_r32_5D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected(:,:,:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(5)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(5)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx5= 1,foundShape(5)
      do idx4= 1,foundShape(4)
         do idx3= 1,foundShape(3)
            do idx2= 1,foundShape(2)
               do idx1= 1,foundShape(1)

                     expected0 = expected(idx1,idx2,idx3,idx4,idx5)
                     found0 = found(idx1,idx2,idx3,idx4,idx5)
                     if (expected0 /= found0 ) then
                        idxLocation = (/ idx1,idx2,idx3,idx4,idx5 /)
                  !???      tolerance_ = 0.0
                        call throwDifferentValuesWithLocation( &
                        &       expected0, &
                        &       found0, &
                        &       idxLocation, &
                        &       tolerance_, &
                        &       location )
                        return ! bail
                     end if

               end do
            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_5D_r32_5D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_5D_r32_5D_tol32( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r32)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_5D_r32_5D_tol32_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_5D_r32_5D_tol32_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(5)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r32) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(5)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx5= 1,foundShape(5)
      do idx4= 1,foundShape(4)
         do idx3= 1,foundShape(3)
            do idx2= 1,foundShape(2)
               do idx1= 1,foundShape(1)

                     expected0 = expected(idx1,idx2,idx3,idx4,idx5)
                     found0 = found(idx1,idx2,idx3,idx4,idx5)
                     if (expected0 /= found0 ) then
                        idxLocation = (/ idx1,idx2,idx3,idx4,idx5 /)
                  !???      tolerance_ = 0.0
                        call throwDifferentValuesWithLocation( &
                        &       expected0, &
                        &       found0, &
                        &       idxLocation, &
                        &       tolerance_, &
                        &       location )
                        return ! bail
                     end if

               end do
            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r32) :: found

   real(kind=r32), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_5D_r32_5D_tol32_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_0D_r64_5D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_0D_r64_5D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_0D_r64_5D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(5)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx5= 1,foundShape(5)
      do idx4= 1,foundShape(4)
         do idx3= 1,foundShape(3)
            do idx2= 1,foundShape(2)
               do idx1= 1,foundShape(1)

                     expected0 = expected
                     found0 = found(idx1,idx2,idx3,idx4,idx5)
                     if (expected0 /= found0 ) then
                        idxLocation = (/ idx1,idx2,idx3,idx4,idx5 /)
                  !???      tolerance_ = 0.0
                        call throwDifferentValuesWithLocation( &
                        &       expected0, &
                        &       found0, &
                        &       idxLocation, &
                        &       tolerance_, &
                        &       location )
                        return ! bail
                     end if

               end do
            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_0D_r64_5D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_0D_r64_5D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_0D_r64_5D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_0D_r64_5D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(5)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx5= 1,foundShape(5)
      do idx4= 1,foundShape(4)
         do idx3= 1,foundShape(3)
            do idx2= 1,foundShape(2)
               do idx1= 1,foundShape(1)

                     expected0 = expected
                     found0 = found(idx1,idx2,idx3,idx4,idx5)
                     if (expected0 /= found0 ) then
                        idxLocation = (/ idx1,idx2,idx3,idx4,idx5 /)
                  !???      tolerance_ = 0.0
                        call throwDifferentValuesWithLocation( &
                        &       expected0, &
                        &       found0, &
                        &       idxLocation, &
                        &       tolerance_, &
                        &       location )
                        return ! bail
                     end if

               end do
            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_0D_r64_5D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_0D_r64_5D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_0D_r64_5D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r64_5D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(5)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx5= 1,foundShape(5)
      do idx4= 1,foundShape(4)
         do idx3= 1,foundShape(3)
            do idx2= 1,foundShape(2)
               do idx1= 1,foundShape(1)

                     expected0 = expected
                     found0 = found(idx1,idx2,idx3,idx4,idx5)
                     if (expected0 /= found0 ) then
                        idxLocation = (/ idx1,idx2,idx3,idx4,idx5 /)
                  !???      tolerance_ = 0.0
                        call throwDifferentValuesWithLocation( &
                        &       expected0, &
                        &       found0, &
                        &       idxLocation, &
                        &       tolerance_, &
                        &       location )
                        return ! bail
                     end if

               end do
            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r64_5D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_int_5D_r64_5D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     integer, intent(in) :: expected(:,:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_int_5D_r64_5D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_int_5D_r64_5D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     integer, intent(in) :: expected(:,:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(5)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      integer :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(5)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx5= 1,foundShape(5)
      do idx4= 1,foundShape(4)
         do idx3= 1,foundShape(3)
            do idx2= 1,foundShape(2)
               do idx1= 1,foundShape(1)

                     expected0 = expected(idx1,idx2,idx3,idx4,idx5)
                     found0 = found(idx1,idx2,idx3,idx4,idx5)
                     if (expected0 /= found0 ) then
                        idxLocation = (/ idx1,idx2,idx3,idx4,idx5 /)
                  !???      tolerance_ = 0.0
                        call throwDifferentValuesWithLocation( &
                        &       expected0, &
                        &       found0, &
                        &       idxLocation, &
                        &       tolerance_, &
                        &       location )
                        return ! bail
                     end if

               end do
            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      integer :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_int_5D_r64_5D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r32_5D_r64_5D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r32_5D_r64_5D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r32_5D_r64_5D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: expected(:,:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(5)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      real(kind=r32) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(5)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx5= 1,foundShape(5)
      do idx4= 1,foundShape(4)
         do idx3= 1,foundShape(3)
            do idx2= 1,foundShape(2)
               do idx1= 1,foundShape(1)

                     expected0 = expected(idx1,idx2,idx3,idx4,idx5)
                     found0 = found(idx1,idx2,idx3,idx4,idx5)
                     if (expected0 /= found0 ) then
                        idxLocation = (/ idx1,idx2,idx3,idx4,idx5 /)
                  !???      tolerance_ = 0.0
                        call throwDifferentValuesWithLocation( &
                        &       expected0, &
                        &       found0, &
                        &       idxLocation, &
                        &       tolerance_, &
                        &       location )
                        return ! bail
                     end if

               end do
            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r32) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r32_5D_r64_5D_tol64_internal

   
  !---------------------------------------------------------------------------
  !> Asserts that two real numbers are equal.  If they are not, an
  !! Exception is thrown with the given message.
  !!
  !! @param expected - expected real numbers
  !! @param found -  found real numbers
  !! @param message - the identifying message for the Exception
  !!
  !! @throw Exception - when two real numbers are not equal.
  !---------------------------------------------------------------------------

   subroutine assertEqual_r64_5D_r64_5D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
     character(len=*), optional, intent(in) :: message  ! not used yet!
     type (SourceLocation), optional, intent(in) :: location

     real(kind=kind(tolerance)) :: tolerance_
     character(len=:), allocatable :: message_
     type (SourceLocation) :: location_

     if(present(tolerance)) then
        tolerance_ = tolerance
     else
        tolerance_ = real(0.,kind=r64)
     end if

     if(present(location)) then 
        location_ = location
     else
        location_ = UNKNOWN_SOURCE_LOCATION
     end if

     if(present(message)) then
        message_ = message
     else
        message_ = NULL_MESSAGE
     end if

     call assertEqual_r64_5D_r64_5D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_5D_r64_5D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:,:,:,:)
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance 
     real(kind=kind(tolerance)) :: tolerance_

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
      real(kind=kind(found)) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))

     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(5)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
      real(kind=r64) :: found0

!
! Capture the location where things went wrong. Using foundRank.
      integer :: idxLocation(5)
!

      foundShape = shape(found)

      ! Case:  tolerance !== 0
      tolerance_ = tolerance

   ! If the expected is scalar, then we are conformable.  Otherwise, we have to check the shape.
   ! The following segment is elided if the expected rank is zero.

      call assertSameShape(shape(expected),shape(found),location=location)
      if (anyExceptions()) return

      expectedSize = size(expected); expectedShape = shape(expected)

   ! Size and shape okay.  Now compare elements... If all tolerable, return...
      delta = expected - found ! Note use of implicit iteration, delta can have nontrivial rank

   ! Question:  How to handle 0-rank case?  How to handle tolerance == 0?
      if (isWithinTolerance(delta, real(tolerance_,kind=r64), L_INFINITY_NORM)) return

   ! Check for difference

   do idx5= 1,foundShape(5)
      do idx4= 1,foundShape(4)
         do idx3= 1,foundShape(3)
            do idx2= 1,foundShape(2)
               do idx1= 1,foundShape(1)

                     expected0 = expected(idx1,idx2,idx3,idx4,idx5)
                     found0 = found(idx1,idx2,idx3,idx4,idx5)
                     if (expected0 /= found0 ) then
                        idxLocation = (/ idx1,idx2,idx3,idx4,idx5 /)
                  !???      tolerance_ = 0.0
                        call throwDifferentValuesWithLocation( &
                        &       expected0, &
                        &       found0, &
                        &       idxLocation, &
                        &       tolerance_, &
                        &       location )
                        return ! bail
                     end if

               end do
            end do
         end do
      end do
   end do

contains

subroutine throwDifferentValuesWithLocation( &
&   expected, found, iLocation, tolerance, location )
   use Params_mod
   use StringUtilities_mod
   use Exception_mod
   use ThrowFundamentalTypes_mod, only : locationFormat
   ! , differenceReport, valuesReport
   implicit none
      real(kind=r64) :: expected
      real(kind=r64) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

! scalar case
! in throwDifferentValuesWithLocation  !!!!***CURRENTLY ACTIVE***!!!!
    call throw( &
         & trim(valuesReport(expected, found)) // &
         & '; ' // trim(differenceReport(abs(found - expected), tolerance_)) //  &
!         & '; ' // trim(differenceReport(found - expected, tolerance_)) //  &
         & ';  first difference at element '//trim(locationInArray)//'.', &
         & location = location &
         )    
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_5D_r64_5D_tol64_internal

! end interface assertEqual implementations

      ! Need to reconsider this part -- provide correct type-specific routines rather than
      ! just cast everything to real*8.
      subroutine throwDifferentValuesString64(expected, found, at, location, tolerance)
         real(kind=r64), intent(in) :: expected
         real(kind=r64), intent(in) :: found
         character(len=*), intent(in) :: at
         type (SourceLocation), optional, intent(in) :: location
         real(kind=r64), optional, intent(in) :: tolerance
         real(kind=r64) :: tolerance_

         if(present(tolerance))then
            tolerance_ = tolerance
         else
            tolerance_ = 0.0
         end if

         call throw( &
              & trim(valuesReport(real(expected), real(found))) // &
              & '; ' // trim(differenceReport(real(found - expected), real(tolerance_))) //  &
              & ';  first difference at element <'//trim(at)//'>.', &
              & location = location &
              )
      end subroutine throwDifferentValuesString64

! interface differenceReport implementations

    character(len=MAXLEN_MESSAGE) &
    & function differenceReport_real3232(difference, tolerance) result(differenceReport)
     real(kind=r32), intent(in) :: difference
     real(kind=r32), intent(in) :: tolerance
!     real(kind=r32), optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(real(difference,kind=r32))) // &
      & '| > tolerance:' // trim(toString(tolerance))
    end function 

   
    character(len=MAXLEN_MESSAGE) &
    & function differenceReport_real3264(difference, tolerance) result(differenceReport)
     real(kind=r32), intent(in) :: difference
     real(kind=r64), intent(in) :: tolerance
!     real(kind=r64), optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(real(difference,kind=r32))) // &
      & '| > tolerance:' // trim(toString(tolerance))
    end function 

   
    character(len=MAXLEN_MESSAGE) &
    & function differenceReport_real6432(difference, tolerance) result(differenceReport)
     real(kind=r64), intent(in) :: difference
     real(kind=r32), intent(in) :: tolerance
!     real(kind=r32), optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(real(difference,kind=r32))) // &
      & '| > tolerance:' // trim(toString(tolerance))
    end function 

   
    character(len=MAXLEN_MESSAGE) &
    & function differenceReport_real6464(difference, tolerance) result(differenceReport)
     real(kind=r64), intent(in) :: difference
     real(kind=r64), intent(in) :: tolerance
!     real(kind=r64), optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(real(difference,kind=r32))) // &
      & '| > tolerance:' // trim(toString(tolerance))
    end function 

   
    character(len=MAXLEN_MESSAGE) &
    & function differenceReport_complex3232(difference, tolerance) result(differenceReport)
     complex(kind=r32), intent(in) :: difference
     real(kind=r32), intent(in) :: tolerance
!     real(kind=r32), optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(cmplx(difference,kind=r32))) // &
      & '| > tolerance:' // trim(toString(tolerance))
    end function 

   
    character(len=MAXLEN_MESSAGE) &
    & function differenceReport_complex3264(difference, tolerance) result(differenceReport)
     complex(kind=r32), intent(in) :: difference
     real(kind=r64), intent(in) :: tolerance
!     real(kind=r64), optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(cmplx(difference,kind=r32))) // &
      & '| > tolerance:' // trim(toString(tolerance))
    end function 

   
    character(len=MAXLEN_MESSAGE) &
    & function differenceReport_complex6432(difference, tolerance) result(differenceReport)
     complex(kind=r64), intent(in) :: difference
     real(kind=r32), intent(in) :: tolerance
!     real(kind=r32), optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(cmplx(difference,kind=r32))) // &
      & '| > tolerance:' // trim(toString(tolerance))
    end function 

   
    character(len=MAXLEN_MESSAGE) &
    & function differenceReport_complex6464(difference, tolerance) result(differenceReport)
     complex(kind=r64), intent(in) :: difference
     real(kind=r64), intent(in) :: tolerance
!     real(kind=r64), optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(cmplx(difference,kind=r32))) // &
      & '| > tolerance:' // trim(toString(tolerance))
    end function 

! end interface differenceReport implementations
! interface valuesReport implementations

      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_integerintegerdefaultdefault(expected, found) result(valuesReport)
        integer, intent(in) :: expected
        integer, intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_integerrealdefault32(expected, found) result(valuesReport)
        integer, intent(in) :: expected
        real(kind=r32), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_integerrealdefault64(expected, found) result(valuesReport)
        integer, intent(in) :: expected
        real(kind=r64), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_integercomplexdefault32(expected, found) result(valuesReport)
        integer, intent(in) :: expected
        complex(kind=c32), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(cmplx(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(cmplx(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_integercomplexdefault64(expected, found) result(valuesReport)
        integer, intent(in) :: expected
        complex(kind=c64), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(cmplx(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(cmplx(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_realinteger32default(expected, found) result(valuesReport)
        real(kind=r32), intent(in) :: expected
        integer, intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_realinteger64default(expected, found) result(valuesReport)
        real(kind=r64), intent(in) :: expected
        integer, intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_realreal3232(expected, found) result(valuesReport)
        real(kind=r32), intent(in) :: expected
        real(kind=r32), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_realreal6432(expected, found) result(valuesReport)
        real(kind=r64), intent(in) :: expected
        real(kind=r32), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_realreal3264(expected, found) result(valuesReport)
        real(kind=r32), intent(in) :: expected
        real(kind=r64), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_realreal6464(expected, found) result(valuesReport)
        real(kind=r64), intent(in) :: expected
        real(kind=r64), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_realcomplex3232(expected, found) result(valuesReport)
        real(kind=r32), intent(in) :: expected
        complex(kind=c32), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(cmplx(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(cmplx(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_realcomplex6432(expected, found) result(valuesReport)
        real(kind=r64), intent(in) :: expected
        complex(kind=c32), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(cmplx(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(cmplx(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_realcomplex3264(expected, found) result(valuesReport)
        real(kind=r32), intent(in) :: expected
        complex(kind=c64), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(cmplx(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(cmplx(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_realcomplex6464(expected, found) result(valuesReport)
        real(kind=r64), intent(in) :: expected
        complex(kind=c64), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(cmplx(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(cmplx(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_complexinteger32default(expected, found) result(valuesReport)
        complex(kind=c32), intent(in) :: expected
        integer, intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_complexinteger64default(expected, found) result(valuesReport)
        complex(kind=c64), intent(in) :: expected
        integer, intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_complexreal3232(expected, found) result(valuesReport)
        complex(kind=c32), intent(in) :: expected
        real(kind=r32), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_complexreal6432(expected, found) result(valuesReport)
        complex(kind=c64), intent(in) :: expected
        real(kind=r32), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_complexreal3264(expected, found) result(valuesReport)
        complex(kind=c32), intent(in) :: expected
        real(kind=r64), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_complexreal6464(expected, found) result(valuesReport)
        complex(kind=c64), intent(in) :: expected
        real(kind=r64), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(real(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(real(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_complexcomplex3232(expected, found) result(valuesReport)
        complex(kind=c32), intent(in) :: expected
        complex(kind=c32), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(cmplx(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(cmplx(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_complexcomplex6432(expected, found) result(valuesReport)
        complex(kind=c64), intent(in) :: expected
        complex(kind=c32), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(cmplx(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(cmplx(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_complexcomplex3264(expected, found) result(valuesReport)
        complex(kind=c32), intent(in) :: expected
        complex(kind=c64), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(cmplx(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(cmplx(found,kind=r32))) // ''
      
      end function

   
      character(len=MAXLEN_MESSAGE) &
      & function valuesReport_complexcomplex6464(expected, found) result(valuesReport)
        complex(kind=c64), intent(in) :: expected
        complex(kind=c64), intent(in) :: found

! Note: removed '<.>'
        valuesReport = &
      & 'expected: ' // trim(toString(cmplx(expected,kind=r32))) // &
      & ' but found: ' // trim(toString(cmplx(found,kind=r32))) // ''
      
      end function

! end interface valuesReport implementations
end module AssertReal_mod