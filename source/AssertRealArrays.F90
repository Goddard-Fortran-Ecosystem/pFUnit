module AssertRealArrays_mod

   use Params_mod
   use AssertBasic_mod
   use Exception_mod
   use SourceLocation_mod
   use ThrowFundamentalTypes_mod, only : throwNonConformable


   implicit none
   private


   public :: assertEqual
   public :: vectorNorm
   public :: isWithinTolerance
 
   public :: L_INFINITY_NORM
   public :: L1_NORM
   public :: L2_NORM


   integer, parameter :: L_INFINITY_NORM = 0
   integer, parameter :: L1_NORM         = 1
   integer, parameter :: L2_NORM         = 2


interface vectorNorm

   module procedure vectorNorm_0D
   module procedure vectorNorm_1D
   module procedure vectorNorm_2D
   module procedure vectorNorm_3D
   module procedure vectorNorm_4D
   module procedure vectorNorm_5D

end interface vectorNorm


interface isWithinTolerance

   module procedure isWithinTolerance_0D
   module procedure isWithinTolerance_1D
   module procedure isWithinTolerance_2D
   module procedure isWithinTolerance_3D
   module procedure isWithinTolerance_4D
   module procedure isWithinTolerance_5D

end interface isWithinTolerance


interface assertEqual

   module procedure assertEqual_int_0D_r32_0D_tol32
   module procedure assertEqual_r32_0D_r32_0D_tol32
   module procedure assertEqual_r64_0D_r32_0D_tol64
   module procedure assertEqual_int_0D_r64_0D_tol64
   module procedure assertEqual_r32_0D_r64_0D_tol64
   module procedure assertEqual_r64_0D_r64_0D_tol64
   module procedure assertEqual_int_0D_r32_1D_tol32
   module procedure assertEqual_r32_0D_r32_1D_tol32
   module procedure assertEqual_r64_0D_r32_1D_tol64
   module procedure assertEqual_int_1D_r32_1D_tol32
   module procedure assertEqual_r32_1D_r32_1D_tol32
   module procedure assertEqual_r64_1D_r32_1D_tol64
   module procedure assertEqual_int_0D_r64_1D_tol64
   module procedure assertEqual_r32_0D_r64_1D_tol64
   module procedure assertEqual_r64_0D_r64_1D_tol64
   module procedure assertEqual_int_1D_r64_1D_tol64
   module procedure assertEqual_r32_1D_r64_1D_tol64
   module procedure assertEqual_r64_1D_r64_1D_tol64
   module procedure assertEqual_int_0D_r32_2D_tol32
   module procedure assertEqual_r32_0D_r32_2D_tol32
   module procedure assertEqual_r64_0D_r32_2D_tol64
   module procedure assertEqual_int_2D_r32_2D_tol32
   module procedure assertEqual_r32_2D_r32_2D_tol32
   module procedure assertEqual_r64_2D_r32_2D_tol64
   module procedure assertEqual_int_0D_r64_2D_tol64
   module procedure assertEqual_r32_0D_r64_2D_tol64
   module procedure assertEqual_r64_0D_r64_2D_tol64
   module procedure assertEqual_int_2D_r64_2D_tol64
   module procedure assertEqual_r32_2D_r64_2D_tol64
   module procedure assertEqual_r64_2D_r64_2D_tol64
   module procedure assertEqual_int_0D_r32_3D_tol32
   module procedure assertEqual_r32_0D_r32_3D_tol32
   module procedure assertEqual_r64_0D_r32_3D_tol64
   module procedure assertEqual_int_3D_r32_3D_tol32
   module procedure assertEqual_r32_3D_r32_3D_tol32
   module procedure assertEqual_r64_3D_r32_3D_tol64
   module procedure assertEqual_int_0D_r64_3D_tol64
   module procedure assertEqual_r32_0D_r64_3D_tol64
   module procedure assertEqual_r64_0D_r64_3D_tol64
   module procedure assertEqual_int_3D_r64_3D_tol64
   module procedure assertEqual_r32_3D_r64_3D_tol64
   module procedure assertEqual_r64_3D_r64_3D_tol64
   module procedure assertEqual_int_0D_r32_4D_tol32
   module procedure assertEqual_r32_0D_r32_4D_tol32
   module procedure assertEqual_r64_0D_r32_4D_tol64
   module procedure assertEqual_int_4D_r32_4D_tol32
   module procedure assertEqual_r32_4D_r32_4D_tol32
   module procedure assertEqual_r64_4D_r32_4D_tol64
   module procedure assertEqual_int_0D_r64_4D_tol64
   module procedure assertEqual_r32_0D_r64_4D_tol64
   module procedure assertEqual_r64_0D_r64_4D_tol64
   module procedure assertEqual_int_4D_r64_4D_tol64
   module procedure assertEqual_r32_4D_r64_4D_tol64
   module procedure assertEqual_r64_4D_r64_4D_tol64
   module procedure assertEqual_int_0D_r32_5D_tol32
   module procedure assertEqual_r32_0D_r32_5D_tol32
   module procedure assertEqual_r64_0D_r32_5D_tol64
   module procedure assertEqual_int_5D_r32_5D_tol32
   module procedure assertEqual_r32_5D_r32_5D_tol32
   module procedure assertEqual_r64_5D_r32_5D_tol64
   module procedure assertEqual_int_0D_r64_5D_tol64
   module procedure assertEqual_r32_0D_r64_5D_tol64
   module procedure assertEqual_r64_0D_r64_5D_tol64
   module procedure assertEqual_int_5D_r64_5D_tol64
   module procedure assertEqual_r32_5D_r64_5D_tol64
   module procedure assertEqual_r64_5D_r64_5D_tol64

end interface assertEqual

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
  function vectorNorm_0D(x, norm) result(y)
    real (kind=r64), intent(in) :: x
    integer :: norm
    real (kind=r64) :: y

    y = abs(x) ! independent of norm for rank=0 (scalar) case.

  end function vectorNorm_0D

   
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
  function vectorNorm_1D(x, norm) result(y)
    real (kind=r64), intent(in) :: x(:)
    integer :: norm
    real (kind=r64) :: y

    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
       y = sqrt(sum(x**2))
    end select

  end function vectorNorm_1D

   
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
  function vectorNorm_2D(x, norm) result(y)
    real (kind=r64), intent(in) :: x(:,:)
    integer :: norm
    real (kind=r64) :: y

    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
       y = sqrt(sum(x**2))
    end select

  end function vectorNorm_2D

   
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
  function vectorNorm_3D(x, norm) result(y)
    real (kind=r64), intent(in) :: x(:,:,:)
    integer :: norm
    real (kind=r64) :: y

    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
       y = sqrt(sum(x**2))
    end select

  end function vectorNorm_3D

   
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
  function vectorNorm_4D(x, norm) result(y)
    real (kind=r64), intent(in) :: x(:,:,:,:)
    integer :: norm
    real (kind=r64) :: y

    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
       y = sqrt(sum(x**2))
    end select

  end function vectorNorm_4D

   
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
  function vectorNorm_5D(x, norm) result(y)
    real (kind=r64), intent(in) :: x(:,:,:,:,:)
    integer :: norm
    real (kind=r64) :: y

    select case (norm)  ! code to support rank /= 0 cases.
    case (L_INFINITY_NORM)
       y = maxval(abs(x))
    case (L1_NORM)
       y = sum(abs(x))
    case (L2_NORM)
       y = sqrt(sum(x**2))
    end select

  end function vectorNorm_5D

! end interface vectorNorm implementations
! interface isWithinTolerance implementations

   logical function isWithinTolerance_0D(x, tolerance, norm)
     real (kind=r64), intent(in) :: x
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_0D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_0D

   
   logical function isWithinTolerance_1D(x, tolerance, norm)
     real (kind=r64), intent(in) :: x(:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_1D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_1D

   
   logical function isWithinTolerance_2D(x, tolerance, norm)
     real (kind=r64), intent(in) :: x(:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_2D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_2D

   
   logical function isWithinTolerance_3D(x, tolerance, norm)
     real (kind=r64), intent(in) :: x(:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_3D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_3D

   
   logical function isWithinTolerance_4D(x, tolerance, norm)
     real (kind=r64), intent(in) :: x(:,:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_4D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_4D

   
   logical function isWithinTolerance_5D(x, tolerance, norm)
     real (kind=r64), intent(in) :: x(:,:,:,:,:)
     real (kind=r64), intent(in) :: tolerance
     integer,         intent(in) :: norm

     isWithinTolerance_5D = ( vectorNorm(x, norm) <= tolerance )

   end function isWithinTolerance_5D

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
     real(kind=r32), intent(in) :: found
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta
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
         tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta
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
         tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_0D_r32_0D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r32), intent(in) :: found
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_0D_r32_0D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r32_0D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(0)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
         tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r32_0D_tol64_internal

   
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
     real(kind=r64), intent(in) :: found
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta
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
         tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta
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
         tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta
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
         tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_0D_r32_1D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_0D_r32_1D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r32_1D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found(:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r32_1D_tol64_internal

   
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
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected(:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected(:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_1D_r32_1D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:)
     real(kind=r32), intent(in) :: found(:)
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_1D_r32_1D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_1D_r32_1D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found(:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected(:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(1)
     integer :: foundShape(1)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_1D_r32_1D_tol64_internal

   
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
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected(:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected(:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected(:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
            tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_0D_r32_2D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_0D_r32_2D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r32_2D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r32_2D_tol64_internal

   
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
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected(:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected(:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_2D_r32_2D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:)
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_2D_r32_2D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_2D_r32_2D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found(:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected(:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(2)
     integer :: foundShape(2)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_2D_r32_2D_tol64_internal

   
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
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected(:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected(:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected(:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
               tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_0D_r32_3D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_0D_r32_3D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r32_3D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r32_3D_tol64_internal

   
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
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected(:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected(:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_3D_r32_3D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_3D_r32_3D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_3D_r32_3D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found(:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected(:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(3)
     integer :: foundShape(3)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_3D_r32_3D_tol64_internal

   
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
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected(:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected(:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected(:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                  tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_0D_r32_4D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_0D_r32_4D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r32_4D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r32_4D_tol64_internal

   
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
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected(:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected(:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_4D_r32_4D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_4D_r32_4D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_4D_r32_4D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found(:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected(:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(4)
     integer :: foundShape(4)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_4D_r32_4D_tol64_internal

   
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
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected(:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected(:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected(:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                     tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_0D_r32_5D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_0D_r32_5D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_0D_r32_5D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(0)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_0D_r32_5D_tol64_internal

   
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
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected(:,:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r32), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected(:,:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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

   subroutine assertEqual_r64_5D_r32_5D_tol64( &
   &  expected, found, message, tolerance, location )
     implicit none
     real(kind=r64), intent(in) :: expected(:,:,:,:,:)
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), optional, intent(in) :: tolerance
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

     call assertEqual_r64_5D_r32_5D_tol64_internal ( &
     &  expected, found, tolerance_, message_, location_ )
     
   end subroutine

   subroutine assertEqual_r64_5D_r32_5D_tol64_internal( &
   &  expected, found, tolerance, message, location )
     implicit none
     real(kind=r32), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected(:,:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
     integer :: i, i1, i2
     integer :: expectedSize
     ! maybe use (:) in the following...
     integer :: expectedShape(5)
     integer :: foundShape(5)
     ! expected and found combinations are [0,icomb] by [1,2,3,4,5,...].
     integer :: idx1,idx2,idx3,idx4,idx5 ! for iter.

! Scalar "temp" variables
      real(kind=r64) :: expected0
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
   implicit none
   real(kind=r64) :: expected
   real(kind=r32) :: found

   real(kind=r64), intent(in) :: tolerance
   type (SourceLocation), intent(in) :: location
   integer, intent(in) :: iLocation(:)
   integer :: iLocationSize
   integer, parameter :: MAXLEN_SHAPE = 80
   character(len=MAXLEN_SHAPE) :: locationInArray
   write(locationInArray,locationFormat(iLocation)) iLocation

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_5D_r32_5D_tol64_internal

   
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
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     integer, intent(in) :: expected(:,:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r32), intent(in) :: expected(:,:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
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
     real(kind=r64), intent(in) :: found(:,:,:,:,:)
     real(kind=r64), intent(in) :: tolerance
     real(kind=kind(tolerance)) :: tolerance_
     real(kind=r64), intent(in) :: expected(:,:,:,:,:)

     character(len=*), intent(in) :: message  ! not used yet!
     type (SourceLocation), intent(in) :: location

     real(kind=kind(tolerance_)) :: ONE=1
     real(kind=kind(tolerance_)), parameter :: DEFAULT_TOLERANCE = tiny(ONE)
     logical :: conformable
  
     integer :: first
     real(kind=r64) :: delta(size(found,1),size(found,2),size(found,3),size(found,4),size(found,5))
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
   !
      if(size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found), location=location)
         ! Test failed... So return?
         return
      end if
   
      ! Check shapes
      expectedSize = size(expected); expectedShape = shape(expected)
   !   foundShape = shape(found)
      do i = 1, size(expectedShape)
         if( expectedShape(i) /= foundShape(i) ) then
            call throwNonConformable(expectedShape, foundShape, location=location)
            return ! bail
         end if
      end do

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
                        tolerance_ = 0.0
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
   use AssertReal_mod, only : differenceReport, valuesReport
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

    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(locationInArray) // '>' // &
         & trim(valuesReport(real(expected), real(found))) // &
         & trim(differenceReport(real(found - expected), real(tolerance))), &
         & location=location &
         & )
         
end subroutine throwDifferentValuesWithLocation

end subroutine assertEqual_r64_5D_r64_5D_tol64_internal

! end interface assertEqual implementations
end module AssertRealArrays_mod