
module ThrowFundamentalTypes_mod

  use Params_mod
  use StringUtilities_mod
  use Exception_mod
  use AssertReal_mod, only : differenceReport, valuesReport
  use SourceLocation_mod

  implicit none
  private

  public :: locationFormat
  public :: throwNonConformable
  public :: throwDifferentValues
  public :: throwDifferentValuesWithLocation

!mlr-!!! Can we put these in one place?
!mlr-   integer, parameter :: MAX_LEN_MSG   = 1000
!mlr-   integer, parameter :: MAX_LEN_FLOAT = 25
!mlr-   integer, parameter :: MAX_LEN_INT   = 15
!mlr-  
!mlr-   integer, parameter :: L_INFINITY_NORM = 0
!mlr-   integer, parameter :: L1_NORM         = 1
!mlr-   integer, parameter :: L2_NORM         = 2

!  interface locationFormat
!     module procedure locationFormat
!  end interface locationFormat

  interface throwDifferentValues
     module procedure throwDifferentValues_ii
     module procedure throwDifferentValues_ir
     module procedure throwDifferentValues_rr
  end interface throwDifferentValues

  interface throwDifferentValuesWithLocation
     module procedure throwDifferentValuesWithLocation_ii
     module procedure throwDifferentValuesWithLocation_ir
     module procedure throwDifferentValuesWithLocation_rr
  end interface throwDifferentValuesWithLocation

contains

  ! Consider promoting to module level scope.
  subroutine throwNonConformable(shapeExpected, shapeFound, sourceLoc)
    integer, intent(in) :: shapeExpected(:)
    integer, intent(in) :: shapeFound(:)
    type (SourceLocation), optional, intent(in) :: sourceLoc

    call throw( &
         & 'Assertion failed: non-conformable real arrays.' // new_line('$') //&
         & '    expected shape: <['//trim(toString(shapeExpected))//']>' // new_line('$') //&
         & '   but found shape: <['//trim(toString(shapeFound))//']>', &
         & location=sourceLoc &
         & )
  end subroutine throwNonConformable

  subroutine compareElements(expected, found, i1, i2, sourceLoc)
    real, intent(in) :: expected, found
    integer, intent(in) :: i1, i2
    type (SourceLocation), optional, intent(in) :: sourceLoc

    ! the test
    if (expected /= found) then
       call throwDifferentValues(expected, found, i1, i2, 0.0, sourceLoc=sourceLoc)
    end if
  end subroutine compareElements

  subroutine throwDifferentValues_ii(iExpected, iFound, i1, i2, tolerance, sourceLoc)
    integer, intent(in) :: iExpected, iFound
    integer, intent(in) :: i1, i2
    real, intent(in) :: tolerance
    type (SourceLocation), optional, intent(in) :: sourceLoc

    ! Check with team to see if this is okay.
    call throwDifferentValues_rr(real(iExpected), real(iFound), i1, i2, tolerance, &
         & sourceLoc=sourceLoc)

  end subroutine throwDifferentValues_ii

  subroutine throwDifferentValues_ir(iExpected, found, i1, i2, tolerance, sourceLoc)
    integer, intent(in) :: iExpected
    real, intent(in) :: found
    integer, intent(in) :: i1, i2
    real, intent(in) :: tolerance
    type (SourceLocation), optional, intent(in) :: sourceLoc

    ! Check with team to see if this is okay.
    call throwDifferentValues_rr(real(iExpected), found, i1, i2, tolerance, &
         & sourceLoc=sourceLoc)

  end subroutine throwDifferentValues_ir

  subroutine throwDifferentValues_rr(expected, found, i1, i2, tolerance, &
       & sourceLoc )
    real, intent(in) :: expected, found
    integer, intent(in) :: i1, i2
    real, intent(in) :: tolerance
    type (SourceLocation), optional, intent(in) :: sourceLoc

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80

    ! "location" is not used in the original AssertEqual code.
    character(len=MAXLEN_SHAPE) :: location
    write(location,'("[",i0,", ",i0," ]")') i1, i2

    call throw( &
         & 'Assertion failed:  unequal real 2D arrays.' // new_line('$') // &
         & '  First difference at element <' // location // '>' // &
         & trim(valuesReport(expected, found)) // &
         & trim(differenceReport(found - expected, tolerance)), &
         & location=sourceLoc &
!         & trim(differenceReport(found - expected, 0.)) &
         & )

  end subroutine throwDifferentValues_rr

  subroutine throwDifferentValuesWithLocation_ii( &
       & iExpected, iFound, iLocation, tolerance, sourceLoc )
    integer, intent(in) :: iExpected, iFound
    integer, intent(in) :: iLocation(:)
    real, intent(in) :: tolerance
    type (SourceLocation), optional, intent(in) :: sourceLoc

    ! Check with team to see if this is okay.
    call throwDifferentValuesWithLocation_rr( &
         & real(iExpected), real(iFound), iLocation, tolerance, sourceLoc=sourceLoc )

  end subroutine throwDifferentValuesWithLocation_ii

  subroutine throwDifferentValuesWithLocation_ir( &
       & iExpected, found, iLocation, tolerance, sourceLoc)
    integer, intent(in) :: iExpected
    real, intent(in) :: found
    integer, intent(in) :: iLocation(:)
    real, intent(in) :: tolerance
    type (SourceLocation), optional, intent(in) :: sourceLoc

!    print *,'20000'
    ! Check with team to see if this is okay. ! Answer: meh...
    call throwDifferentValuesWithLocation_rr( &
         & real(iExpected), found, iLocation, tolerance, sourceLoc=sourceLoc)

  end subroutine throwDifferentValuesWithLocation_ir

  function locationFormat(iLocation) result (fmt)
    integer, intent(in) :: iLocation(:)

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80*2

    character(len=MAXLEN_SHAPE) :: fmt
    integer :: iLocationSize

    iLocationSize = size(iLocation)
    
    if (iLocationSize .eq. 1) then
       fmt = '("[" i0 "]")'
    else
       write(fmt,*) '("["',iLocationSize-1,'(i0,", ") i0 "]")'
    end if

  end function locationFormat

  subroutine throwDifferentValuesWithLocation_rr( &
       & expected, found, iLocation, tolerance, sourceLoc)
    real, intent(in) :: expected, found
    integer, intent(in) :: iLocation(:)
    integer :: iLocationSize
    real, intent(in) :: tolerance
    type (SourceLocation), optional, intent(in) :: sourceLoc

    !mlr maybe move this to a larger scope...
    integer, parameter :: MAXLEN_SHAPE = 80*2

    ! "location" is not used in the original AssertEqual code.
    character(len=MAXLEN_SHAPE) :: location

    write(location, locationFormat(iLocation)) iLocation

!    print *, &
!         & 'Assertion failed: unequal arrays.' // NEWLINE // &
!         & '  First difference at element <' // trim(location) // '>' // &
!         & trim(valuesReport(expected, found)) // &
!         & trim(differenceReport(found - expected, tolerance))


    call throw( &
         & 'Assertion failed: unequal arrays.' // new_line('$') // &
         & '  First difference at element <' // trim(location) // '>' // &
         & trim(valuesReport(expected, found)) // &
         & trim(differenceReport(found - expected, tolerance)), &
         & location=sourceLoc &
         & )

  end subroutine throwDifferentValuesWithLocation_rr

end module ThrowFundamentalTypes_mod
