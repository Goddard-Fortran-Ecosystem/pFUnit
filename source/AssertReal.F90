module AssertReal_mod
   use AssertBasic_mod
   use StringUtilities_mod
   use SourceLocation_mod
   use Exception_mod
   implicit none
   private

   public :: assertEqual

   ! exposed for testing purposes only
   public :: valuesReport
   public :: differenceReport
   public :: shapeReport

   interface assertEqual
      module procedure assertEqualTolerance_
      module procedure assertEqual_1D1D
   end interface

contains

   subroutine assertEqualTolerance_(expected, found, tolerance, message, location)
      real, intent(in) :: expected
      real, intent(in) :: found
      real, intent(in), optional :: tolerance
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=MAXLEN_MESSAGE) :: throwMessage

      real :: tolerance_
      character(len=:), allocatable :: message_
      type (SourceLocation) :: location_

      if(present(tolerance))then
         tolerance_ = tolerance
      else 
         tolerance_ = 0.
      end if

      if(present(message))then
         message_ = message
      else
         message_ = NULL_MESSAGE
      end if

      if(present(location))then
         location_ = location
      else
         location_ = UNKNOWN_SOURCE_LOCATION
      end if


      if (abs(found-expected) > tolerance_) then

         throwMessage = trim(valuesReport(expected, found)) // new_line('$') // &
              & trim(differenceReport(found - expected, tolerance_))

         call throw(appendWithSpace(message_, throwMessage), location_)

      end if

    end subroutine assertEqualTolerance_

   subroutine assertEqual_1D1D(expected, found, location)
      real, intent(in) :: expected(:)
      real, intent(in) :: found(:)
      type (SourceLocation), optional, intent(in) :: location
      type (SourceLocation) :: location_

      integer :: i
      integer, parameter :: MAXLEN_SHAPE = 80

      call assertSameShape(shape(expected), shape(found))
      if (anyExceptions()) return
      do i = 1, size(expected)
         call compareElements(expected(i), found(i), i)
      end do

   contains

!      subroutine throwNonConformable(shapeExpected, shapeFound)
!         integer, intent(in) :: shapeExpected(:)
!         integer, intent(in) :: shapeFound(:)
!
!         call throw( &
!              & 'Assertion failed: non-conformbable real arrays.' // new_line('$') //& 
!              & '    expected shape: <['//trim(toString(shapeExpected))//']>' // new_line('$') //&
!              & '   but found shape: <['//trim(toString(shapeFound))//']>' &
!              & )
!      end subroutine throwNonConformable
      
      subroutine compareElements(expected, found, at)
         real, intent(in) :: expected
         real, intent(in) :: found
         integer, intent(in) :: at
         
         if (expected /= found) then
            call throwDifferentValues(expected, found, at)
         end if

      end subroutine compareElements

      subroutine throwDifferentValues(expected, found, at)
         real, intent(in) :: expected
         real, intent(in) :: found
         integer, intent(in) :: at

         character(len=MAXLEN_SHAPE) :: location
         write(location,'("[",i0,"]")') at

         call throw( &
              & 'Assertion failed: unequal real 1D arrays.' // new_line('$') // & 
              & '  First difference at element <['//trim(toString(at))//']>' // &
              & trim(valuesReport(expected, found)) // &
              & trim(differenceReport(found - expected, 0.))  &
              )
      end subroutine throwDifferentValues
      
   end subroutine assertEqual_1D1D

   character(len=MAXLEN_MESSAGE) function valuesReport(expected, found)
      real, intent(in) :: expected
      real, intent(in) :: found

      valuesReport = 'expected: <' // trim(toString(expected)) // '> but found: <' // trim(toString(found)) // '>'
   end function valuesReport

   character(len=MAXLEN_MESSAGE) function differenceReport(difference, tolerance)
      real, intent(in) :: difference
      real, optional, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(difference)) // '| > tolerance:' // trim(toString(tolerance))
   end function differenceReport

   character(len=MAXLEN_MESSAGE) FUNCTION shapeReport(expectedShape, foundShape)
     integer, intent(in) :: expectedShape(:)
     integer, intent(in) :: foundShape(:)

     shapeReport = new_line('$') // &
          & '    expected shape: <['//trim(toString(expectedShape))//']>' // new_line('$') // &
          & '   but found shape: <['//trim(toString(foundShape))//']>'

   end function shapeReport

end module AssertReal_mod
