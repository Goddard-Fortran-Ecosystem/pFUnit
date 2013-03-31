module AssertReal_mod
   use StringUtilities
   use Exception_mod
   implicit none
   private

   public :: assertEqual

   ! exposed for testing purposes only
   public :: valuesReport
   public :: differenceReport
   public :: shapeReport

   interface assertEqual
      module procedure assertEqualExact
      module procedure assertEqualTolerance
      module procedure assertEqual_1D1D
   end interface

contains

   subroutine assertEqualExact(expected, found, line, file)
      real, intent(in) :: expected
      real, intent(in) :: found
      integer, optional, intent(in) :: line
      character(len=*), optional, intent(in) :: file

      call assertEqual(expected, found, tolerance=0., line=line, file=file)

   end subroutine assertEqualExact

   subroutine assertEqualTolerance(expected, found, tolerance, line, file)
      real, intent(in) :: expected
      real, intent(in) :: found
      real, intent(in) :: tolerance
      integer, optional, intent(in) :: line
      character(len=*), optional, intent(in) :: file

      if (abs(found-expected) > tolerance) then
         call throw( &
              & 'Assertion failed: unequal real values.' //  &
              & trim(valuesReport(expected, found)) // &
              & trim(differenceReport(found - expected, tolerance)) &
              & )
      end if
   end subroutine assertEqualTolerance

   character(len=MAXLEN_MESSAGE) function shapeReport(expectedShape, foundShape)
      integer, intent(in) :: expectedShape(:)
      integer, intent(in) :: foundShape(:)

      shapeReport = new_line('$') // &
           & '    expected shape: <['//trim(toString(expectedShape))//']>' // new_line('$') // &
           & '   but found shape: <['//trim(toString(foundShape))//']>'

   end function shapeReport

   subroutine assertEqual_1D1D(expected, found, line, file)
      real, intent(in) :: expected(:)
      real, intent(in) :: found(:)
      integer, optional, intent(in) :: line
      character(len=*), optional, intent(in) :: file

      integer :: i
      integer, parameter :: MAXLEN_SHAPE = 80


      if (size(expected) /= size(found)) then
         call throwNonConformable(shape(expected), shape(found))
      else
         do i = 1, size(expected)
            call compareElements(expected(i), found(i), i)
         end do
      end if

   contains

      subroutine throwNonConformable(shapeExpected, shapeFound)
         integer, intent(in) :: shapeExpected(:)
         integer, intent(in) :: shapeFound(:)

         call throw( &
              & 'Assertion failed: non-conformbable real arrays.' // new_line('$') //& 
              & '    expected shape: <['//trim(toString(shapeExpected))//']>' // new_line('$') //&
              & '   but found shape: <['//trim(toString(shapeFound))//']>' &
              & )
      end subroutine throwNonConformable
      
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
      valuesReport = &
           & new_line('$') // '    expected: <' // trim(toString(expected)) // '>' // &
           & new_line('$') // '   but found: <' // trim(toString(found)) // '>'
   end function valuesReport

   character(len=MAXLEN_MESSAGE) function differenceReport(difference, tolerance)
      real, intent(in) :: difference
      real, intent(in) :: tolerance
      differenceReport = &
           & new_line('$') // '  difference: |' // trim(toString(difference)) // '| > ' // trim(toString(tolerance))
   end function differenceReport

end module AssertReal_mod
