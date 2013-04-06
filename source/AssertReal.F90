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

   interface assertEqual
      module procedure assertEqualExact
      module procedure assertEqualExact_withMessage
      module procedure assertEqualTolerance
      module procedure assertEqualTolerance_withMessage
      module procedure assertEqual_1D1D
   end interface

contains

   subroutine assertEqualExact(expected, found, unused, file, line)
      real, intent(in) :: expected
      real, intent(in) :: found
      type (UnusableArgument), optional, intent(in) :: unused
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: line

      call assertEqual(expected, found, tolerance=0., message=NULL_MESSAGE, file=file, line=line)

   end subroutine assertEqualExact

   subroutine assertEqualExact_withMessage(expected, found, message, line, file)
      real, intent(in) :: expected
      real, intent(in) :: found
      character(len=*), intent(in) :: message
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: line

      call assertEqual(expected, found, tolerance=0., message=message, file=file, line=line)

   end subroutine assertEqualExact_withMessage

   subroutine assertEqualTolerance(expected, found, tolerance, unused, file, line)
      real, intent(in) :: expected
      real, intent(in) :: found
      real, intent(in) :: tolerance
      type (UnusableArgument), optional, intent(in) :: unused
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: line

      call assertEqual(expected, found, tolerance, NULL_MESSAGE, file=file, line=line)
   end subroutine assertEqualTolerance

   subroutine assertEqualTolerance_withMessage(expected, found, tolerance, message, file, line)
      real, intent(in) :: expected
      real, intent(in) :: found
      real, intent(in) :: tolerance
      character(len=*), intent(in) :: message
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: line

      character(len=MAXLEN_MESSAGE) :: throwMessage

      if (abs(found-expected) > tolerance) then

         throwMessage = trim(valuesReport(expected, found)) // new_line('$') // &
              & trim(differenceReport(found - expected, tolerance))

         call throw(appendWithSpace(message, throwMessage), SourceLocation(file, line))

      end if

   end subroutine assertEqualTolerance_withMessage

   subroutine assertEqual_1D1D(expected, found, line, file)
      real, intent(in) :: expected(:)
      real, intent(in) :: found(:)
      integer, optional, intent(in) :: line
      character(len=*), optional, intent(in) :: file

      integer :: i
      integer, parameter :: MAXLEN_SHAPE = 80


      call assertSameShape(shape(expected), shape(found))
      if (anyExceptions()) return
      do i = 1, size(expected)
         call compareElements(expected(i), found(i), i)
      end do

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

      valuesReport = 'expected: <' // trim(toString(expected)) // '> but found: <' // trim(toString(found)) // '>'
   end function valuesReport

   character(len=MAXLEN_MESSAGE) function differenceReport(difference, tolerance)
      real, intent(in) :: difference
      real, intent(in) :: tolerance
      differenceReport = '    difference: |' // trim(toString(difference)) // '| > tolerance:' // trim(toString(tolerance))
   end function differenceReport

end module AssertReal_mod
