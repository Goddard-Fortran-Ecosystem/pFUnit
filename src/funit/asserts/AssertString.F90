module pf_AssertString
   use pf_SourceLocation
   use pf_StringUtilities
   use pf_Exception
   implicit none
   private

   public :: assertEqual

   interface assertEqual
      module procedure assertEqual_string
      module procedure assertEqual_string_0d1d
      module procedure assertEqual_string_1d1d
   end interface assertEqual


contains
   
   
   subroutine assertEqual_string(expected, found, message, location, &
        & whitespace)
      use PF_ExceptionList, only: throw

      character(len=*), intent(in) :: expected
      character(len=*), intent(in) :: found
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      type (WhitespaceOptions), optional, intent(in) :: &
           & whitespace

      character(len=:), allocatable :: message_
      type (WhitespaceOptions) :: whitespace_

      character(len=:), allocatable :: throwMessage
      integer :: i, j
      integer :: numI, numJ
      integer :: numSameCharacters

      integer, parameter :: iachar_spc = 32, iachar_tab = 9

      logical :: checkCharacterByCharacter
      logical :: throwException
      logical :: whitespaceYes
      character(len=:), allocatable :: expected_, found_

      throwException = .false.

      message_ = NULL_MESSAGE
      if (present(message)) message_ = message

      if(present(whitespace))then
         whitespace_ = whitespace
         if (.not. any(whitespace_ == [TRIM_ALL, IGNORE_ALL, IGNORE_DIFFERENCES, KEEP_ALL])) then
            throwMessage = & 
                 & 'assertEqualString_InternalError: ' &
                 & // 'Unknown case for handling Whitespace'
            call throw(appendWithSpace(message_,throwMessage), location)
            return
      end if
         
      else
         ! This is the default whitespace option.  TRIM_ALL is the legacy behavior.
         ! TODO:  Change default behavior to IGNORE_DIFFERENCES.
         whitespace_ = TRIM_ALL
      end if

      if (whitespace_ == KEEP_ALL) then
         expected_ = expected
         found_    = found
      else
         expected_ = trimAll(expected)
         found_    = trimAll(found)
      end if

      ! Determine if we need to iterate through the characters in the strings.
      ! Trim: ignore leading & trailing white space.  
      ! Ignore: ignore all white space.
      ! Keep: white space is significant.
      ! Worry:  Original code written to !print out trimmed strings.  Not sure what effect
      ! Keep will have.
      checkCharacterByCharacter = expected_ /= found_
      numI = len(expected_); numJ = len(found_)

      ! Flag a check if zero-length arrays are involved.
      if ((numI .eq. 0) .or. (numJ .eq. 0)) then
         checkCharacterByCharacter = .true.
      end if
      
      ! Fortran implicitly pads strings of different lengths with spaces
      ! when comparing using /= or ==.  Detect them and compare carefully.
      if (numI .ne. numJ) then
         checkCharacterByCharacter = .true.
      end if
      
      if (checkCharacterByCharacter) then
         numSameCharacters = 0

         ! Cycle over both strings, compare each element, skipping if needed.
         i = 1; j = 1
         countNumSameCharacters: do

            ! Is a string traversal complete?
            if ( i .gt. numI .or. j .gt. numJ ) then
               ! If both made it to end, exit ok, else continue other traverse.
               if ( i .gt. numI .and. j .gt. numJ ) exit
            end if

            ! Handle whitespace options.
            whitespaceYes = .false.
            if ( i .le. numI ) whitespaceYes = whitespacep(expected_(i:i)) 
            if ( j .le. numJ ) whitespaceYes = whitespaceYes .or. &
                & whitespacep(found_(j:j)) 

            if ( whitespaceYes ) then

               select case (whitespace_%value)

                  ! IGNORE_ALL?  Then skip that element.  Skip on i first, then j.
               case (IGNORE_ALL%value)
                  if( i .le. numI ) then
                     if(whitespacep(expected_(i:i)))then
                        i=i+1; cycle
                     end if
                  end if
                  if( j .le. numJ ) then
                     if(whitespacep(found_(j:j)))then
                        j=j+1; cycle
                     end if
                  end if

                  ! IGNORE_DIFFERENCES?
                  ! If either i & j start sequences that are white, skip past.
               case (IGNORE_DIFFERENCES%value)

                  !print *,2001

                  ! Because we expect to be dealing with trimmed strings
                  ! at this point, we need both sequences to be
                  ! whitespace, else fail.

                  if(  &
                       & .not.( &
                       &       whitespacep(expected_(i:i)) &
                       &       .and.whitespacep(found_(j:j))) ) then
                     throwException = .true.; exit
                  end if

                  !print *,2100

                  ! Skip past i's whitespace.
                  iWhitespace: if( i .le. numI ) then
                     iLoop: do
                        ! Found white char, skip.
                        if(whitespacep(expected_(i:i)))then
                           i=i+1; if (i .gt. numI) exit iLoop
                        else
                           exit iLoop
                        end if
                     end do iLoop
                     ! i now either indexes non-whitespace or is past its bound.
                  end if iWhitespace

                  ! Skip past j's whitespace.
                  jWhitespace: if( j .le. numJ ) then
                     jLoop: do
                        if(whitespacep(found_(j:j)))then
                           ! Found white char, skip.
                           j=j+1; if (j .gt. numJ) exit jLoop
                        else
                           exit jLoop
                        end if
                     end do jLoop
                     ! j now either indexes non-whitespace or is past its bound.
                  end if jWhitespace

                  ! If both finish at the same time, i,j .gt. numI, numJ.
                  ! should be an error condition.  Remember, we're
                  ! dealing with trimmed sequences.
                  !
                  !if ( i .gt. numI .and. j .gt. numJ ) then
                  !   ...cycle loop...
                  !end if

               end select

            end if

            ! Fail if a traverse is complete.
            if ( i .gt. numI .or. j .gt. numJ ) then
               throwException = .true. ; exit
            end if

            ! A character is not white space!

            ! Both characters are not whitespace:  fail if unequal.
            if (expected_(i:i) /= found_(j:j)) then
               throwException = .true. ; exit
            end if

            ! Consume both of the equal characters.
            i=i+1; j=j+1; numSameCharacters = numSameCharacters + 1

         end do countNumSameCharacters

         if (throwException) then
            if (whitespace_ == KEEP_ALL) then
               expected_ = expected
               found_    = found
            else
               expected_ = trimTrailingWhitespace(expected)
               found_    = trimTrailingWhitespace(found)
            end if

            throwMessage = &
                 & 'String assertion failed:' // new_line('A') // &
                 & '    expected: <"' // expected_ // '">' // new_line('A') // &
                 & '   but found: <"' // found_ // '">' // new_line('A') // &
                 & '  first diff:   ' // repeat('-', numSameCharacters) // '^'

            call throw(appendWithSpace(message_, throwMessage), location)

         end if

      ! else ! if checkCharacterByCharacter == .false. and we don't have to compare character-by-character

      end if

   end subroutine assertEqual_string


   subroutine assertEqual_string_0d1d(expected, found, message, location, &
        & whitespace)

      character(len=*), intent(in) :: expected
      character(len=*), intent(in) :: found(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      type (WhitespaceOptions), optional, intent(in) :: whitespace

      integer :: i

      do i = 1, size(found)
         call assertEqual(expected, found(i), message=message, location=location, whitespace=whitespace)
      end do

   end subroutine assertEqual_string_0d1d

   subroutine assertEqual_string_1d1d(expected, found, message, location, &
        whitespace)
      use PF_AssertUtilities

      character(len=*), intent(in) :: expected(:)
      character(len=*), intent(in) :: found(:)
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location
      type (WhitespaceOptions), optional, intent(in) :: whitespace

      integer :: i

      if (.not. size(expected) == size(found)) then
         call fail_not_conformable(shape(expected), shape(found), message=message, location=location)
         return
      end if
      
      do i = 1, min(size(found),size(expected))
         call assertEqual(expected(i), found(i), message=message, location=location, whitespace=whitespace)
      end do

   end subroutine assertEqual_string_1d1d

end module pf_AssertString
