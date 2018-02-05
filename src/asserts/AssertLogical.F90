   subroutine assertEqualLogical_(expected, found, message, location)
      use PF_ExceptionList_mod, only: throw
      logical, intent(in) :: expected
      logical, intent(in) :: found
      character(len=*), optional, intent(in) :: message
      type (SourceLocation), optional, intent(in) :: location

      character(len=:), allocatable :: throwMessage
      character(len=:), allocatable :: message_

      character(len=:), allocatable :: str_expected, str_found

      if (expected) then
         str_expected = 'TRUE'
      else
         str_expected = 'FALSE'
      end if

      if (found) then
         str_found = 'TRUE'
      else
         str_found = 'FALSE'
      end if
      
      if (expected .neqv. found) then
         throwMessage = &
              & 'Logical assertion failed:' // new_line('A') // &
              & '    expected: <"' // str_expected // '">' // new_line('A')// &
              & '   but found: <"' // str_found // '">' // new_line('A')

         message_ = NULL_MESSAGE
         if (present(message)) message_ = message

         call throw(appendWithSpace(message_,throwMessage), location)
      end if

   contains

      pure function to_string(flag) result(string)
         character(len=:), allocatable :: string
         logical, intent(in) :: flag

         if (flag) then
            string = 'TRUE'
         else
            string = 'FALSE'
         end if
      end function to_string
         
      
   end subroutine assertEqualLogical_

