module test_AssertInteger_mod
  use pFUnitException_mod
  use AssertInteger_mod
  use AssertString_mod
  implicit none
  private

  public :: test_assertEqual_scalar
  public :: test_assertEqual_scalar_message
  public :: test_arrayString
  public :: test_assertEqual_vector
  public :: test_assertEqual_scalar_vector

contains

  subroutine test_assertEqual_scalar()
    character(len=1000) :: expectedMessage

    call assertEqual(0, 0) ! should not raise exception

    call assertEqual(0, 1) ! should raise the appropriateException
    expectedMessage = 'Integer scalar assertion failed:' // NEW_LINE('a')
    call append(expectedMessage, '       Expected:  0' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: 1')

    call expect(expectedMessage)
    
  end subroutine test_assertEqual_scalar

  subroutine test_assertEqual_scalar_message()
    character(len=1000) :: expectedMessage

    call assertEqual(0, 0, message = 'equal') ! should not raise exception

    call assertEqual(0, 1, message = 'unequal') ! should raise the appropriateException
    expectedMessage = 'Integer scalar assertion failed: unequal' // NEW_LINE('a')
    call append(expectedMessage, '       Expected:  0' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: 1')

    call expect(expectedMessage)
    
  end subroutine test_assertEqual_scalar_message

  subroutine test_arrayString()

    call assertEqual('(/ 1 /)', arrayString( (/ 1 /) ))
    call assertEqual('(/ 12 /)', arrayString( (/ 12 /) ))

    call assertEqual('(/ 1, 2 /)', arrayString( (/ 1,2 /) ))
    call assertEqual('(/ 12, 23, 456, 1 /)', arrayString( (/ 12,23,456,1 /) ))

  end subroutine test_arrayString

  subroutine test_assertequal_vector()
    character(len=1000) :: expectedMessage
    
    integer :: A(2)
    integer :: B(2)
    integer :: C(2)
    integer :: D(3)

    A = (/ 1, 2 /)
    B = (/ 1, 3 /)
    C = (/ 4, 5 /)
    D = (/ 1, 2, 4 /)

    call assertEqual(A, A, message = 'case A == A')
    call assertEqual(B, B, message = 'case B == B')
    call assertEqual(C, C, message = 'case C == C')
    call assertEqual(D, D, message = 'case D == D')

    call assertEqual(A, B, message = 'case A /= B')
    expectedMessage = 'Integer vector assertion failed: case A /= B' // NEW_LINE('a')
    call append(expectedMessage, '       First difference at element 2.' // NEW_LINE('a'))
    call append(expectedMessage, '       Expected:  (/ 1, 2 /)' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: (/ 1, 3 /)')

    call expect(expectedMessage)
    
    call assertEqual(A, C, message = 'case A /= C')
    expectedMessage = 'Integer vector assertion failed: case A /= C' // NEW_LINE('a')
    call append(expectedMessage, '       First difference at element 1.' // NEW_LINE('a'))
    call append(expectedMessage, '       Expected:  (/ 1, 2 /)' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: (/ 4, 5 /)')

    call expect(expectedMessage)

    call assertEqual(A, D, message = 'case A /= D')
    expectedMessage = 'Integer vector assertion failed: case A /= D' // NEW_LINE('a')
    call append(expectedMessage, '       Arrays of different length.' // NEW_LINE('a'))
    call append(expectedMessage, '       First difference at element 3.' // NEW_LINE('a'))
    call append(expectedMessage, '       Expected:  (/ 1, 2 /)' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: (/ 1, 2, 4 /)')

    call expect(expectedMessage)

  end subroutine test_assertequal_vector

  subroutine test_assertequal_scalar_vector
    character(len=1000) :: expectedMessage
    
    integer :: A(2)
    integer :: B(2)
    integer :: C(2)

    A = (/ 1, 1 /)
    B = (/ 1, 2 /)
    C = (/ 3, 4 /)

    call assertEqual(1, A, message = 'case 1 == A')

    call assertEqual(1, B, message = 'case 1 /= B')
    expectedMessage = 'Integer vector assertion failed: case 1 /= B' // NEW_LINE('a')
    call append(expectedMessage, '       First difference at element 2.' // NEW_LINE('a'))
    call append(expectedMessage, '       Expected:  (/ 1, 1 /)' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: (/ 1, 2 /)')

    call expect(expectedMessage)
    
    call assertEqual(1, C, message = 'case 1 /= C')
    expectedMessage = 'Integer vector assertion failed: case 1 /= C' // NEW_LINE('a')
    call append(expectedMessage, '       First difference at element 1.' // NEW_LINE('a'))
    call append(expectedMessage, '       Expected:  (/ 1, 1 /)' // NEW_LINE('a'))
    call append(expectedMessage, '       but found: (/ 3, 4 /)')

    call expect(expectedMessage)

  end subroutine test_assertequal_scalar_vector

  subroutine expect(message)
     character(len=*) :: message
     type (Exception_type) :: anException
     
     anException = catchAny()
     if (anException == NO_EXCEPTION) then
        call throw(Exception('Failed to raise exception for strings of unequal length.'))
     else
        if (trim(message) /= trim(getMessage(anException))) then
           call throw(Exception('Incorrect message when comparing strings with different messages.'))
        end if
     end if
  end subroutine expect

  subroutine append(string, suffix)
    character(len=*), intent(inout) :: string
    character(len=*), intent(in)    :: suffix

    string = trim(string) // trim(suffix)

  end subroutine append

end module test_AssertInteger_mod
