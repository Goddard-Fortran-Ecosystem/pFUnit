#include "reflection.h"
module test_AssertString_mod
  use pFUnitException_mod
  use AssertString_mod, only: assertEqual, trimWhiteSpace

  public :: testAssertEqual_unequalLength
  public :: testAssertEqual_different
  public :: test_trimWhiteSpace
  public :: testAssertEqual_ignoreWhiteSpace

contains

  subroutine testAssertEqual_unequalLength()
    character(len=1000) :: expectedMessage

    call assertEqual(' ', 'a')
    expectedMessage = 'String assertion failed:' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Strings of unequal length.' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Differences begin at position 1.' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Expected:  ""' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       but found: "a"'
    call expect(expectedMessage)

    call assertEqual(' ', 'a', message='test message')
    expectedMessage = 'String assertion failed: test message' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Strings of unequal length.' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Differences begin at position 1.' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Expected:  ""' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       but found: "a"'
    call expect(expectedMessage)

    call assertEqual('ac', 'abc', message='test message')
    expectedMessage = 'String assertion failed: test message' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Strings of unequal length.' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Differences begin at position 2.' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Expected:  "ac"' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       but found: "abc"'
    call expect(expectedMessage)

  end subroutine testAssertEqual_unequalLength

  subroutine testAssertEqual_different
    character(len=1000) :: expectedMessage

    call assertEqual('a', 'b')
    expectedMessage = 'String assertion failed:' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Differences begin at position 1.' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       Expected:  "a"' // NEW_LINE('a')
    expectedMessage = trim(expectedMessage) // '       but found: "b"'

    call expect(expectedMessage)

  end subroutine testAssertEqual_different

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
  
  subroutine testAssertEqual_ignoreWhiteSpace
    character(len=1000) :: expectedMessage

    call assertEqual('a c', 'a  c', ignoreWhiteSpace = .true.)
    call assertEqual(' a c', 'a c', ignoreWhiteSpace = .true.)
    call assertEqual('1 2 3', '1 2  3', ignoreWhiteSpace = .true.)

  end subroutine testAssertEqual_ignoreWhiteSpace

  subroutine test_trimWhiteSpace()

    call assertEqual('a', trimWhiteSpace(' a'), message = 'Problem with leading spaces.')
    call assertEqual('a', trimWhiteSpace('  a'), message = 'Problem with leading spaces.')
    call assertEqual('a b', trimWhiteSpace('a   b'), message = 'Problem with middle spaces.')

  end subroutine test_trimWhiteSpace

end module test_AssertString_mod
