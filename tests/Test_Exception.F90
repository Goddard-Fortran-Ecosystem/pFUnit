#include "reflection.h"
module Test_Exception_mod
   use TestSuite_mod
   use Exception_mod, only: newException
   use Exception_mod, only: newExceptionList
   use Assert_mod, only: assertEqual
   use Assert_mod, only: assertTrue
   use Assert_mod, only: assertFalse
   use Exception_mod, only: ExceptionList, Exception
   use SourceLocation_mod
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite
      use TestSuite_mod, only: newTestSuite
      use TestMethod_mod, only: newTestMethod!, TestMethod

      type (TestSuite) :: suite

      suite = newTestSuite('ExceptionTests')

#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))

      ADD(testGetNumExceptions)
      ADD(testCatchEmpty)
      ADD(testThrow1)
      ADD(testCatchFail)
      ADD(testCatchSucceed)
      ADD(testCatchOnly)
      ADD(testCatchAndRemove)
      ADD(testCatchButPreserveA)
      ADD(testCatchButPreserveB)
      ADD(testCatchButPreserveC)
      ADD(testCatchAnyButPreserveA)
      ADD(testCatchAnyButPreserveB)
      ADD(testCatchAnyButPreserveC)

      ADD(testGetLineNumberNoInfo)
      ADD(testGetLineNumber)
      ADD(testGetFileNameNoInfo)
      ADD(testGetFileName)

      ADD(testThrowWithLineAndFile)

   end function suite

   subroutine testGetNumExceptions()
      type (ExceptionList) :: list

      list = newExceptionList()
      call assertEqual(0, list%getNumExceptions())
      call list%throwMessage('anException')
      call assertEqual(1, list%getNumExceptions())
      call list%throwMessage('anotherException')
      call assertEqual(2, list%getNumExceptions())
      call list%clearAll()
      call assertEqual(0, list%getNumExceptions())

   end subroutine testGetNumExceptions
   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchEmpty
   !
   ! !AUTHORS: Tom Clune and Megan Damon
   !
   ! !DESCRIPTION: 
   ! Test that no exceptions are caught when there aren't any.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   ! !INTERFACE:
   subroutine testCatchEmpty()
      type (ExceptionList) :: list
      type (Exception) :: anException

      !EOP
      !BOC
      list = newExceptionList()
      anException = list%catchAny()
      call assertTrue(anException%isNull())
      !EOC
   end subroutine testCatchEmpty

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testThrow1
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Test that a thrown exception can be caught.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------

   ! !INTERFACE:
   subroutine testThrow1()

      !EOP
      !BOC
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: message = 'anException'

      list = newExceptionList()

      call list%throwMessage(message)
      anException = list%catchAny()
      call assertEqual(message, anException%getMessage())

      !EOC
   end subroutine testThrow1

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchFail
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Test that unintended exceptions are not caught.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------

   ! !INTERFACE:
   subroutine testCatchFail()
      use Exception_mod, only: ExceptionList, Exception
      !EOP
      !BOC
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: message = 'anException'
      list = newExceptionList()

      call list%throwMessage(message)
      anException = list%catchAny()
      call assertFalse(list%catch('different exception'))
      !EOC
   end subroutine testCatchFail

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchSucceed
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Test that only the intended exception is caught.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------

   ! !INTERFACE:
   subroutine testCatchSucceed()
      !EOP
      !BOC
      type (ExceptionList) :: list
      character(len=*), parameter :: message = 'anException'
      list = newExceptionList()

      call list%throwMessage(message)
      call assertTrue(list%catch(message))
      !EOC
   end subroutine testCatchSucceed

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchOnly
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Catch should only succeed for those exceptions which have happened
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------

   ! !INTERFACE:
   subroutine testCatchOnly()
      !EOP
      !BOC
      type (ExceptionList) :: list
      character(len=*), parameter :: message1 = 'first exception'
      character(len=*), parameter :: message2 = 'second exception'

      list = newExceptionList()
      call list%throwMessage(message1)
      call assertFalse(list%catch(message2))!, 'message2 has not been thrown yet')

      call list%throwMessage(message2)
      call assertTrue(list%catch(message1))!, 'message2 is masking message1')
      call assertTrue(list%catch(message2))!, 'message2 was lost')
      !EOC
   end subroutine testCatchOnly

   subroutine testCatchAndRemove()
      !EOP
      !BOC
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: message1 = 'first exception'
      character(len=*), parameter :: message2 = 'second exception'

      list = newExceptionList()

      call list%throwMessage(message1)
      anException = list%catchAny()
      call assertEqual(0, list%getNumExceptions())!, 'catchAny() did not remove only exception')
      call assertFalse(list%catch(message1))

      call list%throwMessage(message1)
      call list%throwMessage(message2)
      call assertTrue(list%catch(message2))
      call assertEqual(1, list%getNumExceptions())!, 'catch() did not reduce list by 1')
      call assertFalse(list%catch(message2))!, 'message2 should have been removed by previous catch')
      call assertEqual(1, list%getNumExceptions())!, 'numExceptions should still be 1')
      call assertTrue(list%catch(message1))
      call assertEqual(0, list%getNumExceptions())!, 'catch() did not reduce list by 1')
      call assertFalse(list%catch(message1))!, 'message1 should have been removed by previous catch')
      call assertEqual(0, list%getNumExceptions())!, 'numExceptions should still be 0')

      !EOC
   end subroutine testCatchAndRemove

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchButPreserveA
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Check that the Preserve variant does not remove exception from the list.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------

   ! !INTERFACE:
   subroutine testCatchButPreserveA()
      !EOP
      !BOC
      type (ExceptionList) :: list
      character(len=*), parameter :: message = 'a message'

      list = newExceptionList()

      call list%throwMessage(message)
      call assertTrue(list%catch(message, preserve=.true.))
      !EOC
   end subroutine testCatchButPreserveA

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchButPreserveB
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Check that the Preserve variant does not remove exception from the list.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------

   ! !INTERFACE:
   subroutine testCatchButPreserveB()
      !EOP
      !BOC
      type (ExceptionList) :: list
      character(len=*), parameter :: message = 'a message'
      logical :: found

      list = newExceptionList()

      call list%throwMessage(message)
      found = list%catch(message, preserve=.true.)
      call assertTrue(list%catch(message)) 
      !EOC
   end subroutine testCatchButPreserveB

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchButPreserveC
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Check that the Preserve variant does not remove exception from the list.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------

   ! !INTERFACE:
   subroutine testCatchButPreserveC()
      !EOP
      !BOC
      type (ExceptionList) :: list
      character(len=*), parameter :: message1 = 'a message'
      character(len=*), parameter :: message2 = 'another message'
      logical :: found

      list = newExceptionList()

      call list%throwMessage(message1)
      call list%throwMessage(message2)
      found = list%catch(message1, preserve=.true.)
      found = list%catch(message2, preserve=.true.)
      call assertEqual(2, list%getNumExceptions())
      !EOC
   end subroutine testCatchButPreserveC

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchAnyButPreserveA
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Check that the Preserve variant does not remove exception from the list.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------

   ! !INTERFACE:
   subroutine testCatchAnyButPreserveA()
      !EOP
      !BOC
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: message = 'a message'

      list = newExceptionList()

      call list%throwMessage(message)
      anException = list%catchAny(preserve = .true.)
      call assertFalse(anException%isNull())
      call assertEqual(message, anException%getMessage())
      !EOC
   end subroutine testCatchAnyButPreserveA

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchAnyButPreserveB
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Check that the Preserve variant does not remove exception from the list.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------

   ! !INTERFACE:
   subroutine testCatchAnyButPreserveB()
      !EOP
      !BOC
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: message = 'a message'

      list = newExceptionList()

      call list%throwMessage(message)
      anException = list%catchAny(preserve=.true.)
      call assertTrue(list%catch(message)) 
      !EOC
   end subroutine testCatchAnyButPreserveB

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchAnyButPreserveC
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Check that the Preserve variant does not remove exception from the list.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------

   ! !INTERFACE:
   subroutine testCatchAnyButPreserveC()
      !EOP
      !BOC
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: message1 = 'a message'
      character(len=*), parameter :: message2 = 'another message'

      list = newExceptionList()

      call list%throwMessage(message1)
      call list%throwMessage(message2)
      anException = list%catchAny(preserve =.true.)
      call assertEqual(2, list%getNumExceptions())
      !EOC
   end subroutine testCatchAnyButPreserveC

   subroutine testGetLineNumberNoInfo()
      use Exception_mod, only: UNKNOWN_LINE_NUMBER
      type (Exception) :: anException

      anException = newException()
      call assertEqual(UNKNOWN_LINE_NUMBER, anException%getLineNumber())

   end subroutine testGetLineNumberNoInfo

   subroutine testGetLineNumber()
      type (Exception) :: anException
      integer, parameter :: LINE_NUMBER = 2

      anException = newException('message', &
           & SourceLocation(fileName='foo', lineNumber=LINE_NUMBER))
      call assertEqual(LINE_NUMBER, anException%getLineNumber())

   end subroutine testGetLineNumber

   subroutine testGetFileNameNoInfo()
      use Exception_mod, only: UNKNOWN_FILE_NAME
      type (Exception) :: anException

      anException = newException()
      call assertEqual(UNKNOWN_FILE_NAME, anException%getFileName())

   end subroutine testGetFileNameNoInfo

   subroutine testGetFileName()
      type (Exception) :: anException
      character(len=*), parameter :: FILE_NAME = 'foo'
      anException = newException('message', &
           & SourceLocation(lineNumber=3, fileName=FILE_NAME))
      call assertEqual(FILE_NAME, anException%getFileName())

   end subroutine testGetFileName

   subroutine testThrowWithLineAndFile()
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: FILE_NAME = 'foo'

      list = newExceptionList()
      call list%throw('message', &
           & SourceLocation(fileName=FILE_NAME, lineNumber=2))
      anException = list%catchAny()
      call assertEqual(FILE_NAME, anException%getFileName())

   end subroutine testThrowWithLineAndFile

end module test_Exception_mod
