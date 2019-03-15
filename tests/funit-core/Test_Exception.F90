!#include "reflection.h"
!-------------------------------------------------------------------------------
! NASA/GSFC, Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: Test_Exception
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC
!!
!! @date
!! 20 Mar 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 20 Mar 2015 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------

module Test_Exception
   use PF_TestSuite
   use PF_ExceptionList
   use PF_Exception, only: Exception
   use PF_Assert, only: assertEqual
   use PF_Assert, only: assertTrue
   use PF_Assert, only: assertFalse
   use PF_SourceLocation
   implicit none
   private

   public :: suite

contains

   function suite()
      use PF_TestSuite, only: TestSuite
      use PF_TestMethod, only: TestMethod

      type (TestSuite) :: suite

      suite = TestSuite('ExceptionTests')

!#define ADD(method) call suite%addTest(TestMethod(REFLECT(method)))

      call suite%addTest( &
           &   TestMethod('testGetNumExceptions', &
           &                  testGetNumExceptions))
      call suite%addTest( &
           &   TestMethod('testCatchNextEmpty', &
           &                  testCatchNextEmpty))
      call suite%addTest( &
           &   TestMethod('testThrow1', &
           &                  testThrow1))
      call suite%addTest( &
           &   TestMethod('testCatchFail', &
           &                  testCatchFail))
      call suite%addTest( &
           &   TestMethod('testCatchSucceed', &
           &                  testCatchSucceed))
      call suite%addTest( &
           &   TestMethod('testCatchOnly', &
           &                  testCatchOnly))
      call suite%addTest( &
           &   TestMethod('testCatchAndRemove', &
           &                  testCatchAndRemove))
      call suite%addTest( &
           &   TestMethod('testCatchButPreserveA', &
           &                  testCatchButPreserveA))
      call suite%addTest( &
           &   TestMethod('testCatchButPreserveB', &
           &                  testCatchButPreserveB))
      call suite%addTest( &
           &   TestMethod('testCatchButPreserveC', &
           &                  testCatchButPreserveC))
      call suite%addTest( &
           &   TestMethod('testCatchNextButPreserveA', &
           &                  testCatchNextButPreserveA))
      call suite%addTest( &
           &   TestMethod('testCatchNextButPreserveB', &
           &                  testCatchNextButPreserveB))
      call suite%addTest( &
           &   TestMethod('testCatchNextButPreserveC', &
           &                  testCatchNextButPreserveC))

      call suite%addTest( &
           &   TestMethod('testGetLineNumberNoInfo', &
           &                  testGetLineNumberNoInfo))
      call suite%addTest( &
           &   TestMethod('testGetLineNumber', &
           &                  testGetLineNumber))
      call suite%addTest( &
           &   TestMethod('testGetFileNameNoInfo', &
           &                  testGetFileNameNoInfo))
      call suite%addTest( &
           &   TestMethod('testGetFileName', &
           &                  testGetFileName))

      call suite%addTest( &
           &   TestMethod('testThrowWithLineAndFile', &
           &                  testThrowWithLineAndFile))

   end function suite

   subroutine testGetNumExceptions()
      type (ExceptionList) :: list

      call assertEqual(0, list%get_num_exceptions())
      call list%throw_message('anException')
      call assertEqual(1, list%get_num_exceptions())
      call list%throw_message('anotherException')
      call assertEqual(2, list%get_num_exceptions())
      call list%clear()
      call assertEqual(0, list%get_num_exceptions())

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
   subroutine testCatchNextEmpty()
      type (ExceptionList) :: list
      type (Exception) :: anException

      !EOP
      !BOC
      anException = list%catch_next()
      call assertTrue(anException%isNull())
      !EOC
   end subroutine testCatchNextEmpty

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

      call list%throw_message(message)
      anException = list%catch_next()
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
      use PF_Exception, only: Exception
      use PF_ExceptionList, only: ExceptionList
      !EOP
      !BOC
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: message = 'anException'

      call list%throw_message(message)
      anException = list%catch_next()
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

      call list%throw_message(message)
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

      call list%throw_message(message1)
      call assertFalse(list%catch(message2))!, 'message2 has not been thrown yet')

      call list%throw_message(message2)
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

      call list%throw_message(message1)
      anException = list%catch_next()
      call assertEqual(0, list%get_num_exceptions())!, 'catchNext() did not remove only exception')
      call assertFalse(list%catch(message1))

      call list%throw_message(message1)
      call list%throw_message(message2)
      call assertTrue(list%catch(message2))
      call assertEqual(1, list%get_num_exceptions())!, 'catch() did not reduce list by 1')
      call assertFalse(list%catch(message2))!, 'message2 should have been removed by previous catch')
      call assertEqual(1, list%get_num_exceptions())!, 'numExceptions should still be 1')
      call assertTrue(list%catch(message1))
      call assertEqual(0, list%get_num_exceptions())!, 'catch() did not reduce list by 1')
      call assertFalse(list%catch(message1))!, 'message1 should have been removed by previous catch')
      call assertEqual(0, list%get_num_exceptions())!, 'numExceptions should still be 0')

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

      call list%throw_message(message)
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

      call list%throw_message(message)
      found = list%catch(message, preserve=.true.)
      call assertTrue(found)
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

      call list%throw_message(message1)
      call list%throw_message(message2)
      found = list%catch(message1, preserve=.true.)
      call assertTrue(found)
      found = list%catch(message2, preserve=.true.)
      call assertTrue(found)
      call assertEqual(2, list%get_num_exceptions())
      !EOC
   end subroutine testCatchButPreserveC

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchNextButPreserveA
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
   subroutine testCatchNextButPreserveA()
      !EOP
      !BOC
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: message = 'a message'

      call list%throw_message(message)
      anException = list%catch_next(preserve = .true.)
      call assertFalse(anException%isNull())
      call assertEqual(message, anException%getMessage())
      !EOC
   end subroutine testCatchNextButPreserveA

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: testCatchNextButPreserveB
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
   subroutine testCatchNextButPreserveB()
      !EOP
      !BOC
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: message = 'a message'

      call list%throw_message(message)
      anException = list%catch_next(preserve=.true.)
      call assertTrue(list%catch(message)) 
      !EOC
   end subroutine testCatchNextButPreserveB

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
   subroutine testCatchNextButPreserveC()
      !EOP
      !BOC
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: message1 = 'a message'
      character(len=*), parameter :: message2 = 'another message'

      call list%throw_message(message1)
      call list%throw_message(message2)
      anException = list%catch_next(preserve =.true.)
      call assertEqual(2, list%get_num_exceptions())
      !EOC
   end subroutine testCatchNextButPreserveC

   subroutine testGetLineNumberNoInfo()
      use PF_Exception, only: UNKNOWN_LINE_NUMBER
      type (Exception) :: anException

      anException = Exception()
      call assertEqual(UNKNOWN_LINE_NUMBER, anException%getLineNumber())

   end subroutine testGetLineNumberNoInfo

   subroutine testGetLineNumber()
      type (Exception) :: anException
      integer, parameter :: LINE_NUMBER = 2

      anException = Exception('message', &
           & SourceLocation(fileName='foo', lineNumber=LINE_NUMBER))
      call assertEqual(LINE_NUMBER, anException%getLineNumber())

   end subroutine testGetLineNumber

   subroutine testGetFileNameNoInfo()
      use PF_Exception, only: UNKNOWN_FILE_NAME
      type (Exception) :: anException

      anException = Exception()
      call assertEqual(UNKNOWN_FILE_NAME, anException%getFileName())

   end subroutine testGetFileNameNoInfo

   subroutine testGetFileName()
      type (Exception) :: anException
      character(len=*), parameter :: FILE_NAME = 'foo'
      anException = Exception('message', &
           & SourceLocation(lineNumber=3, fileName=FILE_NAME))
      call assertEqual(FILE_NAME, anException%getFileName())

   end subroutine testGetFileName

   subroutine testThrowWithLineAndFile()
      type (ExceptionList) :: list
      type (Exception) :: anException
      character(len=*), parameter :: FILE_NAME = 'foo'

      call list%throw('message', &
           & SourceLocation(fileName=FILE_NAME, lineNumber=2))
      anException = list%catch_next()
      call assertEqual(FILE_NAME, anException%getFileName())

   end subroutine testThrowWithLineAndFile

end module test_Exception
