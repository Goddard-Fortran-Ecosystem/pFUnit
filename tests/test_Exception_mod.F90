!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!BOP
! !MODULE: test_Exception
!
! !AUTHORS:      Tom Clune
! !AFFILIATION:  NASA SIVO
! !DATE:         29 Aug 2006
!
! !DESCRIPTION: 
!
! This module tests Exception_mod.
!
! !REVISION HISTORY:
! None
!-------------------------------------------------------------------------------

! !INTERFACE:  
module test_Exception_mod
   use pfunit
   use pFUnitException_mod
   implicit none
   private
   
   public :: test_catchEmpty
   public :: test_throw1
   public :: test_catchFail
   public :: test_catchSucceed
   public :: test_catchOnly
   public :: test_catchRethrow
   public :: test_clearAll
   public :: test_catchOnce
   public :: test_catchGetException
   public :: test_ignoreOtherBlock
   public :: test_nestedTry
   public :: test_macros
   public :: test_getMessage
   public :: test_ignoreSameMessage


#ifdef USE_MPI
   public :: test_gatherExceptionsA
   public :: test_gatherExceptionsB
#endif

contains

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_catchEmpty
   !
   ! !AUTHORS: Megan Damon 
   !
   ! !DESCRIPTION: 
   ! Test that no exceptions are caught 
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   ! !INTERFACE:
   subroutine test_catchEmpty()
      implicit none
      type (Exception_type) :: anException

   !EOP
   !BOC
      anException = catchAny () 
      call assertEqual ("none", getMessage (anException)) 
      call clearAll()
   !EOC
   end subroutine test_catchEmpty
   
   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_throw1
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
   subroutine test_throw1()
      implicit none

   !EOP
   !BOC
      type (Exception_type) :: anException

      call assertFalse(catch())

      anException = Exception('exception thrown')
      call throw(anException)
      call assertTrue(catch())

      call clearAll()
   !EOC
   end subroutine test_throw1

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_catchFail
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
   subroutine test_catchFail()

   !EOP
   !BOC
      call throw(Exception('exception thrown'))
      call assertFalse(catch(Exception('a different exception')))

      call clearAll()
   !EOC
   end subroutine test_catchFail

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_catchSucceed
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
   subroutine test_catchSucceed()
      type (Exception_type) :: anException
      
   !EOP
   !BOC
      anException = Exception('exception thrown')
      call throw(anException)
      call assertTrue(catch(anException))

      call clearAll()
   !EOC
   end subroutine test_catchSucceed

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_catchOnly
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
   subroutine test_catchOnly()
      type (Exception_type) :: exception_1
      type (Exception_type) :: exception_2
   !EOP
   !BOC
      exception_1 = Exception('exception 1')
      exception_2 = Exception('exception 2')
      call throw(exception_1)
      call assertFalse(catch(exception_2),'exception 2 has not been thrown yet')

      call throw(exception_2)
      call assertTrue(catch(exception_1),'exception 1 should be found')
      call assertTrue(catch(exception_2),'exception 2 should be found')

      call clearAll()
   !EOC
   end subroutine test_catchOnly

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_catchRethrow
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Check that the rethrow option keeps the exception in the list.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_catchRethrow()
      type (Exception_type) :: exceptionA, exceptionB
   !EOP
   !BOC
      exceptionA = Exception('exception 1')
      exceptionB = Exception('exception 2')

      call throw(exceptionA)
      call throw(exceptionB)

      if (catch(exceptionA, rethrow = .true.)) then
         call assertTrue(catch(exceptionA),'exceptionA should be found again')
      else
         call throw(Exception('exceptionA was not even found the first time?'))
      end if

      if (catch(exceptionB, rethrow = .false.)) then
         call assertFalse(catch(exceptionB),'exceptionB should be found again')
      else
         call throw(Exception('exceptionB was not even found the first time?'))
      end if

      call clearAll()
   !EOC
   end subroutine test_catchRethrow

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_clearAll
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Verify that clearAll() removes all exceptions.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_clearAll()
      type (Exception_type) :: exceptionA, exceptionB
   !EOP
   !BOC
      exceptionA = Exception('exception 1')
      exceptionB = Exception('exception 2')

      call throw(exceptionA)
      call throw(exceptionB)
      
      call clearAll()

      call assertFalse(catch(),'should not be any exceptions now')
      call assertFalse(catch(exceptionA),'exceptionA should not be found')
      call assertFalse(catch(exceptionB),'exceptionB should not be found')

      call clearAll()
   !EOC
   end subroutine test_clearAll

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_catchOnce
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! A complex use case.  Verify that a given exception is only caught once.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_catchOnce()
      type (Exception_type) :: exception_1
      type (Exception_type) :: exception_3
      type (Exception_type) :: exception_4
      type (Exception_type) :: exception_5
   !EOP
   !BOC
      exception_1 = Exception('exception 1')
      exception_3 = Exception('exception 3')
      exception_4 = Exception('exception 4')
      exception_5 = Exception('exception 5')

      call throw(exception_1)
      call assertTrue(catch(exception_1),'exception 1 should be found')
      call assertFalse(catch(exception_1),'exception 1 should not be found a second time')

      call assertFalse(catch(),'no exceptions should be left at this point')
      call throw(exception_3)
      call throw(exception_4)
      call throw(exception_5)

      call assertTrue(catch(exception_4))
      call assertFalse(catch(exception_4),'exception 4 should not be found a second time')

      call assertTrue(catch(),'should be 2 exceptions left')
      call assertTrue(catch(),'should be 1 exception left')
      call assertFalse(catch(),'should be no exceptions left')

      call clearAll()
   !EOC
   end subroutine test_catchOnce

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_catchGetException
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Test that returned exception is among those that are active.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_catchGetException()
      type (Exception_type) :: exception1
      type (Exception_type) :: exception2
      type (Exception_type) :: foundException
   !EOP
   !BOC
      exception1 = Exception('exception 1')
      call throw(exception1)
      exception2 = Exception('exception 2')
      call throw(exception2)

      foundException = catchAny()
      call assertTrue( foundException == exception1 .or. foundException == exception2 )
      if (foundException == exception1) then
         call assertTrue(catchAny() == exception2)
      else
         call assertTrue(catchAny() == exception1)
      end if
      call assertFalse(catch(),'none left')
      call assertTrue( catchAny() == NO_EXCEPTION )

      call clearAll()
   !EOC
   end subroutine test_catchGetException

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_ignoreOtherBlock
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Test that only exceptions within current try block are found.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_ignoreOtherBlock()
      type (Exception_type) :: exception1
      type (Exception_type) :: exception2
   !EOP
   !BOC

      exception1 = Exception('exception 1')
      exception2 = Exception('exception 2')

      call try()
      call throw(exception1)
      call endTry()

      call try()
      call throw(exception2)
      call assertFalse(catch(exception1),'not within this try block')
      call endTry()

      call clearAll()
   !EOC
   end subroutine test_ignoreOtherBlock

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_macros
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Use macros to manage try/catch blocks.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_macros()
      type (Exception_type) :: exception1
      type (Exception_type) :: exception2
   !EOP
   !BOC
      exception1 = Exception('exception 1')
      exception2 = Exception('exception 2')

!#include "TryCatch.h"
!!$
!!$      TRY
!!$        call doSomething1()
!!$      CATCH(exception1)
!!$        call handleException1(1)
!!$      OR_CATCH(exception2)
!!$        call handleException2(1)
!!$      END_TRY
!!$
!!$      TRY
!!$        call doSomething2()
!!$      CATCH(exception1)
!!$        call handleException1(2)
!!$      OR_CATCH(exception2)
!!$        call handleException2(2)
!!$      END_TRY
!!$
      call clearAll()
!!$   !EOC
!!$   contains
!!$
!!$      ! First THROW invokes return - masking second exception
!!$      subroutine doSomething1()
!!$         THROW(exception1); return
!!$         THROW(exception2); return
!!$      end subroutine doSomething1
!!$
!!$      subroutine doSomething2()
!!$         THROW(exception2); return
!!$         THROW(exception1); return
!!$      end subroutine doSomething2
!!$
!!$      subroutine handleException1(n)
!!$         implicit none
!!$         integer, intent(in) :: n
!!$         call assertEqual(1, n)
!!$      end subroutine handleException1
!!$
!!$      subroutine handleException2(n)
!!$         implicit none
!!$         integer, intent(in) :: n
!!$         call assertEqual(2, n)
!!$      end subroutine handleException2

   end subroutine test_macros

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_nestedTry
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Check for proper behavior with nested Try blocks using macros.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_nestedTry()
      type (Exception_type) :: exception1
      type (Exception_type) :: exception2
   !EOP
   !BOC
!!$      exception1 = Exception('exception 1')
!!$      exception2 = Exception('exception 2')
!!$
!!$      TRY
!!$          THROW(Exception1)
!!$
!!$        TRY
!!$          THROW(exception2)
!!$        CATCH(exception1)
!!$          call assertFalse(1 == 1,'this line should not be executed')
!!$        END_TRY
!!$
!!$        TRY
!!$        CATCH(exception2)
!!$          call assertFalse(1 == 1,'this line should not be executed')
!!$        END_TRY
!!$
!!$        if (catch(exception2)) then
!!$           call assertTrue(1 == 1,'this line should be executed')
!!$        elseif (catch(exception1)) then
!!$           call assertTrue(1 == 1,'this line should be executed')
!!$        end if
!!$
!!$      CATCH(exception2)
!!$          call assertTrue(1 == 1,'this line should be executed')
!!$      OR_CATCH(exception1)
!!$          call assertTrue(1 == 1,'this line should be executed')
!!$      END_TRY
!!$
!!$      call clearAll()
   !EOC                  
   end subroutine test_nestedTry

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_getMessage
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Verify that getMessage returns the correct information from an exception.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_getMessage()
      character(len=*), parameter :: myMessage = 'exception Message'
      integer, parameter :: MAX_LEN=80
      character(len=MAX_LEN) :: foundMessage
   !EOP
   !BOC
      foundMessage = getMessage(Exception(myMessage))
      call assertEqual(myMessage, foundMessage)
   !EOC
   end subroutine test_getMessage

   !---------------------------------------------------------------------------
   !BOP
   ! !IROUTINE: test_ignoreSameMessage
   !
   ! !AUTHORS: Tom Clune
   !
   ! !DESCRIPTION: 
   ! Verify that two Exceptions with coincidentally same message are not treated as
   ! equal.  Prevents name space collisions, but may confuse some users.
   !
   ! !REVISION HISTORY:
   ! None
   !---------------------------------------------------------------------------
   
   ! !INTERFACE:
   subroutine test_ignoreSameMessage()

      character(len=*), parameter :: sameMessage = 'same Message'
      type (Exception_type) :: exception_A
      type (Exception_type) :: exception_B
   !EOP
   !BOC
      exception_A = Exception(sameMessage)
      exception_B = Exception(sameMessage)

      call assertFalse(exception_A == exception_B)
   !EOC
   end subroutine test_ignoreSameMessage

#ifdef USE_MPI
#include "reflection.h"

   subroutine test_gatherExceptionsA()
      type (MpiTestCase_type) :: test
      type (TestResult_type) :: result
      
      integer, parameter  :: NPES = 1
      integer             :: p
      integer :: unit, stat
      
      character(len=MAX_LEN_MSG) :: msg

      test = MpiTestCase(REFLECT(checkGather), numProcesses=NPES)
      
      result = newTestResult()
      call Run(test, TestInfo(), result) ! should launch an MPI job and fill test result
      
      if (amRoot()) then
      call assertEqual(1, numRun(result), 'numRun.')
         call assertEqual(0, numFailed(result),' numFailed.')
      end if

      call clean(result)
      call clean(test)
      
   end subroutine test_gatherExceptionsA
   
   subroutine test_gatherExceptionsB()
      type (MpiTestCase_type) :: test
      type (TestResult_type) :: result
      
      integer, parameter  :: NPES = 2
      integer             :: p
      integer :: unit, stat
      
      character(len=MAX_LEN_MSG) :: msg

!!$      test = MpiTestCase(REFLECT(checkGather), numProcesses=NPES)
!!$      
!!$      result = newTestResult()
!!$      call Run(test, TestInfo(), result) ! should launch an MPI job and fill test result
!!$      
!!$      call assertEqual(1, numRun(result), 'numRun.')
!!$      call assertEqual(0, numFailed(result),' numFailed.')
!!$      
!!$      call clean(result)
!!$      call clean(test)
      
   end subroutine test_gatherExceptionsB

   subroutine checkGather(info)
      use pFunit
      use MpiServices_mod
      type (TestInfo_type), intent(in) :: info
      
      integer :: rank, npes, ier
      integer :: p
      character(len=80) :: msg
      type (Exception_type) :: foundException

!!$         if (catch(preserve=.true.)) then
!!$            print*,'uh oh 0: ',trim(getMessage(foundException))
!!$         end if
!!$      write(msg,'("Exception raised on process ",i3.3,".")') processRank(info)
!!$      call throw(Exception(trim(msg)))
!!$
!!$      call gatherExceptions(mpiCommunicator(info))
!!$      if (amRoot(info)) then
!!$
!!$      npes = numProcesses(info)
!!$         do p = 0, npes - 1
!!$            write(msg,'("(pe:",i5.1,") Exception raised on process ",i3.3,".")') p, p
!!$            if (.not. catch(trim(msg))) then
!!$               print*,__LINE__,__FILE__
!!$               call throw(Exception("Failed to find msg: <" // trim(msg) // ">"))
!!$            end if
!!$         end do
!!$
!!$         if (catch(preserve=.true.)) then
!!$            print*,'uh oh A: ',trim(getMessage(foundException))
!!$            call throw(Exception("Ugathered messages remain on some processes."))
!!$            foundException = catchAny()
!!$            if (.not. (foundException  == NO_EXCEPTION)) then
!!$               print*,'uh oh B: ',trim(getMessage(foundException))
!!$            end if
!!$         end if
!!$
!!$      end if

   end subroutine checkGather
#endif

end module test_Exception_mod


