!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: AbstractPrinter
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Halvor Lund, SINTEF Energy Research
!!
!! @date
!! 05 Feb 2014
!!
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
!-------------------------------------------------------------------------------
module AbstractPrinter_mod
   use TestListener_mod
   implicit none
   private

   public :: AbstractPrinter, PrinterPointer

   type, abstract, extends(TestListener) :: AbstractPrinter
   contains
     procedure(addFailure), deferred :: addFailure
     procedure(startTest), deferred :: startTest
     procedure(endTest), deferred :: endTest
     procedure(print), deferred :: print
     procedure :: addError
   end type AbstractPrinter

   type PrinterPointer
     class (AbstractPrinter), pointer :: pPrinter
   end type PrinterPointer

   abstract interface
      subroutine addFailure(this, testName, exceptions)
         use Exception_mod
         import AbstractPrinter
         class (AbstractPrinter), intent(inOut) :: this
         character(len=*), intent(in) :: testName
         type (Exception), intent(in) :: exceptions(:)
      end subroutine addFailure

      subroutine startTest(this, testName)
         import AbstractPrinter
         class (AbstractPrinter), intent(inOut) :: this
         character(len=*), intent(in) :: testName
      end subroutine startTest

      subroutine endTest(this, testName)
         import AbstractPrinter
         class (AbstractPrinter), intent(inOut) :: this
         character(len=*), intent(in) :: testName
      end subroutine endTest

      subroutine print(this, result, runTime)
         use TestResult_mod
         import AbstractPrinter
         class (AbstractPrinter), intent(in) :: this
         type (TestResult), intent(in) :: result
         real, intent(in) :: runTime
      end subroutine print

   end interface

contains

   ! Most scenarios in Fortran cannot diagnose true errors, so
   ! an empty stub is provided here for convenience.
   subroutine addError(this, testName, exceptions)
      use Exception_mod, only: Exception
      class (AbstractPrinter), intent(inout) :: this
      character(len=*), intent(in) :: testName
      type (Exception), intent(in) :: exceptions(:)
   end subroutine addError

end module AbstractPrinter_mod
