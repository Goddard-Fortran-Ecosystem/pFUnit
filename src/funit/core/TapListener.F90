#include "unused_dummy.fh"

module PF_TapListener
   use PF_Exception
   use PF_TestListener
   implicit none
   private

   public :: TapListener

   type, extends(TestListener) :: TapListener
      integer :: unit
      integer :: privateUnit
   contains
      procedure :: addFailure
      procedure :: addError
      procedure :: startTest
      procedure :: disableTest
      procedure :: endTest
      procedure :: endRun
      procedure :: addSuccess
   end type TapListener

   interface TapListener
      module procedure new_TapListener_unit
      module procedure new_TapListener_file
   end interface TapListener


contains

   function new_TapListener_unit(unit) result(listener)
      type (TapListener) :: listener
      integer, intent(in) :: unit

      listener%unit = unit

   end function new_TapListener_unit

   function new_TapListener_file(file) result(listener)
      type (TapListener) :: listener
      character(*), intent(in) :: file

      open(newunit=listener%unit, file=file,status='unknown',form='formatted',access='sequential')

   end function new_TapListener_file

   

   subroutine addFailure(this, testName, exceptions)
      use PF_ExceptionList
      class (TapListener), intent(inOut) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

      type (Exception), pointer :: e

      write(this%unit,*)'not ok - ', testName
      if (exceptions%size() > 0) then
         write(this%unit,*)'  ---'
         e => exceptions%at(1)
         write(this%unit,*)'  message: ', e%message
         write(this%unit,*)'  ...'
      end if
      flush(this%unit)

   end subroutine addFailure

   
   subroutine addError(this, testName, exceptions)
      use PF_ExceptionList
      class (TapListener), intent(inOut) :: this
      character(len=*), intent(in) :: testName
      type (ExceptionList), intent(in) :: exceptions

      ! Experimental:  TAP does not obviously distinguish
      ! failures from errors
      call this%addFailure(testName, exceptions)

   end subroutine addError

   subroutine startTest(this, testName)
      class (TapListener), intent(inOut) :: this
      character(len=*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
   end subroutine startTest

   subroutine disableTest(this, testName)
      class (TapListener), intent(inOut) :: this
      character(len=*), intent(in) :: testName

      write(this%unit,*)'ok - ', testName, ' # skip'
      flush(this%unit)

   end subroutine disableTest

   subroutine endTest(this, testName)
      class (TapListener), intent(inOut) :: this
      character(len=*), intent(in) :: testName

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(testName)
   end subroutine endTest

   subroutine endRun(this, result)
     use PF_AbstractTestResult, only : AbstractTestResult
     class (TapListener), intent(inOut) :: this
     class (AbstractTestResult), intent(in) :: result

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(result)
   end subroutine endRun

   subroutine addSuccess(this, testName)
      class (TapListener), intent(inout) :: this
      character(*), intent(in) :: testName

      write(this%unit,*) 'ok - ', testName
      flush(this%unit)

   end subroutine addSuccess

end module PF_TapListener
