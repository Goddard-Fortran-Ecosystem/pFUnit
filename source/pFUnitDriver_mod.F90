!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: pFUnitDriver_mod
!
!> @brief
!! Implements the driver with shared-object libraries for running tests.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 19 Mar 2007 
!!
! REVISION HISTORY:
! 19 Mar 2007 - Initial Version
! 14 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module pFUnitDriver_mod
   implicit none
   private

   public :: loadDsoTests
   public :: runDsoTests

   interface loadDsoTests
      module procedure loadTestsAll
      module procedure loadTestsSpec
   end interface

contains

   !---------------------------------------------------------------------------
   !> Loads all tests supporting the shared-object libraries.
   !!
   !! @param aSuite - given test suite 
   !! @param dsoHandle - DSO Handler 
   !! @param testNames - given test names
   !---------------------------------------------------------------------------
   subroutine loadTestsAll(aSuite, dsoHandle, testNames)
      use TestSuite_mod
      use SharedObjLibUtilities_mod
      use FortranNameMangle_mod
      use ProcedurePointer_mod
      use TestCase_mod
      type (TestSuite_type) :: aSuite
      type (SharedObjLibUtilities_type) :: dsoHandle
      character(len=*), intent(in) :: testNames(:)

      character (len=MAX_SYMBOL_LENGTH), pointer :: libNames(:)
      character (len=MAX_SYMBOL_LENGTH) :: procName
      type (ProcedurePointer_type) :: procPointer
      type (TestCase_type) :: tcase
      integer :: i, j

      libNames => externalNames(dsoHandle)

      do i = 1, size(libNames)
         procName = procedureName(libNames(i))
         do j = 1, size(testNames)

            if (match(procName, testNames(j))) then
               procPointer = getProcedureHandle(dsoHandle, libNames(i))
               tcase = TestCase()
               call addTestMethodFromAddress(tcase, procname, address(procPointer))
               call add(aSuite, tcase)
            end if

         end do
      end do

      deallocate(libNames)

   contains

      !------------------------------------------------------------------------
      !> Returns the result of matching between two input strings.
      !!
      !! @param string1 - given expected string to be matched
      !! @param string2 - given found string for matched string
      !------------------------------------------------------------------------
      logical function match(string1, string2)
         character(len=*), intent(in) :: string1
         character(len=*), intent(in) :: string2

         match = (index(trim(string1), trim(string2)) == 1)

      end function match

   end subroutine loadTestsAll

   !---------------------------------------------------------------------------
   !> Loads all specific tests supporting the shared-object libraries.
   !!
   !! @param aSuite - given test suite 
   !! @param dsoHandle - DSO Handler 
   !! @param testList - given list of tests
   !---------------------------------------------------------------------------
   subroutine loadTestsSpec(aSuite, dsoHandle, testList)
      use TestCase_mod
      use TestSuite_mod
      use pFUnitException_mod
      use ProcedurePointer_mod
      use SharedObjLibUtilities_mod
      use IO_Utilities_mod
      type (TestSuite_type) :: aSuite
      type (SharedObjLibUtilities_type) :: dsoHandle
      character(len=*), intent(in) :: testList

      integer :: unit
      integer :: ioStatus
      character(len=MAX_SYMBOL_LENGTH) :: testName
      type (TestCase_type) :: tcase
      type (ProcedurePointer_type) :: procPointer
      
      if (catch(preserve=.true.)) return ! load failed

      unit = openFile(testList, status='old', form='formatted')
      if (catch(preserve=.true.)) return ! open failed

      do ! loop over test names
         read(unit,*, iostat = ioStatus) testName
         if (ioStatus /= 0) exit
         procPointer = getProcedureHandle(dsoHandle, testName, mangle=.true.)
         tCase = TestCase()
         call addTestMethodFromAddress(tcase, testName, address(procPointer))
         call add(aSuite, tCase)
      end do

   end subroutine loadTestsSpec

   !---------------------------------------------------------------------------
   !> Performs to run tests with shared-object libraries.
   !!
   !! @param libraryName - given library name 
   !! @param pattern - given pattern for match
   !! @param testResult - test result object
   !---------------------------------------------------------------------------
   recursive subroutine runDsoTests(libraryName, pattern, testResult)
      use TestResult_mod
      use TestSuite_mod
      use pFUnitException_mod
      use IO_Utilities_mod
      use SharedObjLibUtilities_mod

      character(len=*), intent(in) :: libraryName
      character(len=*), intent(in) :: pattern
      type (TestResult_type) :: testResult

      type (TestSuite_type) :: suite
      type (SharedObjLibUtilities_type) :: handle

      if (catch(preserve=.true.)) return

      handle = openLibrary(trim(libraryName))
      if (.not. catch(preserve=.true.)) then
         suite = TestSuite('top')
         call loadDsoTests(suite, handle, (/ pattern /))
         call run(suite, testResult)
         call closeLibrary(handle)
         call clean(suite)
      end if

    end subroutine runDsoTests

end module pFUnitDriver_mod
