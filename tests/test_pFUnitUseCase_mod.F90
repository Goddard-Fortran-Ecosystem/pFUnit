module test_pFUnitUseCase_mod
   use pfunit
   use Sharedobjlibutilities_mod
   use pFUnitDriver_mod
   implicit none
   private

   public :: test_testCase

contains

   subroutine test_testCase()
      use ProcedurePointer_mod
      type (SharedObjLibUtilities_type) :: shared
      type(ProcedurePointer_type) :: procPointer

      type (TestResult_type) :: aResult
      type (TestSuite_type) :: aSuite
      type (TestCase_type) :: tCase

      character (len=*), parameter :: libraryName='tests/testData/libpFUnitTestsA.so'
      character (len=MAX_SYMBOL_LENGTH), pointer :: names(:)
      character(len=100)  :: summary_statement
      integer :: i
      type (Exception_type) :: foundException

      shared = openLibrary(libraryName)
      if (catch(preserve=.true.)) return

      names => externalNames(shared)
      aSuite = TestSuite('libpFUnitTestsA.so')

      do i = 1, size(names)
         if (index(names(i),'test_') > 0) then
            procPointer = getProcedureHandle(shared,names(i))
            if (catch(preserve=.true.)) return
            tcase = TestCase()
            call addTestMethodFromAddress(tcase, names(i), address(procPointer))
            call add(aSuite, tCase)
         end if
      end do

      deallocate(names)

      aResult = newTestResult()

      call run(aSuite, aResult)
      summary_statement = Summary(aResult)
      
      call assertEqual("2 run, 1 failed (1 severe)", summary_Statement(1:26))

      call clean(aResult)
      call clean(aSuite)
      call closeLibrary(shared)

   end subroutine test_testCase

end module test_pFUnitUseCase_mod
