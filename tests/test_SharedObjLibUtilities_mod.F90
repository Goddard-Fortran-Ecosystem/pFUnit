module test_SharedObjLibUtilities_mod
   use pfUnit
   use SharedObjLibUtilities_mod
   use FortranNameMangle_mod
   implicit none
   private

   public :: test_openLibrary
   public :: test_openLibraryFail
   public :: test_getProcedureHandle
   public :: test_getProcedureHandleFail
   public :: test_ExternalSymbolsA
   public :: test_ExternalSymbolsB
   public :: test_F90ModuleSymbols

contains


   subroutine test_openLibrary()
      type (SharedObjLibUtilities_type) :: shared

      character (len=*), parameter :: libraryName='tests/testData/libfoo.so'

      shared = openLibrary(libraryName)
      if (catch(preserve=.true.)) return

      call assertEqual (libraryName, trim(name(shared)))
      call closeLibrary(shared)

   end subroutine test_openLibrary

   subroutine test_openLibraryFail()
      use pFUnitException_mod, only: catch
      type (SharedObjLibUtilities_type) :: shared

      character (len=*), parameter :: libraryName='libDoesNotExist.so'

      shared = openLibrary(libraryName)
      if (catch()) then
 !!!         call popException ()
      else
         call raiseException ("found library that doesn't exist!???! - may cause problems for later tests")
         call closeLibrary(shared)
         if (catch()) call raiseException('failed to close library')
      endif

   end subroutine test_openLibraryFail

   subroutine test_getProcedureHandle()
      use ProcedurePointer_mod
      type (SharedObjLibUtilities_type) :: shared
      type(ProcedurePointer_type) :: procPointer
      character (len=*), parameter :: libraryName='tests/testData/libfoo.so'
      external invoke0Arguments

      shared = openLibrary(libraryName)
      if (catch()) then
         call raiseException('failed to open library: '//trim(libraryName))
         return
      end if
      procPointer = getProcedureHandle(shared, 'foo', mangle = .true.)
      if (catch()) then
         call raiseException('failed to obtain valid pointer for symbol')
      else
         call invoke0Arguments(procPointer)
      end if

      if(exceptionRaised('foo was called')) then
         call popException()
      else
         call raiseException('foo not called')
      endif

      call closeLibrary(shared)
      if (catch()) call raiseException('failed to close library')

   end subroutine test_getProcedureHandle

   subroutine test_getProcedureHandleFail()
      use ProcedurePointer_mod
      use pFUnitException_mod, only: catch
      type (SharedObjLibUtilities_type) :: shared
      type(ProcedurePointer_type) :: procPointer

      character (len=*), parameter :: libraryName='tests/testData/libfoo.so'

      shared = openLibrary(libraryName)
      if (catch()) then
         call raiseException('failed to open library: '//trim(libraryName))
         return
      end if
      procPointer = getProcedureHandle(shared, 'bar', mangle = .true.)

      ! We expect failure - bar should not exist
      if (catch()) then
         call popException()
      else
         call raiseException('symbol bar() should not have been found')
      endif

      call closeLibrary(shared)

   end subroutine test_getProcedureHandleFail

   subroutine test_externalSymbolsA()
      use ProcedurePointer_mod
      use pFUnitException_mod, only: catch
      type (SharedObjLibUtilities_type) :: shared

      character (len=*), parameter :: libraryName='tests/testData/libfoo.so'
      character (len=MAX_SYMBOL_LENGTH), pointer :: names(:)

      shared = openLibrary(libraryName)
      names => externalNames(shared)
      call assertEqual(1, size(names))

      if (size(names) == 1) then
         call assertEqual(fortranNameMangle('foo'),names(1))
      end if
      deallocate(names)

      call closeLibrary(shared)

   end subroutine test_externalSymbolsA

   subroutine test_externalSymbolsB()
      use ProcedurePointer_mod
      use pFUnitException_mod, only: catch
      type (SharedObjLibUtilities_type) :: shared
      character (len=*), parameter :: libraryName='tests/testData/libfoo2.so'
      character (len=MAX_SYMBOL_LENGTH), pointer :: names(:)
      integer :: i

      shared = openLibrary(libraryName)
      names => externalNames(shared)
      call assertEqual(3, size(names))

      if (size(names) == 3) then
         call assertTrue(Any( (/ (fortranNameMangle('foo') == trim(names(i)),i=1,size(names)) /) ))
         call assertTrue(Any( (/ (fortranNameMangle('foo2') == trim(names(i)),i=1,size(names)) /) ))
         call assertTrue(Any( (/ (fortranNameMangle('foo_3') == trim(names(i)),i=1,size(names)) /) ), 'foo_3 not found')
      end if
      deallocate(names)

      call closeLibrary(shared)
      if (catch()) call raiseException('failed to close library')

   end subroutine test_externalSymbolsB

   subroutine test_F90ModuleSymbols()
      use ProcedurePointer_mod
      use pFUnitException_mod, only: catch
      type (SharedObjLibUtilities_type) :: shared
      character (len=*), parameter :: libraryName='tests/testData/libbar.so'
      character (len=MAX_SYMBOL_LENGTH), pointer :: names(:)
      integer :: i
      integer :: numExpected

      numExpected = 2

      shared = openLibrary(libraryName)
      names => externalNames(shared)
      call assertEqual(numExpected, size(names))

      if (size(names) == numExpected) then
         call assertTrue(Any( (/ (fortranNameMangle('method1', &
              & moduleName = 'bar_mod') == trim(names(i)),i=1,size(names)) /) ), 'method1 not found')
         call assertTrue(Any( (/ (fortranNameMangle('method_2', &
              & moduleName = 'bar_mod') == trim(names(i)),i=1,size(names)) /) ), 'method_2 not found')
      end if
      deallocate(names)

      call closeLibrary(shared)

   end subroutine test_F90ModuleSymbols

end module test_SharedObjLibUtilities_mod
