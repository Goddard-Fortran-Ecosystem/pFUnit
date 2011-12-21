module test_FortranNameMangle_mod
   use pFUnit
   use FortranNameMangle_mod
   implicit none
   private

   public :: test_lowerCase
   public :: test_fortranNameMangle_xlf
   public :: test_fortranNameMangle_g95
   public :: test_fortranNameMangle_nag

   public :: test_isModuleProcedure
   public :: test_moduleName
   public :: test_procedureName


contains

   subroutine test_lowerCase()

      call assertEqual('a',lowerCase('A'))
      call assertEqual('abcd',lowerCase('ABCD'))
      call assertEqual('abcd',lowerCase('abCd'))
      call assertEqual('abcd',lowercase('abCd '),ignoreWhiteSpace=.true.)

   end subroutine test_lowerCase


   subroutine test_FortranNameMangle_xlf()
      character(len=*), parameter :: compiler = 'xlf'
      
      call assertEqual('foo', fortranNameMangle('foo', compiler = compiler))
      call assertEqual('foo', fortranNameMangle('FOO', compiler = compiler))
      call assertEqual('foo_3', fortranNameMangle('foo_3', compiler = compiler))

      call assertEqual('__bar_mod_MOD_method1', fortranNameMangle('method1', moduleName = 'Bar_mod', compiler = compiler))

   end subroutine test_FortranNameMangle_xlf

   subroutine test_FortranNameMangle_g95()
      character(len=*), parameter :: compiler = 'g95'

      call assertEqual('foo_', fortranNameMangle('foo', compiler = compiler))
      call assertEqual('foo_', fortranNameMangle('FoO', compiler = compiler))
      call assertEqual('foo_3__', fortranNameMangle('foo_3', compiler = compiler))

      call assertEqual('bar_mod_MP_method1', fortranNameMangle('method1', moduleName = 'Bar_mod', compiler = compiler))
   end subroutine test_FortranNameMangle_g95

   subroutine test_FortranNameMangle_nag()
      character(len=*), parameter :: compiler = 'NAG'

      call assertEqual('foo_', fortranNameMangle('foo', compiler = compiler))
      call assertEqual('foo_', fortranNameMangle('FoO', compiler = compiler))
      call assertEqual('bar_mod_MP_method1', &
           & fortranNameMangle('method1', moduleName = 'Bar_mod', compiler = compiler))

   end subroutine test_FortranNameMangle_nag


   subroutine test_procedureName()

      call tstIdentity('foo')
      call tstIdentity('foo_')
      call tstIdentity('foo_2')

      call tstIdentity('foo', f90Module = 'bar')
      call tstIdentity('foo_', f90Module = 'bar')
      call tstIdentity('foo_2', f90Module = 'bar')

      call tstIdentity('test_b', f90Module = 'bar')

   contains

      subroutine tstIdentity(name, f90Module)
         character(len=*), intent(in) :: name
         character(len=*), optional, intent(in) :: f90Module
         
         call assertIdentity(name, 'xlf', f90Module)
         call assertIdentity(name, 'g95', f90Module)
         call assertIdentity(name, 'NAG', f90Module)

      end subroutine tstIdentity
      
      subroutine assertIdentity(name, compiler, f90Module)
         character(len=*), intent(in) :: name
         character(len=*), intent(in) :: compiler
         character(len=*), optional, intent(in) :: f90Module
         integer, parameter :: MAX_LEN = 100
         character(len=MAX_LEN) :: identity
         character(len=MAX_LEN) :: mangledName

         mangledName = fortranNameMangle(name, moduleName = f90Module, compiler = compiler)
         identity = procedureName(mangledName, compiler = compiler)

         call assertEqual(lowerCase(name), identity)

      end subroutine assertIdentity

   end subroutine test_procedureName

   subroutine test_isModuleProcedure()

      call tst_isModuleProcedure('foo', 'bar')
      call tst_isModuleProcedure('Foo_', 'bar')
      call tst_isModuleProcedure('foo_b', 'bar')

      call tst_isModuleProcedure('foo', 'bar_')
      call tst_isModuleProcedure('foo_', 'bar_Mod')

   contains

      subroutine tst_isModuleProcedure(name, mod)
         character(len=*), intent(in) :: name
         character(len=*), intent(in) :: mod

         call assertTrue(isModuleProcedure(fortranNameMangle(name, mod, compiler = 'xlf'), compiler = 'xlf'))
         call assertTrue(isModuleProcedure(fortranNameMangle(name, mod, compiler = 'g95'), compiler = 'g95'))
         call assertTrue(isModuleProcedure(fortranNameMangle(name, mod, compiler = 'NAG'), compiler = 'NAG'))
         
         call assertFalse(isModuleProcedure(fortranNameMangle(name, compiler = 'xlf'), compiler = 'xlf'))
         call assertFalse(isModuleProcedure(fortranNameMangle(name, compiler = 'g95'), compiler = 'g95'))
         call assertFalse(isModuleProcedure(fortranNameMangle(name, compiler = 'NAG'), compiler = 'NAG'))

      end subroutine tst_isModuleProcedure

   end subroutine test_isModuleProcedure

   subroutine test_moduleName()

      call tst_moduleName('foo', 'bar')
      call tst_moduleName('Foo_', 'bar')
      call tst_moduleName('foo_b', 'bar')

   contains

      subroutine tst_moduleName(name, mod)
         character(len=*), intent(in) :: name
         character(len=*), intent(in) :: mod

         call assertEqual(lowerCase(mod), moduleName(fortranNameMangle(name, mod, compiler = 'xlf'), compiler = 'xlf'))
         call assertEqual(lowerCase(mod), moduleName(fortranNameMangle(name, mod, compiler = 'g95'), compiler = 'g95'))
         call assertEqual(lowerCase(mod), moduleName(fortranNameMangle(name, mod, compiler = 'NAG'), compiler = 'NAG'))

         call assertEqual(0, len_trim(moduleName(fortranNameMangle(name, compiler = 'xlf'), compiler = 'xlf')))
         call assertEqual(0, len_trim( moduleName(fortranNameMangle(name, compiler = 'g95'), compiler = 'g95')))
         call assertEqual(0, len_trim(moduleName(fortranNameMangle(name, compiler = 'NAG'), compiler = 'NAG')))

      end subroutine tst_moduleName

   end subroutine test_moduleName

end module test_FortranNameMangle_mod
