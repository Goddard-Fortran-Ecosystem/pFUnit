!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: FortranNameMangle 
!
!> @brief
!! Handles Fortran module and procudure names to support the different 
!! compilers.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 12 Mar 2007
!!
! REVISION HISTORY:
! 12 Mar 2007 - Initial Version
! 12 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module FortranNameMangle_mod
   implicit none
   private

   public :: fortranNameMangle
   public :: isModuleProcedure
   public :: moduleName
   public :: procedureName
   public :: lowerCase

   integer, parameter :: MAX_LEN_NAME      = 80
   integer, parameter :: MAX_LEN_PREFIX    = 3
   integer, parameter :: MAX_LEN_SEPARATOR = 5
   integer, parameter :: MAX_LEN_SUFFIX    = 2
   integer, parameter :: MAX_LEN_COMPILER  = 10

   character(len=*), parameter :: MODULE_PREFIX_XLF = '__'
   character(len=*), parameter :: MODULE_PREFIX_G95 = ' '
   character(len=*), parameter :: MODULE_PREFIX_NAG = ' '
   character(len=*), parameter :: MODULE_PREFIX_INTEL = ' '

   character(len=*), parameter :: MODULE_SEPARATOR_XLF = '_MOD_'
   character(len=*), parameter :: MODULE_SEPARATOR_G95 = '_MP_'
   character(len=*), parameter :: MODULE_SEPARATOR_NAG = '_MP_'
   character(len=*), parameter :: MODULE_SEPARATOR_INTEL = '_mp_'

contains

   !---------------------------------------------------------------------------
   !> Contructs the object of the Fortran name including module or procedure 
   !! supporting the specific compiler.
   !!
   !! @param procedureName - given procedure name
   !! @param moduleName - given module name
   !! @param compiler -  given compiler name
   !!
   !! @return the object of Fortran's procedure/module name
   !---------------------------------------------------------------------------
   function fortranNameMangle(procedureName, moduleName, compiler)
      character(len=*), intent(in) :: procedureName
      character(len=*), optional, intent(in) :: moduleName
      character(len=*), optional, intent(in) :: compiler
      character(len= MAX_LEN_NAME) :: fortranNameMangle

      character(len=MAX_LEN_SUFFIX) :: suffix
      character(len=MAX_LEN_NAME) :: base

      character(len=MAX_LEN_PREFIX) :: moduleName_
      character(len=MAX_LEN_COMPILER) :: compiler_

      compiler_ = defaultCompiler()

      if (present(compiler)) compiler_ = compiler

      select case (trim(compiler_))
      case ('xlf')
         suffix = ' '
      case ('g95')
         if (index(trim(procedureName), '_') > 0) then
            suffix = '__'
         else
            suffix = '_'
         endif
      case ('NAG')
         suffix = '_'
      case ('INTEL')
         suffix = '_'
      end select
      
      ! suffix changes if module is present
      if (present(moduleName)) then
         if (trim(compiler_) /= 'INTEL') suffix = ' '
      end if

      if (present(moduleName)) then
         base = trim(prefix(compiler_)) // lowerCase(trim(moduleName)) // trim(separator(compiler_))

      else
         base = ' '
      end if
      fortranNameMangle = trim(base) // lowerCase(trim(procedureName)) // trim(suffix)

   end function fortranNameMangle

   !---------------------------------------------------------------------------
   !> Returns the string with lower case by the given string.
   !! supporting the specific compiler.
   !!
   !! @param string - given string with any case case
   !!
   !! @return new string after it is converted in lower case.
   !---------------------------------------------------------------------------
   function lowerCase(string) result(newString)
      character(len=*), intent(in) :: string
      character(len=len_trim(string))   :: newString

      integer :: i, iascii

      integer, parameter :: ASCII_A = ichar('A')
      integer, parameter :: ASCII_Z = ichar('Z')
      integer, parameter :: SHIFT = ichar('A') - ichar('a')

      do i = 1, len_trim(string)
         iascii = ichar(string(i:i))
         select case (iascii)
         case (ASCII_A:ASCII_Z)
            newString(i:i) = achar(iascii - SHIFT)
         case default
            newString(i:i) = string(i:i)
         end select
      end do

   end function lowerCase

   !---------------------------------------------------------------------------
   !> Determines if the given name with given compiler is procedure module.
   !!
   !! @param name - given name of procedure module 
   !! @param compiler - given type of compiler 
   !!
   !! @return .true. if it is procedure module. Otherwise, .false.
   !---------------------------------------------------------------------------
   logical function isModuleProcedure(name, compiler)
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: compiler
 	
      isModuleProcedure = (hasModulePrefix(name, prefix(compiler)) .and. hasModuleSeparator(name, separator(compiler)))

   end function isModuleProcedure

   !---------------------------------------------------------------------------
   !> Determines if the module has any prefix
   !! 
   !! @param name - given name of module
   !! @param prefix - given prefix
   !! 
   !! @return .true. if it has prefix. Otherwise, .false.
   !---------------------------------------------------------------------------
   logical function hasModulePrefix(name, prefix)
      character(len=*), intent(in) :: name
      character(len=MAX_LEN_PREFIX) :: prefix
      
      hasModulePrefix = (index(name, trim(prefix)) == 1)

   end function hasModulePrefix

   !---------------------------------------------------------------------------
   !> Determines if the module has any separator
   !!
   !! @param name - given name of module
   !! @param prefix - given separator 
   !!
   !! @return .true. if it has separator. Otherwise, .false.
   !---------------------------------------------------------------------------
   logical function hasModuleSeparator(name, separator)
      character(len=*), intent(in) :: name
      character(len=MAX_LEN_PREFIX) :: separator
      
      hasModuleSeparator = (index(name, trim(separator)) > 0)

   end function hasModuleSeparator

   !---------------------------------------------------------------------------
   !> Identifies the prefix by the given compiler.
   !!
   !! @param compiler - given compiler name 
   !!
   !! @return new prefix following up with the compiler.
   !!
   !! @throw Exception when the compier is not supported at this time.
   !---------------------------------------------------------------------------
   function prefix(compiler)
      use pFUnitException_mod
      character(len=*), intent(in) :: compiler
      character(len=MAX_LEN_PREFIX) :: prefix

      select case (trim(compiler))
      case ('xlf')
         prefix = MODULE_PREFIX_XLF
      case ('g95')
         prefix = MODULE_PREFIX_G95
      case ('NAG')
         prefix = MODULE_PREFIX_NAG
      case ('INTEL')
         prefix = MODULE_PREFIX_INTEL
      case default
         call throw(Exception('unsupported compiler option in FortranNameMangle_mod::prefix - '//trim(compiler)))
      end select

   end function prefix

   !---------------------------------------------------------------------------
   !> Identifies the suffix by the given symbol and compiler.
   !!
   !! @param symbol - given symbol name 
   !! @param compiler - given compiler name 
   !!
   !! @return new suffix following up with the compiler.
   !!
   !! @throw Exception when the compier is not supported at this time.
   !---------------------------------------------------------------------------
   function suffix(symbol, compiler)
      use pFUnitException_mod
      character(len=*), intent(in) :: symbol
      character(len=*), intent(in) :: compiler
      character(len=MAX_LEN_PREFIX) :: suffix

      if (isModuleProcedure(symbol, compiler)) then
         suffix = ' '
      else
         select case (trim(compiler))
         case ('xlf')
            suffix = ' '
         case ('g95')
            if ((index(symbol,'_') > 0) .and. (index(symbol,'_') < len_Trim(symbol))) then
               suffix = '__'
            else
               suffix = '_'
            end if
         case ('NAG','INTEL')
            suffix = '_'
         case default
            call throw(Exception('unsupported compiler option in FortranNameMangle_mod::suffix - '//trim(compiler)))
         end select
      end if

   end function suffix

   !---------------------------------------------------------------------------
   !> Identifies the separator that supports the given compiler.
   !!
   !! @param compiler - given compiler name 
   !!
   !! @return new separator supporting the given compiler.
   !!
   !! @throw Exception when the compier is not supported at this time.
   !---------------------------------------------------------------------------
   function separator(compiler)
      use PFUnitException_mod
      character(len=*), intent(in) :: compiler
      character(len=MAX_LEN_SEPARATOR) :: separator

      select case (trim(compiler))
      case ('xlf')
         separator = MODULE_SEPARATOR_XLF
      case ('g95')
         separator = MODULE_SEPARATOR_G95
      case ('NAG')
         separator = MODULE_SEPARATOR_NAG
      case ('INTEL')
         separator = MODULE_SEPARATOR_INTEL
      case default
         call throw(Exception('unsupported compiler option in FortranNameMangle_mod::separator - '//trim(compiler)))
      end select

   end function separator

   !---------------------------------------------------------------------------
   !> Returns the module name with the specific compiler based on determining
   !! if it is module procedure.
   !!
   !! @param symbol - given symbol 
   !! @param compiler - given compiler name 
   !!
   !! @return the string 'bar' if it is module procedure.  Otherwise, returns
   !! the blank string.
   !---------------------------------------------------------------------------
   function moduleName(symbol, compiler)
      character(len = *), intent(in) :: symbol
      character(len = *), intent(in) :: compiler
      character(len = MAX_LEN_NAME) :: moduleName

      if (isModuleProcedure(symbol, compiler)) then
         moduleName = 'bar'
      else
         moduleName = ' '
      end if

   end function moduleName

   !---------------------------------------------------------------------------
   !> Sets the procedure name by following the given symbol and compiler.
   !!
   !! @param symbol - given symbol 
   !! @param compiler - given compiler name 
   !!
   !! @return new procedure name
   !---------------------------------------------------------------------------
   function procedureName(symbol, compiler)
      character(len=*), intent(in) :: symbol
      character(len=*), optional, intent(in) :: compiler
      character(len=MAX_LEN_NAME) :: procedureName

      character(len=MAX_LEN_COMPILER) :: compiler_
      integer :: idxStart
      integer :: idxEnd

      compiler_ = defaultCompiler()
      if (present(compiler)) compiler_ = compiler

      if (isModuleProcedure(symbol, compiler_)) then
         idxStart = index(symbol, trim(separator(compiler_))) + len_trim(separator(compiler_))
         idxEnd = len_trim(symbol)
      else
         idxStart = 1
         idxEnd = len_trim(symbol) - len_trim(suffix(symbol, compiler_))
      end if

      procedureName = symbol(idxStart : idxEnd)

   end function procedureName

   !---------------------------------------------------------------------------
   !> Sets the default compiler 
   !!
   !! @return new compiler name
   !---------------------------------------------------------------------------
   function defaultCompiler()
      character(len=MAX_LEN_COMPILER) :: defaultCompiler
#if defined(IBM)
      defaultCompiler = 'xlf'
#elif defined(G95)
      defaultCompiler = 'g95'
#elif defined(NAG)
      defaultCompiler = 'NAG'
#elif defined(Intel)
      defaultCompiler = 'INTEL'
#endif

   end function defaultCompiler
end module FortranNameMangle_mod
