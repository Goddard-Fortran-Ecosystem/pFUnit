! Serial TestCase 
module TestCase_mod
   use SurrogateTestCase_mod
   use TestResult_mod
   use Test_mod

   private

   public :: TestCase

   type, extends(SurrogateTestCase) :: ConcreteSurrogate
      private
      class (TestCase), pointer :: tCase => null()
   contains
      procedure :: runBare => runBare_surrogate
      procedure :: setName => setName_surrogate
      procedure :: getName => getName_surrogate
   end type ConcreteSurrogate
   
#ifndef DEFERRED_LENGTH_CHARACTER
   integer, parameter :: MAX_LENGTH_NAME = 32
#endif

   type, abstract, extends(Test) :: TestCase
      private
      type (ConcreteSurrogate) :: surrogate
#ifdef DEFERRED_LENGTH_CHARACTER
      character(:), allocatable :: name
#else
      character(len=MAX_LENGTH_NAME) :: name
#endif
   contains
      procedure :: setSurrogate
      procedure :: getName 
      procedure :: setName
      procedure :: countTestCases
      procedure :: run
      procedure :: runBare
      procedure :: setUp
      procedure :: tearDown
      procedure :: getSurrogate
      procedure(runMethod), deferred :: runMethod
   end type TestCase

   abstract interface
      subroutine runMethod(this)
         import TestCase
         class (TestCase), intent(inout) :: this
      end subroutine runMethod
   end interface

contains

   function getName(this) result(name)
      class (TestCase), intent(in) :: this
      character(:), allocatable :: name
      name = this%name
   end function getName

   subroutine setName(this, name)
      class (TestCase), intent(inout) :: this
      character(len=*),intent(in) :: name

      ! Make certain surrogate points back to self
      call this%setSurrogate()
      this%name = trim(name)

   end subroutine setName

   integer function countTestCases(this)
      class (TestCase), intent(in) :: this
      countTestCases = 1
   end function countTestCases

! Implement deferred method from class Test
   subroutine run(this, tstResult, context)
      use SerialContext_mod
      use TestResult_mod
      use ParallelContext_mod
      class (TestCase), intent(inout) :: this
      class (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context

      ! Always run serial tests in a serial context.
      if (context%isRootProcess()) then
         call this%setSurrogate()
         call tstResult%run(this%surrogate, THE_SERIAL_CONTEXT)
      end if

   end subroutine run

   subroutine runBare(this)
      use Exception_mod, only: noExceptions
      class (TestCase), intent(inout) :: this

      call this%setUp()
      if (noExceptions()) then
         call this%runMethod()
         call this%tearDown()
      end if

   end subroutine runBare

   subroutine runBare_surrogate(this)
      class (ConcreteSurrogate), intent(inout) :: this
      class (TestCase), pointer :: p
      p => this%tCase
      call p%runBare()
   end subroutine runBare_surrogate

   function getName_surrogate(this) result(name)
      class (ConcreteSurrogate), intent(in) :: this
      character(:), allocatable :: name
      name = this%tCase%getName()
   end function getName_surrogate

   subroutine setName_surrogate(this, name)
      class (ConcreteSurrogate), intent(inout) :: this
      character(len=*),intent(in) :: name
      call this%tCase%setName(trim(name))
   end subroutine setName_surrogate

   subroutine setUp(this)
      class (TestCase), intent(inOut) :: this
   end subroutine setUp

   subroutine tearDown(this)
      class (TestCase), intent(inOut) :: this
   end subroutine tearDown

   function getSurrogate(this) result(surrogate)
      class (TestCase), target, intent(in) :: this
      type (ConcreteSurrogate), pointer :: surrogate
      surrogate => this%surrogate
   end function getSurrogate

   subroutine setSurrogate(this)
      class (TestCase), target :: this
      this%surrogate%tCase => this
   end subroutine setSurrogate

end module TestCase_mod
