#include "unused_dummy.fh"
!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: TestCase
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune, NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
! Serial TestCase 
module PF_TestCase
   use PF_SurrogateTestCase
   use PF_TestResult
   use PF_Test

   private

   public :: TestCase
   public :: TestCaseReference

   type, extends(SurrogateTestCase) :: ConcreteSurrogate
      private
      class (TestCase), pointer :: tCase => null()
   contains
      procedure :: runBare => runBare_surrogate
      procedure :: setName => setName_surrogate
      procedure :: getName => getName_surrogate
      procedure :: is_disabled
   end type ConcreteSurrogate
   
   type, abstract, extends(Test) :: TestCase
      private
      type (ConcreteSurrogate) :: surrogate
      character(:), allocatable :: name
   contains
      procedure :: setSurrogate
      procedure :: baseName
      procedure :: getName 
      procedure :: setName
      procedure :: countTestCases
      procedure :: run
      procedure :: runBare
      procedure :: setUp
      procedure :: tearDown
      procedure :: getSurrogate
      procedure :: runMethod
   end type TestCase

   type TestCaseReference
      class (TestCase), allocatable :: test
   end type TestCaseReference

contains

   function baseName(this) result(name)
      class (TestCase), intent(in) :: this
      character(:), allocatable :: name
      name = this%name
   end function baseName

   function getName(this) result(name)
      class (TestCase), intent(in) :: this
      character(:), allocatable :: name
      name = this%baseName()
   end function getName

   subroutine setName(this, name)
      class (TestCase), intent(inout) :: this
      character(len=*),intent(in) :: name

      this%name = trim(name)

   end subroutine setName

   integer function countTestCases(this)
      class (TestCase), target, intent(in) :: this
      _UNUSED_DUMMY(this)
      countTestCases = 1
   end function countTestCases

! Implement deferred method from class Test
   recursive subroutine run(this, tstResult, context)
      use PF_SerialContext
      use PF_TestResult
      use PF_ParallelContext
      class (TestCase), target, intent(inout) :: this
      class (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context

      ! NAG 6.1 revealed that one cannot rely on surrogate
      ! due to the non-TARGET for argument "this".
      ! NAG provided the workaround with this inner procedure.

      call inner_run(this, tstresult, context)

   contains

      recursive subroutine inner_run(this,tstresult,context)
        class (TestCase), intent(inout), target :: this
        class (TestResult), intent(inout) :: tstresult
        class (ParallelContext), intent(in) :: context
        class (SurrogateTestCase), allocatable :: surrogate

        ! Always run serial tests in a serial context.
        if (context%isRootProcess()) then
           allocate(surrogate,source=this%getSurrogate())
           call tstResult%run(surrogate, THE_SERIAL_CONTEXT)
        end if

        call context%barrier()

     end subroutine inner_run

   end subroutine run

   recursive subroutine runBare(this)
      use PF_ExceptionList, only: anyExceptions
      class (TestCase), intent(inout) :: this

      call this%setUp()
      if (.not. anyExceptions()) then
         call this%runMethod()
         call this%tearDown()
      end if

   end subroutine runBare

   recursive subroutine runBare_surrogate(this)
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
      _UNUSED_DUMMY(this)
   end subroutine setUp

   subroutine tearDown(this)
      class (TestCase), intent(inOut) :: this
      _UNUSED_DUMMY(this)
   end subroutine tearDown

   function getSurrogate(this) result(surrogate)
      class (TestCase), target, intent(inout) :: this
      class (SurrogateTestCase), pointer :: surrogate
      call this%setSurrogate()
      surrogate => this%surrogate
   end function getSurrogate

   subroutine setSurrogate(this)
      class (TestCase), target :: this
      this%surrogate%tCase => this
   end subroutine setSurrogate

   recursive subroutine runMethod(this)
      use PF_ExceptionList, only: throw
      class (TestCase), intent(inout) :: this
      _UNUSED_DUMMY(this)
      call throw('TestCase::runMethod() must be overridden.')
   end subroutine runMethod

   logical function is_disabled(this)
      class (ConcreteSurrogate), intent(in) :: this

      is_disabled = this%tCase%is_disabled()

   end function is_disabled

end module PF_TestCase
