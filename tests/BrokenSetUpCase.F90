module BrokenSetUpCase_mod
   use TestCase_mod, only: TestCase
   implicit none
   private
   
   public :: BrokenSetUpCase
   public :: newBrokenSetUpCase
   
   type, extends(TestCase) :: BrokenSetUpCase
      private
      character(len=40), public :: runLog
   contains
      procedure :: setUp
      procedure :: runTestMethod
      procedure :: tearDown
   end type BrokenSetUpCase
   
contains

   function newBrokenSetUpCase() result(this)
      type (BrokenSetUpCase), pointer :: this
      allocate(this)
      this%name = 'BrokenSetUpCase'
   end function newBrokenSetUpCase

   subroutine setUp(this)
      use Exception_mod, only: throw, getNumExceptions
      class(BrokenSetUpCase), intent(inOut) :: this

      this%runLog = 'broken setUp'
      call throw('This setUp() is intentionally broken.')

   end subroutine setUp

   subroutine tearDown(this)
      class(BrokenSetUpCase), intent(inOut) :: this

      this%runLog = trim(this%runLog)//' tearDown'

   end subroutine tearDown

   subroutine runTestMethod(this)
      class(BrokenSetUpCase), intent(inOut) :: this

      this%runLog = trim(this%runLog)//' run'

   end subroutine runTestMethod

end module BrokenSetUpCase_mod
