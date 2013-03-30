module TestSuite_mod
   use Test_mod
   implicit none
   private

   public :: TestSuite
   public :: newTestSuite

   type TestPointer
      class (Test), pointer :: pTest => null()
   end type TestPointer
   integer, parameter :: MAX_LENGTH_NAME = 32

   type, extends(Test) :: TestSuite
      private
      character(MAX_LENGTH_NAME) :: name
      type (TestPointer), allocatable :: tests(:)
   contains
      procedure :: getName 
      procedure :: setName
      procedure :: countTestCases
      procedure :: run
      procedure :: addTest
      procedure :: getNumTests
   end type TestSuite

   interface newTestSuite
      module procedure newTestSuite_unnamed
      module procedure newTestSuite_named
   end interface newTestSuite

contains

   function newTestSuite_unnamed() result(newSuite)
      type (TestSuite), pointer :: newSuite
      allocate(newSuite)
      allocate(newSuite%tests(0))
   end function newTestSuite_unnamed

   function newTestSuite_named(name) result(newSuite)
      type (TestSuite), pointer :: newSuite
      character(len=*), intent(in) :: name
      newSuite => newTestSuite()
      call newSuite%setName(name)
   end function newTestSuite_named

   recursive integer function countTestCases(this)
      class (TestSuite), intent(in) :: this
      integer :: i
      
      countTestCases = 0
      do i = 1, this%getNumTests()
         countTestCases = countTestCases + this%tests(i)%pTest%countTestCases()
      end do
  
   end function countTestCases

   recursive subroutine run(this, tstResult, context)
      use ParallelContext_mod
      use TestResult_mod
      class (TestSuite), intent(inout) :: this
      class (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context

      integer :: i
      
      class (Test), pointer :: aTest
      do i = 1, this%getNumTests()
!!$         aTest => this%tests(i)%pTest
         call this%tests(i)%ptest%run(tstResult, context)
      end do
      
   end subroutine run
   
   subroutine addTest(this, aTest)
      class (TestSuite), intent(inout) :: this
      class (Test), intent(in) :: aTest
      
      call extend(this%tests)
      allocate(this%tests(this%getNumTests())%pTest, source=aTest)
      
   contains   
      
      subroutine extend(list)
         type (TestPointer), allocatable :: list(:)
         type (TestPointer), allocatable :: temp(:)
         integer :: n
         
         n = size(list)
         allocate(temp(n))
         temp = list
         
         deallocate(list)
         allocate(list(n+1))
         list(:n) = temp
         deallocate(temp)
         
      end subroutine extend
      
   end subroutine addTest
   
   pure integer function getNumTests(this) 
      class (TestSuite), intent(in) :: this
      getNumTests = size(this%tests)
   end function getNumTests

   function getName(this) result(name)
      class (TestSuite), intent(in) :: this
      character(MAX_LENGTH_NAME) :: name
      name = trim(this%name)
   end function getName

   subroutine setName(this, name)
      class (TestSuite), intent(inout) :: this
      character(len=*),intent(in) :: name
      this%name = trim(name)
   end subroutine setName
   
end module TestSuite_mod
