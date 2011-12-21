module TestSuite_mod
   use Test_mod
   use TestResult_mod
   implicit none
   private

   public :: TestSuite
   public :: newTestSuite

   type TestPointer
      class (Test), pointer :: pTest => null()
   end type TestPointer

   type, extends(Test) :: TestSuite
      type (TestPointer), allocatable :: tests(:)
   contains
      procedure :: countTestCases
      procedure :: run
      procedure :: addTest
      procedure :: getNumMembers
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
      do i = 1, this%getNumMembers()
         countTestCases = countTestCases + this%tests(i)%pTest%countTestCases()
      end do
  
   end function countTestCases

   recursive subroutine run(this, tstResult, context)
      use ParallelContext_mod
      class (TestSuite), intent(inout) :: this
      type (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context

      integer :: i
      
      class (Test), pointer :: aTest
      do i = 1, this%getNumMembers()
         aTest => this%tests(i)%pTest
         call aTest%run(tstResult, context)
      end do
      
   end subroutine run
   
   subroutine addTest(this, aTest)
      class (TestSuite), intent(inout) :: this
      class (Test), target, intent(in) :: aTest
      
      call extend(this%tests)
      this%tests(this%getNumMembers())%pTest => aTest
      
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
   
   integer function getNumMembers(this) 
      class (TestSuite), intent(in) :: this
      getNumMembers = size(this%tests)
   end function getNumMembers
   
end module TestSuite_mod
