!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: TestSuite
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
module PF_TestSuite
   use PF_ExceptionList, only : throw
   use PF_Test
   use PF_TestVector
   implicit none
   private

   public :: TestSuite

   type TestReference
      class (Test), allocatable :: pTest
   end type TestReference

   type, extends(Test) :: TestSuite
!!$      private
      character(:), allocatable  :: name
      type (TestVector) :: tests
   contains
      procedure :: getName 
      procedure :: setName
      procedure :: countTestCases
      procedure :: run
      procedure :: addTest
      procedure :: getNumTests
      procedure :: copy
      generic :: assignment(=) => copy
      procedure :: getTestCases
      procedure :: filter
   end type TestSuite

   interface TestSuite
      module procedure newTestSuite_unnamed
      module procedure newTestSuite_named
   end interface TestSuite

contains

   function newTestSuite_unnamed() result(newSuite)
      type (TestSuite) :: newSuite
      newSuite = newTestSuite_named('')
   end function newTestSuite_unnamed

   function newTestSuite_named(name) result(newSuite)
      type (TestSuite) :: newSuite
      character(len=*), intent(in) :: name

      call newSuite%setName(name)

   end function newTestSuite_named

   recursive subroutine copy(this, b)
      class (TestSuite), intent(out) :: this
      type (TestSuite), intent(in) :: b

      this%name = b%name
      this%tests = b%tests
      
   end subroutine copy

   recursive integer function countTestCases(this)
      class (TestSuite), target, intent(in) :: this
      integer :: i

      class (Test), pointer :: t

      countTestCases = 0
      do i = 1, this%tests%size()
         t => this%tests%at(i)
         countTestCases = countTestCases + t%countTestCases()
      end do
  
   end function countTestCases

   recursive subroutine run(this, tstResult, context)
      use PF_ParallelContext
      use PF_TestResult
      class (TestSuite), target, intent(inout) :: this
      class (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context

      class (Test), pointer :: t
      integer :: i

      do i = 1, this%tests%size()
         t => this%tests%at(i)
         call t%run(tstResult, context)
      end do
      
   end subroutine run

   recursive subroutine addTest(this, aTest)
      class (TestSuite), intent(inout) :: this
      class (Test), intent(in) :: aTest

      class (Test), pointer :: t

      call this%tests%push_back(aTest)
      ! prepend suite name
      t => this%tests%back()
      call t%setName(this%getName() // '.' // t%getName())

   end subroutine addTest
   
   pure integer function getNumTests(this) 
      class (TestSuite), intent(in) :: this
      getNumTests = this%tests%size()
   end function getNumTests

   function getName(this) result(name)
      class (TestSuite), intent(in) :: this
      character(:), allocatable :: name
      name = trim(this%name)
   end function getName

   subroutine setName(this, name)
      class (TestSuite), intent(inout) :: this
      character(len=*),  intent(in)    :: name
      this%name = trim(name)
   end subroutine setName

   subroutine  getTestCases(this, testList)
      use PF_Exception
      use PF_Test
      use PF_TestCase
      class (TestSuite), intent(in) :: this
      type (TestVector), intent(out) :: testList

      call accumulateTestCases(this, testList)

   contains
      
      recursive subroutine accumulateTestCases(this, testList)
         class (TestSuite), intent(in) :: this
         class (TestVector), intent(inout) :: testList
         
         integer :: i
         class (Test), pointer :: t

         do i = 1, this%tests%size()
            t => this%tests%at(i)
            select type (t)
            class is (TestCase)
               call testList%push_back(t)
            class is (TestSuite)
               call accumulateTestCases(t, testList)
            class default
               call throw('Unsupported Test subclass in TestSuite::getTestCases()')
            end select
             
          end do

       end subroutine accumulateTestCases

    end subroutine getTestCases


    recursive function filter(this, a_filter) result(new_suite)
      use pf_TestFilter
      type(TestSuite) :: new_suite
      class(TestSuite), intent(in) :: this
      class(TestFilter), intent(in) :: a_filter

      type (TestVectorIterator) :: iter
      class(Test), pointer :: t

      new_suite = TestSuite(this%name)
      
      iter = this%tests%begin()
      do while (iter /= this%tests%end())
         t => iter%get()

         select type (t)
         class is (TestSuite)
            call new_suite%tests%push_back(t%filter(a_filter))
         class default
            if (a_filter%filter(t)) then
               call new_suite%tests%push_back(t)
            end if
         end select
         call iter%next()
      end do

    end function filter


 end module PF_TestSuite
