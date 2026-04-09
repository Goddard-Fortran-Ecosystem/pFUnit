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
      logical :: shuffle_enabled = .false.
      integer :: shuffle_seed = 0
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
      procedure :: filter_sub
      procedure :: set_shuffle
      procedure :: shuffle_tests
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
      this%shuffle_enabled = b%shuffle_enabled
      this%shuffle_seed = b%shuffle_seed

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

      ! Shuffle tests if enabled
      if (this%shuffle_enabled) then
         call this%shuffle_tests()
      end if

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

    recursive subroutine filter_sub(this, a_filter, new_suite)
      use pf_TestFilter
      type(TestSuite), intent(inout) :: new_suite
      class(TestSuite), intent(in) :: this
      class(TestFilter), intent(in) :: a_filter

      type (TestVectorIterator) :: iter
      class(Test), pointer :: t
      type(TestSuite) :: inner_suite

      iter = this%tests%begin()
      do while (iter /= this%tests%end())
         t => iter%of()

         select type (t)
         class is (TestSuite)
            inner_suite = TestSuite(t%name)
            call t%filter_sub(a_filter, inner_suite)
            call new_suite%tests%push_back(inner_suite)
         class default
            if (a_filter%filter(t)) then
               call new_suite%tests%push_back(t)
            end if
         end select
         call iter%next()
      end do

    end subroutine filter_sub

    recursive function filter(this, a_filter) result(new_suite)
      use pf_TestFilter
      type(TestSuite) :: new_suite
      class(TestSuite), intent(in) :: this
      class(TestFilter), intent(in) :: a_filter

      new_suite = TestSuite(this%name)
      call this%filter_sub(a_filter, new_suite)

    end function filter


   subroutine set_shuffle(this, seed)
      class(TestSuite), intent(inout) :: this
      integer, intent(in) :: seed

      this%shuffle_enabled = .true.
      this%shuffle_seed = seed
   end subroutine set_shuffle


   subroutine shuffle_tests(this)
      class(TestSuite), intent(inout) :: this
      integer :: i, n
      integer, allocatable :: indices(:)
      type(TestVector) :: shuffled_tests

      n = this%tests%size()
      if (n <= 1) return

      call initialize_random_seed(this%shuffle_seed)

      ! Create shuffled index array
      indices = [(i, i=1, n)]
      call shuffle_indices(indices)

      ! Build new vector in shuffled order
      shuffled_tests = TestVector()
      do i = 1, n
         call shuffled_tests%push_back(this%tests%at(indices(i)))
      end do

      this%tests = shuffled_tests
   end subroutine shuffle_tests


   subroutine initialize_random_seed(user_seed)
      integer, intent(in) :: user_seed
      integer, allocatable :: seed_array(:)
      integer :: seed_size

      if (user_seed == 0) then
         ! Use compiler's default random initialization
         call random_seed()
         return
      end if

      ! Use user-provided seed
      call random_seed(size=seed_size)
      seed_array = spread(user_seed, 1, seed_size)
      call random_seed(put=seed_array)
   end subroutine initialize_random_seed


   subroutine shuffle_indices(indices)
      integer, intent(inout) :: indices(:)
      integer :: i, j, n, temp
      real :: rnd

      n = size(indices)
      ! Fisher-Yates shuffle
      do i = n, 2, -1
         call random_number(rnd)
         j = int(rnd * i) + 1
         if (i == j) cycle
         temp = indices(i)
         indices(i) = indices(j)
         indices(j) = temp
      end do
   end subroutine shuffle_indices


 end module PF_TestSuite
