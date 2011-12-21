!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: TestSuite_mod
!
!> @brief
!! Implements the "Composite" pattern to treat collections of TestCase.
!! Member elements can in turn be other TestSuite() objects.  The Run() method 
!! recursively invokes Run() on member elements, with the name of the parent 
!! structure prepended to the testname for reporting purposes.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 16 Oct 2006
!!
! REVISION HISTORY:
! 16 Oct 2006 - Initial Version
! 13 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module TestSuite_mod
   use Assert_mod
   use Params_mod,                only: MAX_LEN_NAME
   use Testcase_mod,              only: TestCase_type
   use ParameterizedTestCase_mod, only: ParameterizedTestCase_type
#ifdef USE_MPI
   use MpiTestCase_mod,         only: MpiTestCase_type
#endif
   implicit none
   private

   public :: TestSuite_type   ! Data type
   public :: TestSuite        ! Constructor
   public :: Add              ! Add element to suite
   public :: Run
   public :: countTests
   public :: Clean

   !---------------------------------------------------
   ! Fake "enum" for different types of suite elements
   !---------------------------------------------------
   integer, Parameter :: ID_TESTCASE   = 1
   integer, Parameter :: ID_TESTSUITE  = 2
   integer, Parameter :: ID_PARAMTEST  = 3
   integer, Parameter :: ID_NEW_MPITEST = 5

   !-------------------------------------------------------------
   ! This data type implements a generic member element.  The
   ! "id" component indicates which actual type is referenced.
   ! In this manner, varying types of elements can be aggregated
   ! into an array.
   !-------------------------------------------------------------
   type GenericTest_type
      private
      integer :: id
      type (TestCase_type),               pointer :: tcase => Null()
      type (ParameterizedTestCase_type),  pointer :: ptest => Null()
      type (TestSuite_type),              pointer :: tsuite => Null()
#ifdef USE_MPI
      type (MpiTestCase_type),         pointer :: newmpitest => Null()
#endif
   end type GenericTest_type

   !----------------------------------------------------------------
   ! A TestSuite is then merely an array of member generic elements,
   ! along with a name for reference purposes.
   !----------------------------------------------------------------
   type TestSuite_type
      private
      character(len=MAX_LEN_NAME)   :: name
      type (GenericTest_type), pointer :: tests(:) => Null()
   end type TestSuite_type

   interface Add
      module procedure add_testcase
      module procedure add_testsuite
      module procedure add_paramtest
#ifdef USE_MPI
      module procedure add_newmpitest
#endif
   end interface

   interface Clean
      module procedure clean_
   end interface

   interface TestSuite
      module procedure testsuite_empty
      module procedure testsuite_arrayofsuites
   end interface

   interface Run
      module procedure run_
   end interface
  
   interface countTests
      module procedure countTests_testsuite
   end interface

contains

   !---------------------------------------------------------------------------
   !> Constructs an empty TestSuite with given name. 
   !!
   !! @param name - given name 
   !!
   !! @return new object of test suite 
   !---------------------------------------------------------------------------
   function TestSuite_empty(name) result(suite)
     type (TestSuite_type) :: suite
     character(len=*), intent(In) :: name

     suite%name = trim(name)
     call assertFalse(associated(suite%tests))
     allocate(suite%tests(0))

   end function TestSuite_empty

   !---------------------------------------------------------------------------
   !> Constructs a TestSuite from an array of suites.
   !!
   !! @param name - given name 
   !! @param name - given suites in array 
   !!
   !! @return new object of test suite 
   !---------------------------------------------------------------------------
   function Testsuite_arrayofsuites(name, suites) result(suite)
      type (TestSuite_type)             :: suite
      character(len=*),   intent(In) :: name
      type (TestSuite_type), intent(in) :: suites(:)

      integer :: i_suite, n_suites

      suite%name = trim(name)

      n_suites = Size(suites)
      allocate(suite%tests(n_suites))
      do i_suite = 1, n_suites
         suite%tests(i_suite)%id = ID_TESTSUITE
         allocate(suite%tests(i_suite)%tsuite)
         suite%tests(i_suite)%tsuite = suites(i_suite)
      end do

   end function Testsuite_arrayofsuites

   !---------------------------------------------------------------------------
   !> Appends TestCase to the member elements.
   !!
   !! @param self - this test suite object 
   !! @param test - given test case object
   !---------------------------------------------------------------------------
   subroutine Add_testcase(self, test)
      type (TestSuite_type) :: self
      type (TestCase_type)  :: test

      type (GenericTest_type), pointer :: new_list(:)
      integer :: n_tests

      n_tests = Size(self%tests)

      call ExtendByOne(self%tests)

      call SetElement(self%tests(n_tests+1), ID_TESTCASE)
      self%tests(n_tests+1)%tcase = test  ! warning: shallow copy

   end subroutine Add_testcase

#ifdef USE_MPI
   !---------------------------------------------------------------------------
   !> Appends MpiTestCase to the member elements.
   !!
   !! @param self - this test suite object 
   !! @param test - given MPI test case object
   !---------------------------------------------------------------------------
   subroutine Add_newmpitest(self, test)
      type (TestSuite_type) :: self
      type (MpiTestCase_type) :: test

      type (GenericTest_type), pointer :: new_list(:)
      integer :: n_tests

      n_tests = Size(self%tests)

      call ExtendByOne(self%tests)

      call SetElement(self%tests(n_tests+1), ID_NEW_MPITEST)
      self%tests(n_tests+1) % newmpitest = test  ! warning: shallow copy

   end subroutine Add_newmpitest
#endif

   !---------------------------------------------------------------------------
   !> Appends ParameterizedTestCase to the member elements.
   !!
   !! @param self - this test suite object 
   !! @param test - given parameterized test case object
   !---------------------------------------------------------------------------
   subroutine Add_paramtest(self, test)
      type (TestSuite_type) :: self
      type (ParameterizedTestCase_type)  :: test

      integer :: n_tests

      n_tests = Size(self%tests)
      call ExtendByOne(self%tests)

      call SetElement(self%tests(n_tests+1), ID_PARAMTEST)
      self%tests(n_tests+1)%ptest = test ! warning: shallow copy
    
   end subroutine Add_paramtest

   !---------------------------------------------------------------------------
   !> Appends TestSuite to the member elements.
   !!
   !! @param self - this test suite object 
   !! @param test - given test suite object
   !---------------------------------------------------------------------------
   subroutine Add_testsuite(self, suite)
      type (TestSuite_type) :: self
      type (TestSuite_type)  :: suite

      integer :: n_tests

      n_tests = Size(self%tests)
      call ExtendByOne(self%tests)

      call SetElement(self%tests(n_tests+1), ID_TESTSUITE)
      self%tests(n_tests+1)%tsuite = suite ! warning: shallow copy
    
   end subroutine Add_testsuite
  
   !---------------------------------------------------------------------------
   !> Allocates new storage sufficient for the additional element, and 
   !! copy original data into new location.
   !!
   !! @param tests - given generic test object
   !---------------------------------------------------------------------------
   subroutine ExtendByOne(tests)
      type (GenericTest_type), pointer :: tests(:)

      integer                       :: n_tests
      type (GenericTest_type), pointer :: new_list(:)

      n_tests = Size(tests)
      allocate(new_list(n_tests+1))
      new_list(1:n_tests) = tests
      deallocate(tests)
      tests => new_list
      nullify(new_list)

   End subroutine ExtendByOne

   !---------------------------------------------------------------------------
   !> Prepares generic element to accept specific type.
   !!
   !! @param tests - given generic test object
   !---------------------------------------------------------------------------
   subroutine SetElement(gentest, id)
      type (GenericTest_type) :: gentest
      integer              :: id

      call assertFalse(associated(gentest%tcase))
      call assertFalse(associated(gentest%ptest))
      call assertFalse(associated(gentest%tsuite))

      gentest%id = id
      select case(id)
      case (ID_TESTCASE)
         allocate(gentest%tcase)
      case (ID_TESTSUITE)
         allocate(gentest%tsuite)
      case (ID_PARAMTEST)
         allocate(gentest%ptest)
#ifdef USE_MPI
      case (ID_NEW_MPITEST)
         allocate(gentest % newmpitest)
#endif
      end select

   end subroutine SetElement

   !---------------------------------------------------------------------------
   !> Loops over member elements and invoke Run() on each.
   !! Must be recursive, since may invoke Run() on nested suites.
   !! Suite name is pushed onto the TestResult name stack for more
   !! meaningful messages, and popped when done.
   !!
   !! @param self - this test suite object
   !! @param result - given test result object
   !!
   !! @return new object of test suite
   !---------------------------------------------------------------------------
   recursive subroutine run_(self, result)
      use TestResult_mod
      type (TestSuite_type) :: self
      integer :: itest
      type (TestResult_type) :: result

      call PushPrefix(result,Trim(self%name))
      do itest = 1, Size(self%tests)
         call RunGeneric(self%tests(itest), result)
      end do
      call PopPrefix(result)

   end subroutine run_

   !---------------------------------------------------------------------------
   !> Invokes Run() on a generic element.  True polymorphism would eliminate 
   !! most of these annoying "select case" constructs.
   !!
   !! @param self - this generic test suite object
   !! @param result - given test result object
   !!
   !! @return new object of test suite
   !---------------------------------------------------------------------------
   recursive subroutine RunGeneric(self, result)
      use TestResult_mod,            only: TestResult_type
      use TestCase_mod,              only: Run
#ifdef USE_MPI
      use MpiTestCase_mod,         only: Run
#endif
      use ParameterizedTestCase_mod, only: Run
      use pFUnitException_mod
      use TestInfo_mod
      type (GenericTest_type) :: self
      type (TestResult_type)  :: result

      select case (self%id)
      case (ID_TESTCASE)
         call Run(self%tcase,result)
      case (ID_TESTSUITE)
         call Run(self%tsuite,result)
      case (ID_PARAMTEST)
         call Run(self%ptest,result)
#ifdef USE_MPI
      case (ID_NEW_MPITEST)
         call Run(self%newmpitest,TestInfo(), result)
#endif
      case Default
         call throw(Exception('Invalid id in TestSuite_mod::RunGeneric()'))
      end select

   end subroutine RunGeneric

   !---------------------------------------------------------------------------
   !> Counts the tests and sub-counts the test
   !!
   !! @param this - this test suite object
   !! @param total- output for the total of tests
   !---------------------------------------------------------------------------
   recursive subroutine countTestsSub(this, total)
      use TestCase_mod, only: countTests
      use ParameterizedTestCase_mod, only: countTests
#ifdef USE_MPI
      use MpiTestCase_mod, only: countTests
#endif
      type (TestSuite_type), intent(in) :: this
      integer, intent(out) :: total
      integer :: cnt, i, subcount

      cnt = 0
      do i = 1, size(this % tests)
         select case (this % tests(i) % id)
         case (ID_TESTCASE)
            cnt = cnt + countTests(this % tests(i) % tcase)
         case (ID_TESTSUITE)
            call countTestsSub(this % tests(i) % tsuite, subcount)
            cnt = cnt + subcount
         case (ID_PARAMTEST)
            cnt = cnt + countTests(this % tests(i) % ptest)
#ifdef USE_MPI
         case (ID_NEW_MPITEST)
            cnt = cnt + countTests(this % tests(i) % newmpitest)
#endif
         end select
      end do
      total = cnt

   end subroutine countTestsSub

   !---------------------------------------------------------------------------
   !> Counts the suite tests 
   !!
   !! @param this - this test suite object
   !! 
   !! @return number of tests in the test suite
   !---------------------------------------------------------------------------
   integer function countTests_testsuite(this)
      type (TestSuite_type), intent(in) :: this

      call countTestsSub(this, countTests_testsuite)

   end function countTests_testsuite

   !---------------------------------------------------------------------------
   !> Releases memory from test suite.  Again, this needs to be recursive.
   !!
   !! @param self - this test suite object
   !---------------------------------------------------------------------------
   Recursive subroutine clean_(self)
      use TestCase_mod,              only: clean
      use ParameterizedTestCase_mod, only: clean
#ifdef USE_MPI
      use MpiTestCase_mod, only: clean
#endif
      type (TestSuite_type) :: self

      integer :: i_test, n_tests

      n_tests = Size(self%tests)
      do i_test = 1, n_tests
         call clean_generic(self%tests(i_test))
      end do
      deallocate(self%tests)

   contains

      !------------------------------------------------------------------------
      !> Clears up the memory of generic test. 
      !!
      !! @param self - this generic test object
      !------------------------------------------------------------------------
      subroutine clean_generic(gentest)
         type (GenericTest_type) :: gentest

         select case (gentest%id)
         case (ID_TESTCASE)
            call Clean(gentest%tcase)
            deallocate(gentest%tcase)
         case (ID_TESTSUITE)
            call Clean(gentest%tsuite)
            deallocate(gentest%tsuite)
         case (ID_PARAMTEST)
            call Clean(gentest%ptest)
            deallocate(gentest%ptest)
#ifdef USE_MPI
         case (ID_NEW_MPITEST)
            call Clean(gentest % newmpitest)
            deallocate(gentest % newmpitest)
#endif
         end select
      end subroutine clean_generic

   end subroutine clean_

end module TestSuite_mod
