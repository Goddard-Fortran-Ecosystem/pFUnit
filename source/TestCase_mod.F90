!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: TestCase_mod
!
!> @brief
!! Implements the test case object 
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 16 Oct 2006
!!
! REVISION HISTORY:
! 16 Oct 2006 - Initial Version
! 12 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module TestCase_mod
   use Assert_mod
   use ProcedurePointer_mod
   use BaseAddress_mod
   implicit none
   private

   include 'BaseAddress.inc'

   public :: TestCase_type
   public :: TestCase   ! constructor
   public :: TestCase1Step ! constructor
   public :: TestCase1StepFixture ! constructor
   public :: addTestMethod
   public :: addTestMethodFromAddress
   public :: run
   public :: runSetUp
   public :: runTearDown
   public :: countTests
   public :: clean

   public :: MAX_LEN_TEST_NAME
   integer, parameter :: MAX_LEN_TEST_NAME = 40

   type TestMethod_type
      character(len=MAX_LEN_TEST_NAME) :: name
      type (ProcedurePointer_type)   :: handle
   end type TestMethod_type

   type TestCase_type
      private
      type (TestMethod_type), pointer :: methods(:) => null()

      type (ProcedurePointer_type), pointer :: setUp => null()
      type (ProcedurePointer_type), pointer :: tearDown => null()

      logical :: passFixture = .false.
      logical :: passMPI = .false.

   end type TestCase_type

   interface run
      module procedure runMethod
      module procedure runTestCase
   end interface

   interface clean
      module procedure clean_
   end interface

   interface TestCase
      module procedure testCase_simple
      module procedure testCase_setUp
      module procedure testCase_fixture
   end interface

   interface TestCase1Step
      module procedure testCase1StepMethod
      module procedure testCase1StepMpi
   end interface

   interface addTestMethod
      module procedure addTestMethod_procedure
   end interface

   interface countTests
      module procedure countTests_testcase
   end interface

contains

   !---------------------------------------------------------------------------
   !> Creates the default constructor with empty for the simple test case.
   !!
   !! @return the empty object of test case 
   !---------------------------------------------------------------------------
   function TestCase_simple() result(this)
      type (TestCase_type) :: this

      allocate(this%methods(0))

   end function TestCase_simple

   !---------------------------------------------------------------------------
   !> Creates the constructor with given setup and teardown methods
   !!
   !! @param setup - setup method externally 
   !! @param tearDown - tearDown method externally
   !!
   !! @return the object of test case 
   !---------------------------------------------------------------------------
   function TestCase_setUp(setUp, tearDown) result(this)
      external setUp
      external tearDown
      type (TestCase_type) :: this

      this = TestCase()

      allocate(this%setup, this%tearDown)
      this%setUp = ProcedurePointer(setUp)
      this%tearDown = ProcedurePointer(tearDown)

   end function TestCase_setUp

   !---------------------------------------------------------------------------
   !> Constructs the test case object with given setup/teardown methods and 
   !! optional fixutre
   !!
   !! @param setup - setup method externally 
   !! @param tearDown - tearDown method externally
   !! @param fixture - optional fixture 
   !!
   !! @return the object of test case 
   !---------------------------------------------------------------------------
   function TestCase_fixture(setUp, tearDown, passFixture) result(this)
      external setUp
      external tearDown
      logical, intent(in) :: passFixture
      type (TestCase_type) :: this

      this = TestCase(setUp, tearDown)
      this%passFixture = .true.

   end function TestCase_fixture

   !---------------------------------------------------------------------------
   !> Constructs the test case object with given method name and procedure.
   !!
   !! @param name - name of test case's step method
   !! @param procedure - procedure method externally
   !!
   !! @return the object of test case 
   !---------------------------------------------------------------------------
   function testCase1StepMethod(name, procedure) result(this)
      character(len=*), intent(in) :: name
      external :: procedure
      type (TestCase_type) :: this

      this = TestCase()
      call addTestMethod(this, name, procedure)

   end function testCase1StepMethod

   !---------------------------------------------------------------------------
   !> Constructs the test case object with given method name, procedure, and 
   !! optional MPI
   !!
   !! @param name - name of test case's step method
   !! @param procedure - procedure method externally
   !! @param passMPI - given optional MPI for use
   !!
   !! @return the object of test case 
   !---------------------------------------------------------------------------
   function testCase1StepMpi(name, procedure, passMPI) result(this)
      character(len=*), intent(in) :: name
      external :: procedure
      logical, intent(in) :: passMPI
      type (TestCase_type) :: this

      this = TestCase()
      call addTestMethod(this, name, procedure, passMPI)

    end function testCase1StepMpi

   !---------------------------------------------------------------------------
   !> Constructs the test case object with given setup/tearDown external 
   !! methods, method name for its fixture 
   !!
   !! @param setup - setup method externally
   !! @param tearDown - tearDown method externally
   !! @param name - name of test case's step method
   !! @param procedure - procedure method externally
   !!
   !! @return the object of test case 
   !---------------------------------------------------------------------------
   function testCase1StepFixture(setUp, tearDown, name, procedure) result(this)
      external setUp
      external tearDown
      character(len=*), intent(in) :: name
      external :: procedure
      type (TestCase_type) :: this

      this = TestCase(setUp, tearDown, passFixture = .true.)
      call addTestMethod(this, name, procedure)

   end function testCase1StepFixture

   !---------------------------------------------------------------------------
   !> Adds the method procedure to the test case object
   !!
   !! @param this - this test case object
   !! @param name - name of test case's step method
   !! @param procedure - procedure method externally
   !! @param passMPI - optional MPI for passing 
   !!
   !! @return updated test case object after adding the method procedure
   !---------------------------------------------------------------------------
   subroutine addTestMethod_procedure(this, name, procedure, passMPI)
      type (TestCase_type), intent(inout) :: this
      character(len=*),     intent(in) :: name
      external :: procedure
      logical, optional, intent(in) :: passMPI

      type (TestMethod_type), pointer :: tmpList(:)
      integer :: numMethods
      
      numMethods = size(this%methods)
      tmpList => this%methods

      allocate(this%methods(numMethods+1))
      this%methods(:numMethods) = tmpList
      deallocate(tmpList)
      
      this%methods(numMethods+1)%name = trim(name)
      this%methods(numMethods+1)%handle = ProcedurePointer(procedure)

      if (present(passMPI)) this % passMPI = passMPI

   end subroutine addTestMethod_procedure

   !---------------------------------------------------------------------------
   !> Adds the test method to the test case object from address
   !!
   !! @param this - this test case object
   !! @param name - name of test case's step method
   !! @param address - address being pointed for function
   !!
   !! @return updated test case object after adding the function pointer
   !---------------------------------------------------------------------------
   subroutine addTestMethodFromAddress(this, name, address)
      use Params_mod, only: KIND_PTR
      use ISO_C_BINDING
      type (TestCase_type), intent(inout) :: this
      character(len=*),     intent(in) :: name
      type (C_FUNPTR), intent(in) :: address
      
      type (TestMethod_type), pointer :: tmpList(:)
      integer :: numMethods
      
      numMethods = size(this%methods)
      tmpList => this%methods

      allocate(this%methods(numMethods+1))
      this%methods(:numMethods) = tmpList
      deallocate(tmpList)

      this%methods(numMethods+1)%name = trim(name)
      this%methods(numMethods+1)%handle = ProcedurePointer(address, name)

   end subroutine addTestMethodFromAddress

   !---------------------------------------------------------------------------
   !> Runs the method by the given name and optional address
   !!
   !! @param this - this test case object
   !! @param name - name of test case's step method
   !! @param address - given optional base address 
   !!
   !! @return updated test case object after running the method 
   !---------------------------------------------------------------------------
   subroutine runMethod(this, name, address)
      use BaseAddress_mod
      use pFUnitException_mod
      use TestInfo_mod
      type (TestCase_type), intent(in) :: this
      character(len=*),     intent(in) :: name
      type (BaseAddress_type), optional :: address

      integer :: iMethod

      type (TestInfo_type), pointer :: info
      type (BaseAddress_type) :: infoAddress
      allocate(info)
      infoAddress = BaseAddress(info)

      do iMethod = 1, size(this%methods)
         if (trim(name) == trim(this%methods(iMethod)%name)) exit
      end do

      if (iMethod > size(this%methods)) then ! not found
         call throw(Exception('Method '//trim(name)//' not found.'))
         return
      end if

      if (present(address)) then
         call invoke(this%methods(iMethod)%handle, address)
      else
         call invoke(this%methods(iMethod)%handle, infoAddress)
      end if

      deallocate(info)

   end subroutine runMethod

   !---------------------------------------------------------------------------
   !> Runs the setup method by the given optional address
   !!
   !! @param this - this test case object
   !! @param address - given optional base address 
   !!
   !! @return updated test case object after running the setup method 
   !---------------------------------------------------------------------------
   subroutine runSetup(this, address)
      use BaseAddress_mod
      use pFUnitException_mod
      type (TestCase_type), intent(in) :: this
      type (BaseAddress_type), optional :: address

      if (.not. associated(this%setUp)) then
         call throw(Exception('No setUp() associated with this test case.'))
         return
      end if

      if (present(address)) then
         call invoke(this%setUp, address)
      else
         call invoke(this%setUp)
      end if
      
   end subroutine runSetup

   !---------------------------------------------------------------------------
   !> Runs the tearDown method by the given optional address
   !!
   !! @param this - this test case object
   !! @param address - given optional base address 
   !!
   !! @return updated test case object after running the setup method 
   !---------------------------------------------------------------------------
   subroutine runTearDown(this, address)
      use BaseAddress_mod
      use pFUnitException_mod
      type (TestCase_type), intent(in) :: this
      type (BaseAddress_type), optional :: address

      if (.not. associated(this%tearDown)) then
         call throw(Exception('No tearDown() associated with this test case.'))
         return
      end if

      if (present(address)) then
         call invoke(this%tearDown, address)
      else
         call invoke(this%tearDown)
      end if

   end subroutine runTearDown

   !---------------------------------------------------------------------------
   !> Runs the test case for the result recursively
   !!
   !! @param this - this test case object
   !! @param address - given result object 
   !---------------------------------------------------------------------------
   recursive subroutine runTestCase(this, aResult)
      use TestResult_mod 
      use Report_mod
      use pFUnitException_mod
      use MpiServices_mod
      type (TestCase_type) :: this
      type (TestResult_type)  :: aResult

      integer :: iMethod
      type (Report_type) :: testReport

#ifdef USE_MPI 
      integer :: ier
! Only root pe participates in serial tests
      if (this % passMPI .or. amRoot()) then
#endif

      do iMethod = 1, size(this%methods)
         if (amRoot()) call testStarted(aResult, name= this % methods(iMethod) % name)
         if (this%passFixture) then
            call runTestWithFixture(this, iMethod, aResult)
         else
            call runTestNoFixture(this, iMethod, aResult)
         end if

      end do

#ifdef USE_MPI 
   end if
#endif

   contains

      !---------------------------------------------------------------------------
      !> Runs the test with no fixure recursively
      !!
      !! @param this - this test case object
      !! @param iMethod - given identifying method
      !! @param address - given test result object 
      !---------------------------------------------------------------------------
      recursive subroutine runTestNoFixture(this, iMethod, aResult)
         type (TestCase_type) :: this
         integer, intent(in)  :: iMethod
         type (TestResult_type)  :: aResult

         logical :: problem

         if (associated(this%setUp)) then
            call runSetUp(this)
            if (handleException(this%methods(iMethod)%name, 'setUp')) return
         end if

         call runMethod(this, this%methods(iMethod)%name)
         problem = handleException(this%methods(iMethod)%name)

         if (associated(this%tearDown)) then
            call runTearDown(this)
            if (handleException(this%methods(iMethod)%name, 'tearDown')) return
         end if

         if (.not. problem) then
           if (iand(mode(aResult),MODE_USE_STDOUT) /= 0) write(*,'(".")',advance='no')
         end if

      end subroutine runTestNoFixture

      !---------------------------------------------------------------------------
      !> Runs the test with fixure recursively
      !!
      !! @param this - this test case object
      !! @param iMethod - given identifying method
      !! @param address - given test result object 
      !---------------------------------------------------------------------------
      recursive subroutine runTestWithFixture(this, iMethod, aresult)
         use BaseAddress_mod
         type (TestCase_type) :: this
         integer, intent(in)  :: iMethod
         type (TestResult_type)  :: aResult

         type (BaseAddress_type) :: fixtureAddress
         integer, parameter :: FIXTURE_SIZE = 1000
         integer, pointer :: fixture(:)
         external BaseAddress
         type (BaseAddress_type) :: BaseAddress
         logical :: problem

         allocate(fixture(FIXTURE_SIZE))
         fixtureAddress = BaseAddress(fixture(1))

         if (associated(this%setUp)) then
            call runSetUp(this, fixtureAddress)
            if (handleException(this%methods(iMethod)%name, 'setUp')) then
               deallocate(fixture) 
               return
            end if
         end if

         call runMethod(this, this%methods(iMethod)%name, fixtureAddress)
         problem = handleException(this%methods(iMethod)%name)

         if (associated(this%tearDown)) then
            call runTearDown(this, fixtureAddress)
            if (handleException(this%methods(iMethod)%name, 'tearDown')) then
               deallocate(fixture)
               return
            end if
         end if

         if (.not. problem) then
           if (mode(aResult) == MODE_USE_STDOUT) write(*,'(".")',advance='no')
         end if

         deallocate(fixture)

      end subroutine runTestWithFixture

      !---------------------------------------------------------------------------
      !> Handles the exception thru the test report
      !!
      !! @param name - given name
      !! @param stage - given stage
      !!
      !! @return receiving a problem from the test report
      !---------------------------------------------------------------------------
      recursive function handleException(name, stage) result(problem)
         character(len=*), intent(in) :: name
         character(len=*), optional, intent(in) :: stage
         logical :: problem

         problem = .false. ! unless
         if (amRoot()) then
            if (catch(preserve=.true.)) then
               testReport = generateExceptionReport()
               if (present(stage)) call append(testReport, '('//trim(stage)//'() failed)')
               
               call testFailed(aResult, trim(name), testReport)
               call clean(testReport)
               call clearAll()
               problem =.true.
            end if
         end if
            
      end function handleException

   end subroutine runTestCase

   !---------------------------------------------------------------------------
   !> Counts the number of test cases
   !!
   !! @param this - this test case object 
   !!
   !! @return countTests_testcase - number of test cases
   !---------------------------------------------------------------------------
   integer function countTests_testcase(this)
      type (TestCase_type), intent(in) :: this

      countTests_testcase = size(this % methods)

   end function countTests_testcase

   !---------------------------------------------------------------------------
   !> Cleans up the test case object for deallocating it
   !!
   !! @param this - this test case object 
   !---------------------------------------------------------------------------
   subroutine clean_(this)
      type (TestCase_type), intent(inout) :: this

      deallocate(this%methods)
      if (associated(this%setUp)) then
         deallocate(this%setUp, this%tearDown)
      end if

   end subroutine clean_

end module TestCase_mod
