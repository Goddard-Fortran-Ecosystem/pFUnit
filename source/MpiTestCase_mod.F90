!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE:  MpiTestCase_mod
!
!> @brief
!! Implements the object for test case using Message Passing Interface (MPI) for 
!! performing the integrated MPI tests.
!!
!! @author
!! Tom Clune, NASA/GSFC SIVO
!!
!! @date
!! 05 Apr 2007
!!
! REVISION HISTORY:
! 05 Apr 2007 - Initial Version
! 15 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module MpiTestCase_mod
   use TestCase_mod, only: MAX_LEN_TEST_NAME
   use ProcedurePointer_mod
   use Report_mod
   use pFUnitException_mod
   implicit none
   private

   public :: MpiTestCase_type
   public :: MpiTestCase
   public :: clean
   public :: run
   public :: isNewMpiDriver
   public :: countTests

   type MpiTestCase_type
      private
      type (ProcedurePointer_type) :: method
      character(len=MAX_LEN_TEST_NAME) :: name
      integer :: numProcesses

      type (ProcedurePointer_type), pointer :: setUp    => null()
      type (ProcedurePointer_type), pointer :: tearDown => null()
      type (ProcedurePointer_type), pointer :: new      => null()
      type (ProcedurePointer_type), pointer :: delete   => null()

   end type MpiTestCase_type

   interface MpiTestCase
      module procedure MpiTestCase_simple
      module procedure MpiTestCase_complex
   end interface

   interface run
      module procedure run_
   end interface
   interface countTests
      module procedure countTests_
   end interface
   interface clean
      module procedure clean_
   end interface

contains

   !---------------------------------------------------------------------------
   !> Constructs the test case using MPI with the given name, external method,
   !! and number of processes.
   !!
   !! @param name - given name of test case 
   !! @param method - given external method
   !! @param numProcesses - given number of processes
   !!
   !! @return new object of MPI test case
   !---------------------------------------------------------------------------
   function MpiTestCase_simple(name, method, numProcesses) result(test)
      character(len=*), intent(in) :: name
      external :: method
      integer, intent(in) :: numProcesses
      type (MpiTestCase_type) :: test

      test % method = ProcedurePointer(method)
      test % numProcesses = numProcesses
      test % name = name
      
   end function MpiTestCase_simple
   
   !---------------------------------------------------------------------------
   !> Constructs the test case using MPI with the given name, external method,
   !! external setup/tearDown/new/delete, and number of processes.
   !!
   !! @param name - given name of test case 
   !! @param method - given external method
   !! @param numProcesses - given number of processes
   !! @param method - given external setup  
   !! @param method - given external tearDown 
   !! @param method - given external new 
   !! @param method - given external delete 
   !!
   !! @return new object of MPI test case
   !! 
   !! @todo may need to reduce the number of arguments 
   !---------------------------------------------------------------------------
   function MpiTestCase_complex(name, method, numProcesses, setUp, tearDown, new, delete) result(test)
      character(len=*), intent(in) :: name
      external :: method
      integer, intent(in) :: numProcesses
      external :: setUp
      external :: tearDown
      external :: new
      external :: delete
      type (MpiTestCase_type) :: test

      test % method = ProcedurePointer(method)
      test % numProcesses = numProcesses
      test % name = name

      allocate(test % setUp);    test % setUp = ProcedurePointer(setUp)
      allocate(test % tearDown); test % tearDown = ProcedurePointer(tearDown)
      allocate(test % new);      test % new = ProcedurePointer(new)
      allocate(test % delete);   test % delete = ProcedurePointer(delete)
      
   end function MpiTestCase_complex
   
   !---------------------------------------------------------------------------
   !> Performs to run the MPI test case with the given test information and 
   !! test result. 
   !!
   !! @param this - this MPI test case object
   !! @param info - given test information object 
   !! @param result - given test result object
   !---------------------------------------------------------------------------
   subroutine run_(this, info, result)
      use BaseAddress_mod
      use TestResult_mod
      use TestInfo_mod
      type (MpiTestCase_type) :: this
      type (TestInfo_type), intent(in) :: info
      type (TestResult_type) :: result
      external BaseAddress
      type (BaseAddress_type) :: BaseAddress
      type (BaseAddress_type) :: infoAddress
      integer :: communicator
      type (Report_type) :: testReport

      type (TestInfo_type) :: myInfo
      type (BaseAddress_type) :: obj

      myInfo = TestInfo(info, numProcs = this % numProcesses)

      if (amActive(myInfo)) then
         if (amRoot(info)) call TestStarted(result, this % name)
         infoAddress = BaseAddress(myInfo)

         if (associated(this % new)) then ! complex case
            call invoke(this % new, BaseAddress(obj))
            call invoke(this % setUp, obj, infoAddress)
            call invoke(this % method, obj, infoAddress)
            call invoke(this % tearDown, obj, infoAddress)
            call invoke(this % delete, obj)
         else ! simple case: no setUp/tearDown
            call invoke(this % method, infoAddress)
         end if

         call gatherExceptions(mpiCommunicator(myInfo))

         if (amRoot(myInfo)) then
            if (catch(preserve=.true.)) then ! found failures
               testReport = generateExceptionReport()
               call testFailed(result, trim(this % name), testReport)
               if (mode(result) == MODE_USE_STDOUT) write(*,'("x")',advance='no')
            end if
            if (mode(result) == MODE_USE_STDOUT) write(*,'("m")',advance='no')
         end if
      end if
      
      call barrier(info)

   end subroutine run_


   !---------------------------------------------------------------------------
   !> Checks with the 'NEW_PFUNIT' environment variable by commanding the 
   !! system.
   !!
   !! @return .true. if it is found, .false. otherwise.
   !---------------------------------------------------------------------------
   logical function isNewMpiDriver()
      integer, parameter :: MAXLEN_COMMAND = 20
      character(len=MAXLEN_COMMAND) :: command, mpiServer

      call get_command(command)
      call get_environment_variable('NEW_PFUNIT', value = mpiServer)
      
      isNewMpiDriver = (index(command, trim(mpiServer)) > 0)

   end function isNewMpiDriver

   !---------------------------------------------------------------------------
   !> Counts the number of test cases.
   !!
   !! @param this - this MPI test case object
   !!
   !! @return number of tests
   !---------------------------------------------------------------------------
   integer function countTests_(this)
      type (MpiTestCase_type), intent(in) :: this
      countTests_ = 1
   end function countTests_

   !---------------------------------------------------------------------------
   !> Clears up the memory of the MPI test case object.
   !!
   !! @param this - this MPI test case object
   !---------------------------------------------------------------------------
   subroutine clean_(this)
      type (MpiTestCase_type) :: this

      if (associated(this % setUp)) deallocate(this % setUp)
      if (associated(this % tearDown)) deallocate(this % tearDown)
      if (associated(this % new)) deallocate(this % new)
      if (associated(this % delete)) deallocate(this % delete)
   end subroutine clean_
   
end module MpiTestCase_mod
