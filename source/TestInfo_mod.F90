!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: TestInfo
!
!> @brief
!! Implements the object for test information to support the serial and MPI 
!! builds passing their tests.  It also supports dynamic shared object (DSO).
!!
!! @author
!! Tom Clune, NASA/GSFC SIVO
!!
!! @date
!! 17 Dec 2007
!!
!> @todo If we could use OO, this would inherit from MpiServices.   Many 
!! procedures are simply pass throughs ...
!
! REVISION HISTORY:
! 17 Dec 2007 - Initial Version
! 15 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module TestInfo_mod
   use pFUnitException_mod
   use MpiServices_mod
   implicit none
   private

   public :: TestInfo_type
   public :: TestInfo
   public :: clean

   public :: query
   public :: setTestType
   public :: getTestType

   public :: numProcesses
   public :: processRank
   public :: mpiCommunicator
   public :: amActive
   public :: amRoot
   public :: barrier

   integer, public, parameter :: UNDEFINED = -1
   integer, public, parameter :: SERIAL    = 1
   integer, public, parameter :: PARALLEL  = 2

   type TestInfo_type
      private
      integer :: testType = UNDEFINED
      integer :: mpiCommunicator
      integer :: numProcesses
      integer :: processRank
   end type TestInfo_type

   interface TestInfo
      module procedure TestInfo_global
      module procedure TestInfo_subset
   end interface

   interface amRoot
      module procedure amRoot_info
   end interface

   interface barrier
      module procedure barrier_info
   end interface

   interface numProcesses
      module procedure numProcessesInfo
   end interface

   interface processRank
      module procedure processRankInfo
   end interface

   interface mpiCommunicator
      module procedure mpiCommunicator_
   end interface

   interface clean
      module procedure clean_
   end interface

   integer, parameter :: NOT_IN_ACTIVE_SUBSET = -1

contains

   !---------------------------------------------------------------------------
   !> Creates the contructor for test information with the given optional
   !! number of processes.  This is for global.
   !!
   !! @param numProcs - given number of processes
   !!
   !! @return new object of test information 
   !---------------------------------------------------------------------------
   function TestInfo_global(numProcs) result (this)
      integer, optional, intent(in) :: numProcs
      type (TestInfo_type) :: this

      integer :: ier
      integer :: numProcs_
      
      this % mpiCommunicator = commWorld()
      numProcs_ = numProcesses(this % mpiCommunicator)
      if (present(numProcs)) then
         if (numProcs > numProcs_) then
            call throw(Exception('Insufficient processes available.'))
            return
         end if
         numProcs_ = numProcs
      end if

      this % numProcesses = numProcs_
      this % processRank = getRank(this % mpiCommunicator)

   end function TestInfo_global

   !---------------------------------------------------------------------------
   !> Creates the contructor for test information with the given object of 
   !! test information and optional number of processes.  This is for subset.
   !!
   !! @param info - given object of test information
   !! @param numProcs - given number of processes
   !!
   !! @return new object of test information 
   !---------------------------------------------------------------------------
   function TestInfo_subset(info, numProcs) result (this)
      type (TestInfo_type), intent(in) :: info
      integer, optional, intent(in) :: numProcs
      type (TestInfo_type) :: this
      integer :: group, subGroup
      integer :: triplet(3)
      integer :: ier

#ifndef USE_MPI
      if (numProcs /= 1) call throw(Exception('Only one process available in serial mode.'))
#endif

      if (numProcs == numProcesses(info)) then
         this = info
         return
      else if (numProcs > numProcesses(info)) then
         call throw(Exception('Insufficient processes available.'))
      end if

#ifdef USE_MPI      
      call mpi_comm_group(info % mpiCommunicator, group, ier)
      triplet = (/ 0, numProcs - 1, 1 /)
      call mpi_group_range_incl(group, 1, triplet, subGroup, ier)
      call MPI_Comm_create(info % mpiCommunicator, subGroup, this % mpiCommunicator, ier)
      
      this % numProcesses = numProcs
      if (processRank(info) < numProcs) then
         this % processRank = info % processRank
      else
         this % processRank = NOT_IN_ACTIVE_SUBSET
      end if
#endif

   end function TestInfo_subset

   !---------------------------------------------------------------------------
   !> Gets the number of processes from the current object of test information.
   !!
   !! @param  this - this test information object
   !!
   !! @return number of processes
   !---------------------------------------------------------------------------
   integer function numProcessesInfo(this)
      type (TestInfo_type), intent(in) :: this
      numProcessesInfo = this % numProcesses
   end function numProcessesInfo

   !---------------------------------------------------------------------------
   !> Gets the rank of the process from the current object of test information.
   !!
   !! @param  this - this test information object
   !!
   !! @return rank of the process
   !---------------------------------------------------------------------------
   integer function processRankInfo(this)
     type (TestInfo_type), intent(in) :: this
     processRankInfo = this % processRank
   end function processRankInfo

   !---------------------------------------------------------------------------
   !> Checks if the rank of the process is active for subset
   !!
   !! @param  this - this test information object
   !!
   !! @return .true. if rank of the process is active, .false. otherwise.
   !---------------------------------------------------------------------------
   logical function amActive(this)
      type (TestInfo_type), intent(in) :: this
      amActive = (processRank(this) /= NOT_IN_ACTIVE_SUBSET)
   end function amActive

   !---------------------------------------------------------------------------
   !> Gets the root information from the current object of test information 
   !! thru the MPI communicator.
   !!
   !! @param  this - this test information object
   !!
   !! @return the root information 
   !---------------------------------------------------------------------------
   logical function amRoot_info(this)
      type (TestInfo_type), intent(in) :: this
      integer, parameter :: ROOT = 0

      amRoot_info = amRoot(mpiCommunicator(this))
   end function amRoot_info

   !---------------------------------------------------------------------------
   !> Performs to call the barrier for the test information by the specific MPI  
   !! communicator.
   !!
   !! @param  this - this test information object
   !---------------------------------------------------------------------------
   subroutine barrier_info(this)
      type (TestInfo_type), intent(in) :: this
      call barrier( this % mpiCommunicator )
   end subroutine barrier_info

   !---------------------------------------------------------------------------
   !> Gets the type of MPI communicator from the object of test information.
   !!
   !! @param  this - this test information object
   !!
   !! @return type of MPI communicator
   !---------------------------------------------------------------------------
   integer function mpiCommunicator_(this)
      type (TestInfo_type), intent(in) :: this
      mpiCommunicator_ = this % mpiCommunicator
    end function mpiCommunicator_

   !---------------------------------------------------------------------------
   !> Clears up the memory of the test information object.
   !!
   !! @param  this - this test information object
   !---------------------------------------------------------------------------
   subroutine clean_(this)
      type (TestInfo_type) :: this
   end subroutine clean_

   !---------------------------------------------------------------------------
   !> Performs to initialize the query for the test information object.
   !!
   !! @param  this - this test information object
   !!
   !! @return .true. if the query is initialized, .false. otherwise.
   !---------------------------------------------------------------------------
   logical function query(this)
      type (TestInfo_type) :: this

      query = .not. isInitialized(this)

   end function query

   !---------------------------------------------------------------------------
   !> Sets the test type to the test information object.
   !!
   !! @param this - this test information object
   !! @param testType - given test type
   !!
   !! @throw Exception when not supporting the specific test type
   !!
   !! @return .true. if the query is initialized, .false. otherwise.
   !---------------------------------------------------------------------------
   subroutine setTestType(this, testType)
      use pFUnitException_mod
      type (TestInfo_type) :: this
      integer, intent(in) :: testType
      
      select case (testType)
      case (SERIAL, PARALLEL)
         this % testType = testType
      case default
         call throw(Exception('unsupported test Type in TestInfo_mod'))
      end select
   end subroutine setTestType

   !---------------------------------------------------------------------------
   !> Gets the test type from the test information object.
   !!
   !! @param this - this test information object
   !!
   !! @return .true. if the query is initialized, .false. otherwise.
   !---------------------------------------------------------------------------
   integer function getTestType(this)
      type (TestInfo_type) :: this
      getTestType = this % testType
   end function getTestType

   !---------------------------------------------------------------------------
   !> Checks if the test type has any defined for its initialization.
   !!
   !! @param this - this test information object
   !!
   !! @return .true. if it is initialized, .false. otherwise.
   !---------------------------------------------------------------------------
   logical function isInitialized(this)
      type (TestInfo_type), intent(in) :: this
      
      isInitialized = (this % testType /= UNDEFINED)
   end function isInitialized

end module TestInfo_mod
