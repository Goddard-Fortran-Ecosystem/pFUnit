#include "unused_dummy.fh"
#include "pf_mpi_defines.fh"

!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: MpiTestCase
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

module PF_MpiTestCase
   use PF_MPI_MODULE
   use PF_MpiContext
   use PF_TestCase
   use PF_AbstractTestParameter
   use PF_MpiTestParameter
   use PF_ParameterizedTestCase, only: ParameterizedTestCase
   implicit none
   private

   public :: MpiTestCase

   type, abstract, extends(ParameterizedTestCase) :: MpiTestCase
      integer :: processRank
      type (MpiContext) :: context
      type (MpiContext) :: parentContext
   contains
      procedure :: countTestCases => countTestCases_mpi
      procedure :: run
      procedure :: runBare
      procedure :: getNumProcesses
      procedure :: getNumProcessesRequested
      procedure :: getProcessRank
      procedure :: getMpiCommunicator
      procedure :: getContext
   end type MpiTestCase

contains

   integer function countTestCases_mpi(this) result(countTestCases)
      class (MpiTestCase), target, intent(in) :: this
      _UNUSED_DUMMY(this)
      countTestCases = 1
   end function countTestCases_mpi

   recursive subroutine run(this, tstResult, context)
      use PF_TestResult, only: TestResult
      use PF_ParallelContext
      use PF_ExceptionList
      use PF_SurrogateTestCase
      class (MpiTestCase), target, intent(inout) :: this
      class (TestResult), intent(inout) :: tstResult
      class (ParallelContext), intent(in) :: context

      class (SurrogateTestCase), pointer :: s_ptr

      select type (context)
      type is (MpiContext)
         this%parentContext = context
      class default
         call throw('MPI test cannot run in a non-MPI context.')
         return
      end select

      s_ptr => this%getSurrogate()
      call tstResult%run(s_ptr, context)

   end subroutine run

   recursive subroutine runBare(this)
      use PF_ExceptionList
      class (MpiTestCase), intent(inout) :: this

      logical :: discard

      ! create subcommunicator
      this%context = this%parentContext%makeSubcontext(this%getNumProcessesRequested())

      if (.not. anyExceptions(this%parentContext)) then
         if (this%context%isActive()) then
            call this%setUp()

            if (.not. anyExceptions(this%context)) then
               call this%runMethod()
               call this%tearDown()
            end if
         end if
      else
         ! only report context failure on root PE
         if (.not. this%parentContext%isRootProcess()) then
            discard = catch()
            if (.false.) print*,discard ! prevent warning from compiler that discard is not used.
         end if
      end if

            call gatherExceptions(this%parentContext)
   end subroutine runBare

   function getMpiCommunicator(this) result(mpiCommunicator)
      type (PF_MPI_COMM_TYPE) :: mpiCommunicator
      class (MpiTestCase), intent(in) :: this
      mpiCommunicator = this%context%getMpiCommunicator()
   end function getMpiCommunicator

   integer function getNumProcesses(this) result(numProcesses)
      class (MpiTestCase), intent(in) :: this
      numProcesses = this%context%getNumProcesses()
   end function getNumProcesses

   integer function getNumProcessesRequested(this) result(numProcessesRequested)
      use PF_ExceptionList
      class (MpiTestCase), intent(in) :: this
      select type (p => this%testParameter)
      class is (MpiTestParameter)
         numProcessesRequested = p%getNumProcessesRequested()
      class default
         call throw('Incorrect type of test parameter in MpiTestCase::getNumProcessesRequested()')
      end select
   end function getNumProcessesRequested

   integer function getProcessRank(this) result(processRank)
      class (MpiTestCase), intent(in) :: this
      processRank = this%context%processRank()
   end function getProcessRank

   function getContext(this) result(context)
      class (MpiTestCase), intent(in) :: this
      class (MpiContext), allocatable :: context

      allocate(context, source=this%context)

   end function getContext

end module PF_MpiTestCase
