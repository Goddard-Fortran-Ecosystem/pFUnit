#include "unused_dummy.fh"
!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: MpiContext
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
module PF_MpiContext
   use PF_ParallelContext
   use PF_ExceptionList, only: throw
   use mpi
   implicit none
   private

   public :: MpiContext

   type, extends(ParallelContext) :: MpiContext
      private
      integer :: mpiCommunicator = MPI_COMM_NULL
      integer :: root = 0
   contains
      procedure :: isActive
      procedure :: getNumProcesses
      procedure :: processRank
      procedure :: isRootProcess
      procedure :: makeSubcontext
      procedure :: barrier
      procedure :: getMpiCommunicator
      procedure :: makeMap
      procedure :: sum_global
      procedure :: maximum
      procedure :: gatherString
      procedure :: gatherInteger
      procedure :: gatherLogical
      procedure :: allReduce
      procedure :: labelProcess

!!$      final :: clean
   end type MpiContext

   interface MpiContext
      module procedure newMpiContext_world
      module procedure newMpiContext_comm
   end interface

contains

   ! Use MPI_COMM_WORLD - avoid except in main program
   function newMpiContext_world() result(context)
      type (MpiContext) :: context
      context = MpiContext(MPI_COMM_WORLD)
   end function newMpiContext_world

   ! Make a duplicate of the communicator for internal use
   function newMpiContext_comm(communicator) result(context)
      type (MpiContext) :: context
      integer, intent(in) :: communicator
      integer :: ier

      call MPI_Comm_dup(communicator, context%mpiCommunicator, ier)
      context%root = 0

   end function newMpiContext_comm

   logical function isActive(this)
      class (MpiContext),  intent(in) :: this

      isActive = (this%mpiCommunicator /= MPI_COMM_NULL)

   end function isActive

   integer function getNumProcesses(this)
      class (MpiContext),  intent(in) :: this

      integer :: ier
      call MPI_Comm_size(this%mpiCommunicator, getNumProcesses, ier)
      if (ier /= MPI_SUCCESS) call throw('failure in MpiContext::numProcesses')

   end function getNumProcesses

   integer function processRank(this)
      class (MpiContext),  intent(in) :: this

      integer :: ier
      call MPI_Comm_rank(this%mpiCommunicator, processRank, ier)
      if (ier /= MPI_SUCCESS) call throw('failure in MpiContext::processRank')

   end function processRank

   logical function isRootProcess(this)
      class (MpiContext),  intent(in) :: this

      isRootProcess = (this%root == this%processRank())

   end function isRootProcess

   ! Returns a new context which represents just a subset of the
   ! processes in the current group.
   function makeSubcontext(this, numSubprocesses) result(subContext)
      use PF_Exception
      class (MpiContext), intent(in) :: this
      integer, intent(in) :: numSubprocesses
      type (MpiContext) :: subContext

      integer, parameter :: NUM_SUBGROUPS = 1
      integer :: ranges(3,1)
      integer :: originalGroup, newGroup
      integer :: newCommunicator
      integer :: ier
      integer npes

      if (numSubprocesses > this%getNumProcesses()) then
         call throw('Insufficient processes to run this test.')
         return
      end if
      if (numSubprocesses < 0) then
         call throw('Must specify a nonnegative number of processes for MPI test.')
         return
      end if

      call Mpi_Comm_group(this%mpiCommunicator, originalGroup, ier)

      if (numSubprocesses == 0) then
         npes = this%getNumProcesses()
      else
         npes = numSubprocesses
      end if

      ranges(:,1) = [0, npes-1, 1]

      call MPI_Group_range_incl (originalGroup, 1, ranges, newGroup, ier)
      call MPI_Comm_create(this%mpiCommunicator, newGroup, newCommunicator, ier)

      if (this%processRank() < npes) then
         subContext%mpiCommunicator = newCommunicator
         subContext%root = 0
      else
         subContext%mpiCommunicator = MPI_COMM_NULL
         subContext%root = -1
      end if


   end function makeSubcontext

   subroutine barrier(this)
      class (MpiContext), intent(in) :: this
      integer :: ier
      call Mpi_barrier(this%mpiCommunicator, ier)
      if (ier /= MPI_SUCCESS) call throw('failure in MpiContext::barrier')
   end subroutine barrier

   function getMpiCommunicator(this) result(mpiCommunicator)
      integer :: mpiCommunicator
      class (MpiContext), intent(in) :: this
      mpiCommunicator = this%mpiCommunicator
   end function getMpiCommunicator


   integer function sum_global(this, value)
      class (MpiContext), intent(in) :: this
      integer, intent(in) :: value

      integer :: ier
      integer :: tmp

      call mpi_allreduce(value, tmp, 1, MPI_INTEGER, MPI_SUM, &
           &     this%mpiCommunicator, ier)
      sum_global = tmp
      
   end function sum_global

   integer function maximum(this, value)
      class (MpiContext), intent(in) :: this
      integer, intent(in) :: value

      integer :: ier
      integer :: tmp

      call mpi_allreduce(value, tmp, 1, MPI_INTEGER, MPI_MAX, &
           &     this%mpiCommunicator, ier)
      maximum = tmp
      
   end function maximum


   subroutine makeMap(this, numEntries, counts, displacements)
      class (MpiContext), intent(in) :: this
      integer, intent(in) :: numEntries
      integer, allocatable :: counts(:)
      integer, allocatable :: displacements(:)

      integer :: p
      integer :: npes
      integer :: ier

      npes = this%getNumProcesses()

      allocate(counts(0:npes-1), displacements(0:npes-1))
      
      call Mpi_AllGather(numEntries, 1, MPI_INTEGER, counts, 1, MPI_Integer, &
           & this%mpiCommunicator, ier)

      displacements(0) = 0
      do p = 1, npes - 1
         displacements(p) = displacements(p-1) + counts(p-1)
      end do

   end subroutine makeMap

   subroutine gatherString(this, values, list)
      class (MpiContext), intent(in) :: this
      character(len=*), intent(in) :: values(:)
      character(len=*), intent(out) :: list(:)

      integer, allocatable :: counts(:), displacements(:)

      integer :: numBytes, numEntries
      integer :: ier
      integer :: i, j, jp
      character(len=1), allocatable :: sendBuffer(:)
      character(len=1), allocatable :: recvBuffer(:)
      character(len=len(list)) :: buf

      intrinsic :: sum

      numBytes = len(list)
      if (size(values) > 0) then
         numEntries = size(values) * numBytes
      else
         numEntries = 0
      end if

      call this%makeMap(numEntries, counts, displacements)

      allocate(sendBuffer(max(numEntries,1)))
      do i = 1, size(values)
         do j = 1, len(values(1))
            jp = j + (i-1)*numBytes
            sendBuffer(jp) = values(i)(j:j)
         end do
      end do

      allocate(recvBuffer(max(sum(counts),1)))

      call Mpi_GatherV( &
           & sendBuffer, numEntries, MPI_CHARACTER, &
           & recvBuffer,   counts, displacements, MPI_CHARACTER, &
           & this%root, this%mpiCommunicator, ier)

      if (this%isRootProcess()) then
         do i = 1, sum(counts)/len(list(1))
            do j = 1, numBytes
               jp = j+(i-1)*numBytes
               buf(j:j) = recvBuffer(jp)
            end do
            list(i) = trim(buf)
         end do
      end if

      deallocate(sendBuffer, recvBuffer)

      deallocate(counts, displacements)

   end subroutine gatherString

   function gatherInteger(this, values) result(global_list)
      integer, allocatable :: global_list(:)
      class (MpiContext), intent(in) :: this
      integer, intent(in) :: values(:)

      integer, allocatable :: counts(:), displacements(:)
      integer :: ier

      call this%makeMap(size(values), counts, displacements)

      allocate(global_list(sum(counts)))
      call Mpi_allGatherV( &
           & values, size(values), MPI_INTEGER, &
           & global_list,   counts, displacements, MPI_INTEGER, &
           & this%mpiCommunicator, ier)

      deallocate(counts, displacements)

   end function gatherInteger

   subroutine gatherLogical(this, values, list)
      class (MpiContext), intent(in) :: this
      logical, intent(in) :: values(:)
      logical, intent(out) :: list(:)

      integer, allocatable :: counts(:), displacements(:)
      integer :: ier
      logical, allocatable :: values_(:) ! mpi hates 0 sized arrays

      call this%makeMap(size(values), counts, displacements)

      allocate(values_(max(1, size(values))))
      values_(:size(values)) = values

      call Mpi_AllgatherV( &
           & values_, size(values), MPI_LOGICAL, &
           & list,   counts, displacements, MPI_LOGICAL, &
           & this%mpiCommunicator, ier)

      deallocate(counts, displacements)

   end subroutine gatherLogical

   function labelProcess(this, message) result(labelled_message)
      character(len=:), allocatable :: labelled_message
      class (MpiContext), intent(in) :: this
      character(*), intent(in) :: message

      integer, parameter :: MAXLEN_SUFFIX = 80
      character(len=MAXLEN_SUFFIX) :: suffix

      write(suffix,'(" (PE=",i0,")")') this%processRank()

      labelled_message = message // trim(suffix)

   end function labelProcess

   logical function allReduce(this, q) result(anyQ)
      class (MpiContext), intent(in) :: this
      logical, intent(in) :: q

      integer :: ier

      call MPI_Allreduce(q, anyQ, 1, MPI_LOGICAL, MPI_LOR, &
           & this%mpiCommunicator, ier)

   end function allReduce


   subroutine clean(this)
      type (MpiContext), intent(inout) :: this
      _UNUSED_DUMMY(this)
      
!!$      call debug(__LINE__,__FILE__)
!!$      call MPI_Comm_free(this%mpiCommunicator, ier)
!!$      call debug(__LINE__,__FILE__)
   end subroutine clean

end module PF_MpiContext
