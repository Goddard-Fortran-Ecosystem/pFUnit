module test_TestInfo_mod
   use pFUnit
   implicit none
   private
   
   public :: test_constructor
   public :: test_rank
   public :: test_subset

contains

   subroutine test_constructor()
      type (TestInfo_type) :: info

      integer :: npes, ier
      npes = numProcesses(commWorld())

      info = TestInfo(numProcs = npes) ! should succeed
      call clean(info)
      if (catch(preserve=.true.)) then
         return
      end if
      info = TestInfo(numProcs = npes + 1) ! should fail
      call clean(info)
      if (.not. catch('Insufficient processes available.')) then
         call throw(Exception('Failed to trap illegal request for additional processes.'))
      end if

   end subroutine test_constructor

   subroutine test_rank()
      type (TestInfo_type) :: info

      integer :: rank, ier

      info = TestInfo()
      rank = getRank(commWorld())
      call assertEqual(rank, processRank(info))
      call clean(info)

   end subroutine test_rank

   subroutine test_subset()
      type (TestInfo_type) :: info
      type (TestInfo_type) :: subinfo

      integer :: npes

      info = TestInfo() ! should succeed
      npes = numProcesses(info)
      subinfo = TestInfo(info, numProcs = npes - 1)
      call clean(info)

      call assertEqual(npes - 1, numProcesses(subinfo))

      if (processRank(info) < npes - 1) then
         call assertTrue(amActive(subinfo),'Process should be active.')
      else
         call assertFalse(amActive(subinfo), 'Process should be inactive.')
      end if

      call clean(subinfo)

   end subroutine test_subset

end module test_TestInfo_mod
