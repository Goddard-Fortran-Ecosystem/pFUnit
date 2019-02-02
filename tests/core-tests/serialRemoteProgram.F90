program main
   use, intrinsic :: iso_fortran_env
   use FUnit, only: initialize, finalize
   use FUnit, only: RemoteRunner
   use FUnit, only: TestSuite
   use FUnit, only: ParallelContext
   use FUnit, only: SerialContext
   use FUnit, only: stub
   use robustTestSuite
   implicit none

!!$   call initialize(useMPI=.false.)
   call initialize(stub)
   call runTests()
   call finalize(stub, .true.) ! let top level driver indicate success/failure

contains

   subroutine runTests()
      use FUnit, only: TestResult
      type (RemoteRunner) :: runner
      type (TestSuite) :: s
      class (ParallelContext), allocatable :: context
      character(:),allocatable :: skipString
      integer :: numSkip, strLength
      integer :: skipArg

      type (TestResult) :: result
      integer :: unit
      ! TODO: make this a command line option
      character(*), parameter :: REMOTE_PROCESS_PIPE = '.remote_process_pipe'

#ifdef USE_MPI
      skipArg = 2
#else
      skipArg = 2
#endif

      call get_command_argument(skipArg, length = strLength)
      allocate(character(len=strLength) :: skipString)
      call get_command_argument(skipArg, value=skipString)
      read (skipString,*)numSkip

   open(newunit=unit,file=REMOTE_PROCESS_PIPE, &
        & action='write', status='old',form='formatted',access='sequential')
      runner = RemoteRunner(numSkip, unit)!OUTPUT_UNIT)
      allocate(context, source=SerialContext())
      s = suite()

      result = runner%run(s, context)

   end subroutine runTests

end program main
