program main
   use, intrinsic :: iso_fortran_env
   use FUnit, only: initialize, finalize, stub
   use FUnit, only: SubsetRunner
   use FUnit, only: TestSuite
   use FUnit, only: ParallelContext
   use FUnit, only: SerialContext, newSerialContext
   use robustTestSuite
   implicit none

!!$   call initialize(useMPI=.false.)
   call initialize(stub)
   call runTests()
   call finalize(stub, .true.) ! let top level driver indicate success/failure

contains

   subroutine runTests()
      use FUnit, only: TestResult
      type (SubsetRunner) :: runner
      type (TestSuite) :: s
      class (ParallelContext), allocatable :: context
      character(:),allocatable :: skipString
      integer :: numSkip, strLength
      integer :: skipArg

      type (TestResult) :: result

#ifdef USE_MPI
      skipArg = 2
#else
      skipArg = 2
#endif

      call get_command_argument(skipArg, length = strLength)
      allocate(character(len=strLength) :: skipString)
      call get_command_argument(skipArg, value=skipString)
      read (skipString,*)numSkip

      runner = SubsetRunner(numSkip, OUTPUT_UNIT)
      allocate(context, source=newSerialContext())
      s = suite()

      result = runner%run(s, context)

   end subroutine runTests

end program main
