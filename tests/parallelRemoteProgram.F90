program main
   use, intrinsic :: iso_fortran_env
   use sFUnit, only: initialize, finalize
   use sFUnit, only: SubsetRunner
   use sFUnit, only: TestSuite
   use sFUnit, only: ParallelContext
   use sFUnit, only: SerialContext, newSerialContext
   use robustTestSuite_mod
   implicit none

!!$   call initialize(useMPI=.false.)
   call initialize()
   call runTests()
   call finalize(.true.) ! let top level driver indicate success/failure

contains

   subroutine runTests()
      use sFUnit, only: TestResult
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
