program main
   use, intrinsic :: iso_fortran_env
   use pFUnit_mod, only: initialize, finalize
   use pFUnit_mod, only: SubsetRunner
   use pFUnit_mod, only: TestSuite
   use pFUnit_mod, only: ParallelContext
   use pFUnit_mod, only: SerialContext, newSerialContext
   use robustTestSuite_mod
   implicit none

   call initialize(useMPI=.false.)
   call runTests()
   call finalize()

contains

   subroutine runTests()
      type (SubsetRunner) :: runner
      type (TestSuite) :: s
      class (ParallelContext), allocatable :: context
      character(:),allocatable :: skipString
      integer :: numSkip, strLength
      integer :: skipArg
      character(len=100) :: command

#ifdef USE_MPI
      skipArg = 2
#else
      skipArg = 2
#endif

      call get_command(command)
      call get_command_argument(skipArg, length = strLength)
      allocate(character(len=strLength) :: skipString)
      call get_command_argument(skipArg, value=skipString)
      read (skipString,*)numSkip

      runner = SubsetRunner(numSkip, OUTPUT_UNIT)
      allocate(context, source=newSerialContext())
      s = suite()

      call runner%run(s, context)
      
   end subroutine runTests

end program main
