program main
   use pfunit_mod
   implicit none
#ifdef USE_MPI
   include 'mpif.h'
#endif

   type (TestSuite) :: all
   class(BaseTestRunner), allocatable :: runner
   class (ParallelContext), allocatable :: context
   
   integer :: i
   character(len=:), allocatable :: executable
   character(len=:), allocatable :: argument
   integer :: length

   logical :: useRobustRunner
   logical :: useSubsetRunner
   integer :: numSkip

   integer :: ier, rank, npes

   useRobustRunner = .false.
   useSubsetRunner = .false.
   numSkip = 0

   call get_command_argument(0, length=length)
   allocate(character(len=length) :: executable)
   call get_command_argument(0, value=executable)

   i = 0
   do
      i = i + 1
      if (i > command_argument_count()) exit

      call get_command_argument(i, length=length)
      allocate(character(len=length) :: argument)
      call get_command_argument(i, value=argument)
      select case(argument)
      case ('-robust')
         useRobustRunner = .true.
      case ('-skip')
         useSubsetRunner = .true.
         i = i + 1
         deallocate(argument)
         call get_command_argument(i, length=length)
         allocate(character(len=length) :: argument)
         call get_command_argument(i, value=argument)
         read(argument,*) numSkip
      end select
      deallocate(argument)
   end do

   if (useRobustRunner) then
      call initialize(useMPI=.false.)
   else
      call initialize(useMPI=.true.)
   end if

   if (useRobustRunner) then
      allocate(context, source=newSerialContext())
#ifdef USE_MPI
      allocate(runner, source=RobustRunner('mpirun -np 4 ' // executable))
#else
      allocate(runner, source=RobustRunner(executable))
#endif
   else if (useSubsetRunner) then
      allocate(runner, source=SubsetRunner(numSkip=numSkip))

#ifdef USE_MPI
      allocate(context, source=newMpiContext())
#else
      allocate(context, source=newSerialContext())
#endif

   else
#ifdef USE_MPI
      allocate(context, source=newMpiContext())
#else
      allocate(context, source=newSerialContext())
#endif
      allocate(runner, source=newTestRunner())
   end if

   all = getTestSuites()

   call runner%run(all, context)

   call finalize()

contains

   function getTestSuites() result(suite)
      type (TestSuite) :: suite

#define ADD_TEST_SUITE(s) type (TestSuite), external :: s
#include "testSuites.inc"
#undef ADD_TEST_SUITE

      suite = newTestSuite()

   ! accumulate tests in top suite
#define ADD_TEST_SUITE(s) call suite%addTest(s())
#include "testSuites.inc"
#undef ADD_TEST_SUITE

   end function getTestSuites
end program main


