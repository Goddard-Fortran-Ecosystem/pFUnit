program main
   use pfunit_mod
   use ParallelContext_mod
   implicit none
#ifdef USE_MPI
   include 'mpif.h'
#endif

   type (TestSuite) :: all
   class(BaseTestRunner), allocatable :: runner

   integer :: i
   character(len=:), allocatable :: executable
   character(len=:), allocatable :: argument
   integer :: length

   logical :: useRobustRunner
   logical :: useSubsetRunner
   integer :: numSkip
   logical :: useMpi

   class (ParallelContext), allocatable :: context

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
#ifdef BUILD_ROBUST
         useRobustRunner = .true.
#else
         ! TODO: This should be a failing test.
         write (*,*) 'Robust runner not built.'
         useRobustRunner = .false.
#endif
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

#ifdef USE_MPI
      useMpi = .true.
#else
      useMpi = .false.
#endif

   if (useRobustRunner) then
      useMpi = .false. ! override build
#ifdef BUILD_ROBUST
#ifdef USE_MPI
      allocate(runner, source=RobustRunner('mpirun -np 4 ' // executable))
#else
      allocate(runner, source=RobustRunner(executable))
#endif
#else
      ! TODO: This should be a failing test.
      write (*,*) 'Robust runner not built.'
#endif
   else if (useSubsetRunner) then
      allocate(runner, source=SubsetRunner(numSkip=numSkip))
   else
      allocate(runner, source=newTestRunner())
   end if

   all = getTestSuites()
   call getContext(context, useMpi)

   call runner%run(all, context)

   call finalize()

contains

   subroutine getContext(context, useMpi)
      class (ParallelContext), allocatable :: context
      logical, intent(in) :: useMpi

#ifdef USE_MPI
      if (useMpi) then
         allocate(context, source=newMpiContext())
         return
      end if
#endif

      allocate(context, source=newSerialContext())

   end subroutine getContext

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


