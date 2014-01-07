program main
   use iso_fortran_env, only: OUTPUT_UNIT
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
   integer :: numArguments
   logical :: debug = .false. ! override with -d
   integer :: outputUnit ! override with -o <filename>
   character(len=:), allocatable :: outputFile

   class (ParallelContext), allocatable :: context
   type (TestResult) :: result

   useRobustRunner = .false.
   useSubsetRunner = .false.
   numSkip = 0

   executable = getCommandLineArgument(0)

   outputUnit = OUTPUT_UNIT ! stdout unless modified below

   ! Loop over optional arguments in the command line
   numArguments = command_argument_count()
   i = 0
   do
      i = i + 1
      if (i > numArguments) exit

      argument = getCommandLineArgument(i)

      select case(argument)
      case ('-h','--help')
         call printHelpMessage()
         call finalize(successful=.true.)
      case ('-v','--verbose','-d','--debug')
         debug = .true.
      case ('-o')
         i = i + 1
         if (i > numArguments) call commandLineArgumentError()

         outputFile = getCommandLineArgument(i)
         
         open(file=outputfile, newUnit=outputUnit, form='formatted', &
              & status='unknown', access='sequential')

      case ('-robust')
#ifndef Windows
         useRobustRunner = .true.
#else
         ! TODO: This should be a failing test.
         write (*,*) 'Robust mode not supported under Windows'
         useRobustRunner = .false.
#endif
      case ('-skip')
         useSubsetRunner = .true.
         i = i + 1
         if (i > numArguments) call commandLineArgumentError()

         argument = getCommandLineArgument(i)
         read(argument,*) numSkip

      case default
         call commandLineArgumentError()
      end select

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
#ifndef Windows
#ifdef USE_MPI
      allocate(runner, source=RobustRunner('mpirun -np 4 ' // executable, outputUnit))
#else
      allocate(runner, source=RobustRunner(executable,outputUnit))
#endif
#else
      ! TODO: This should be a failing test.
      write (*,*) 'Robust mode not supported under Windows'
#endif
   else if (useSubsetRunner) then
      allocate(runner, source=SubsetRunner(numSkip=numSkip))
   else
      allocate(runner, source=newTestRunner(outputUnit))
   end if

   if (debug) call runner%setDebug()

   all = getTestSuites()
   call getContext(context, useMpi)

   result = runner%run(all, context)

   if (outputUnit /= OUTPUT_UNIT) then
      close(outputUnit)
   end if

   call finalize(result%wasSuccessful())
   stop

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


   function getCommandLineArgument(i) result(argument)
      integer, intent(in) :: i
      character(:), allocatable :: argument

      integer :: length

      call get_command_argument(i, length=length)
      allocate(character(len=length) :: argument)
      call get_command_argument(i, value=argument)

   end function getCommandLineArgument

   subroutine commandLineArgumentError()
      use iso_fortran_env, only: OUTPUT_UNIT

      write(OUTPUT_UNIT,*)'Unsupported/mismatched command line arguments.'
      write(OUTPUT_UNIT,*)' '
      call printHelpMessage()
      call finalize(successful=.false.)

   end subroutine commandLineArgumentError

   subroutine printHelpMessage()
      use iso_fortran_env, only: OUTPUT_UNIT

      write(OUTPUT_UNIT,*)'Command line arguments:'
      write(OUTPUT_UNIT,*)' '
      write(OUTPUT_UNIT,*)' Options: '
      write(OUTPUT_UNIT,*)"   '-h', '--help'    : Prints this message"
      write(OUTPUT_UNIT,*)"   '-v', '--verbose' : Logs start/stop of each test"
      write(OUTPUT_UNIT,*)"   '-d', '--debug'   : Logs start/stop of each test (same as -v)"
      write(OUTPUT_UNIT,*)"   '-o <file>'       : Diverts output to specified file"
      write(OUTPUT_UNIT,*)"   '-robust'         : (experimental) runs tests in a separate shell"
      write(OUTPUT_UNIT,*)"                       Attempts to detect/handle hangs and crashes"
      write(OUTPUT_UNIT,*)"   '-skip n'         : used by remote start with 'robust' internally"
      write(OUTPUT_UNIT,*)"                       This flag should NOT be used directly by users."
      write(OUTPUT_UNIT,*)" "

   end subroutine printHelpMessage

end program main


