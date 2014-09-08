program main
   use iso_fortran_env, only: OUTPUT_UNIT
   use pfunit_mod
   implicit none
#ifdef USE_MPI
   include 'mpif.h'
#endif

   type (TestSuite) :: all
   class(BaseTestRunner), allocatable :: runner

   integer :: i
   character(len=:), allocatable :: executable
   character(len=:), allocatable :: fullExecutable
   character(len=:), allocatable :: argument
   integer :: length

   logical :: useRobustRunner
   logical :: useSubsetRunner
   logical :: printXmlFile
   integer :: numSkip
   logical :: useMpi
! Regular Output
   integer :: numArguments
   logical :: debug = .false. ! override with -d
   integer :: outputUnit ! override with -o <filename>
   character(len=:), allocatable :: outputFile
! XML Additions
   character(len=:), allocatable :: xmlFileName
   integer :: iostat
   integer :: xmlFileUnit
   logical :: xmlFileOpened
   integer :: numListeners, iListener
   class (ListenerPointer), allocatable :: listeners(:)
   type (DebugListener) :: debugger
   character(len=128) :: suiteName

! Support for the runs
   class (ParallelContext), allocatable :: context
   type (TestResult) :: result

   useRobustRunner = .false.
   useSubsetRunner = .false.
   printXmlFile = .false.
   numSkip = 0
   numListeners = 1; iListener = 0

   executable = getCommandLineArgument(0)
!   allocate(character(len=length+30) :: fullExecutable)

   outputUnit = OUTPUT_UNIT ! stdout unless modified below

   ! Loop over optional arguments in the command line
   numArguments = command_argument_count()

   suiteName = 'default_suite_name'

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
         numListeners = numListeners + 1
      case ('-o')
         i = i + 1
         if (i > numArguments) call commandLineArgumentError()

         outputFile = getCommandLineArgument(i)
         
         open(file=outputfile, newUnit=outputUnit, form='formatted', &
              & status='unknown', access='sequential')

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
         if (i > numArguments) call commandLineArgumentError()

         argument = getCommandLineArgument(i)
         read(argument,*) numSkip

      case default
         call commandLineArgumentError()

      case ('-xml')
         i = i + 1
         if (i > numArguments) call commandLineArgumentError()
         xmlFileName = getCommandLineArgument(i)
         open(newUnit=xmlFileUnit, file=xmlFileName,  form='formatted', &
              & status='unknown', access='sequential', iostat=iostat)
         if(iostat /= 0) then
            write(*,*) 'Could not open XML file ', xmlFileName, &
                 ', error: ', iostat
         else
            printXmlFile = .true.
            numListeners = numListeners + 1
         end if
      case ('-name')
         i = i + 1
         call get_command_argument(i, value=suiteName)
      end select

   end do


! Allocate and fill listeners array.
   allocate(listeners(numListeners))
! Default listener
   iListener = iListener + 1
   allocate(listeners(iListener)%pListener, source=newResultPrinter(outputUnit))
! XML listener
   if(printXmlFile) then
      iListener = iListener + 1
      allocate(listeners(iListener)%pListener, source=newXmlPrinter(xmlFileUnit))
   end if
! Debugger
   if(debug) then
      iListener = iListener + 1
      debugger=DebugListener(outputUnit)
      allocate(listeners(iListener)%pListener, source=debugger)
   end if

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
      fullExecutable = 'mpirun -np 4 ' // executable
#else
      fullExecutable = executable
#endif
      allocate(runner, source=RobustRunner(fullExecutable, listeners))
#else
      ! TODO: This should be a failing test.
      write (*,*) 'Robust runner not built.'
#endif
   else if (useSubsetRunner) then
      allocate(runner, source=SubsetRunner(numSkip=numSkip))
   else
      allocate(runner, source=newTestRunner(listeners))
   end if

   all = getTestSuites()
   call all%setName(suiteName)

   call getContext(context, useMpi)

   result = runner%run(all, context)

   if (outputUnit /= OUTPUT_UNIT) then
      close(outputUnit)
   end if

   if(printXmlFile) then
      inquire(unit=xmlFileUnit, opened=xmlFileOpened)
      if(xmlFileOpened) then
         close(xmlFileUnit)
      end if
   end if

   call finalize(result%wasSuccessful())

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


