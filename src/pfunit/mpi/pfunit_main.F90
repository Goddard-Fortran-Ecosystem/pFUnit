!-----------------------------------------------------------------------------
! This procedure cannot be inside of a module.  The particular version
! of this procedure that is used depends upon how the application is linked.
! The one in this file is for parallel execution and uses requires pFUnit.
!-----------------------------------------------------------------------------

subroutine funit_main(load_tests, extra_initialize, extra_finalize)
   use pFUnit
   implicit none
   procedure(LoadTests_interface) :: load_tests
   procedure() :: extra_initialize, extra_finalize

   logical :: status ! .true. if no test failures/errors

   call initialize(extra_initialize)
   status = run(load_tests)
   call finalize(extra_finalize, status)
   
end subroutine funit_main

!!$subroutine pfunit_main(load_tests, extra_initialize, extra_finalize)
!!$   use FUnit
!!$   implicit none
!!$
!!$   procedure(), pointer :: extra_initialize, extra_finalize
!!$
!!$   interface
!!$      subroutine load_tests(suite)
!!$         use FUnit
!!$         type (TestSuite), intent(out) :: suite
!!$      end subroutine load_tests
!!$   end interface
!!$
!!$   type (StringUnlimitedMap) :: option_values
!!$   type (TestSuite) :: all_test_suites
!!$
!!$   print*,__FILE__,__LINE__
!!$   option_values = parse()
!!$   call load_tests(all_test_suites)
!!$   call main_sub(all_test_suites, option_values, extra_initialize, extra_finalize)
!!$
!!$contains
!!$
!!$
!!$   function parse() result(option_values)
!!$      type (StringUnlimitedMap) :: option_values
!!$
!!$      ! For processing command line arguments
!!$      type (ArgParser) :: parser
!!$
!!$      call parser%add_option('-h', '--help', action='store_true', &
!!$           & help='provide more information about tests as they run')
!!$      call parser%add_option('-v', '--verbose', action='store_true', dest='verbose', &
!!$           & help='provide more information about tests as they run')
!!$      call parser%add_option('-d', '--debug', action='storet_true', dest='verbose', &
!!$           & help='provide more information about tests as they run')
!!$      call parser%add_option('-o', '--output', &
!!$           & help='Send console output to separate file rather than OUTPUT_UNIT')
!!$      call parser%add_option('--robust', action='store_true', &
!!$           & help='Uses separate execution to support tests that hang or crash.')
!!$      call parser%add_option('--max-timeout-duration', type='real', &
!!$           & help='Used with robust runner to set a default max time _per_ test.')
!!$      call parser%add_option('--max-launch-duration', type='real', &
!!$           & help='Used with robust runner to set a default max time for launch of separate executable.')
!!$      call parser%add_option('--skip', type='integer', &
!!$           & help='Used with robust runner to specify where to (re) start test sequence.')
!!$      call parser%add_option('--xml', action='store_true', &
!!$           & help='use XML printer')
!!$      option_values = parser%parse_args()
!!$
!!$      if (associated(option_values%at('help'))) then
!!$         call parser%print_help()
!!$         stop 'stopping'
!!$      end if
!!$
!!$   end function parse
!!$
!!$end subroutine pfunit_main
!!$
!!$subroutine main_sub(suite, option_values, extra_initialize, extra_finalize)
!!$   use iso_fortran_env, only: OUTPUT_UNIT
!!$   implicit none
!!$   type (TestSuite), intent(inout) :: suite
!!$   type (StringUnlimitedMap), intent(in) :: option_values
!!$   procedure(), pointer :: extra_initialize, extra_finalize
!!$
!!$   class(BaseTestRunner), allocatable :: runner
!!$
!!$   integer :: i
!!$   character(len=:), allocatable :: executable
!!$   character(len=:), allocatable :: argument
!!$
!!$   real :: maxTimeoutDuration
!!$   real :: maxLaunchDuration
!!$
!!$   logical :: useRobustRunner
!!$   logical :: useRemoteRunner
!!$   logical :: printXmlFile
!!$   integer :: numSkip
!!$   logical :: useMpi
!!$   ! Regular Output
!!$   logical :: debug = .false. ! override with -d
!!$   integer :: outputUnit ! override with -o <filename>
!!$   character(len=:), allocatable :: outputFile
!!$   ! XML Additions
!!$   character(len=:), allocatable :: xmlFileName
!!$   integer :: iostat
!!$   integer :: xmlFileUnit
!!$   logical :: xmlFileOpened
!!$   integer :: numListeners, iListener
!!$   class (ListenerPointer), allocatable :: listeners(:)
!!$   type (DebugListener) :: debugger
!!$   character(len=:), allocatable :: suiteName
!!$   character(len=:), allocatable :: fullExecutable
!!$
!!$   ! Support for the runs
!!$   class (ParallelContext), allocatable :: context
!!$   type (TestResult) :: result
!!$
!!$   ! Initialize variables...
!!$
!!$   maxTimeoutDuration = 5.00 ! seconds
!!$   maxLaunchDuration  = 5.00 ! seconds
!!$
!!$   useRobustRunner = .false.
!!$   useRemoteRunner = .false.
!!$   printXmlFile = .false.
!!$   numSkip = 0
!!$   numListeners = 1; iListener = 0
!!$
!!$   executable = getCommandLineArgument(0)
!!$
!!$   outputUnit = OUTPUT_UNIT ! stdout unless modified below
!!$
!!$   suiteName = 'default_suite_name'
!!$
!!$   if (option_values%count('verbose') /= 0) then
!!$      debug = .true.
!!$      numListeners = numListeners + 1
!!$   end if
!!$
!!$   if (option_values%count('output') /= 0) then
!!$      outputFile = to_string(option_values%at('output'))
!!$      open(file=outputFile, newUnit=outputUnit, form='formatted', &
!!$           & status='unknown', access='sequential')
!!$   end if
!!$
!!$   if (option_values%count('robust') /= 0) then
!!$      useRobustRunner = .true.
!!$   end if
!!$
!!$   if (option_values%count('max-timeout-duration') /= 0) then
!!$      maxTimeoutDuration = to_real(option_values%at('max-timeout-duration'))
!!$   end if
!!$
!!$   if (option_values%count('max-launch-duration') /= 0) then
!!$      maxLaunchDuration = to_real(option_values%at('max-launch-duration'))
!!$   end if
!!$
!!$   if (option_values%count('skip') /= 0) then
!!$      useRemoteRunner = .true.
!!$      numSkip = to_integer(option_values%at('skip'))
!!$   end if
!!$
!!$   if (option_values%count('xml') /= 0) then
!!$      xmlFileName = to_string(option_values%count('xml'))
!!$      open(newUnit=xmlFileUnit, file=xmlFileName,  form='formatted', &
!!$           & status='unknown', access='sequential', iostat=iostat)
!!$      if(iostat /= 0) then
!!$         write(*,*) 'Could not open XML file ', xmlFileName, &
!!$              ', error: ', iostat
!!$      else
!!$         printXmlFile = .true.
!!$         numListeners = numListeners + 1
!!$      end if
!!$   end if
!!$
!!$   if (option_values%count('name') /= 0) then
!!$      suiteName = to_string(option_values%at('name'))
!!$   end if
!!$
!!$
!!$   ! Allocate and fill listeners array.
!!$   allocate(listeners(numListeners))
!!$   ! Default listener
!!$   iListener = iListener + 1
!!$   allocate(listeners(iListener)%pListener, source=newResultPrinter(outputUnit))
!!$   ! XML listener
!!$   if(printXmlFile) then
!!$      iListener = iListener + 1
!!$      allocate(listeners(iListener)%pListener, source=newXmlPrinter(xmlFileUnit))
!!$   end if
!!$   ! Debugger
!!$   if(debug) then
!!$      iListener = iListener + 1
!!$      debugger=DebugListener(outputUnit)
!!$      allocate(listeners(iListener)%pListener, source=debugger)
!!$   end if
!!$
!!$   ! Initialize should be called on the second timethrough.
!!$
!!$   ! useMPI optional argument has no effect if not USE_MPI.
!!$   if (useRobustRunner) then
!!$   else
!!$      call initialize()
!!$   end if
!!$
!!$   !-------------------------------------------------------------------------
!!$   ! Some users may have 1-time only non-reentrant libraries that must
!!$   ! be initialized prior to executing their tests.  The motivating example
!!$   ! here is the Earth System Modeling Framework.  Rather than customize
!!$   ! this driver to each case as it arises, we are leaving it to users
!!$   ! to write a single init routine that is invoked here.
!!$   !-------------------------------------------------------------------------
!!$   if (associated(extra_initialize)) call extra_initialize()
!!$
!!$   if (useRobustRunner) then
!!$      useMpi = .false. ! override build
!!$
!!$      if (useMpi) then
!!$         fullExecutable = 'mpirun -np 4 ' // executable
!!$      else
!!$         fullExecutable = executable
!!$      end if
!!$      !      allocate(runner, source=RobustRunner(fullExecutable, listeners))
!!$      allocate(runner, &
!!$           & source=RobustRunner( &
!!$           &    fullExecutable, &
!!$           &    listeners, &
!!$           &    maxLaunchDuration=maxLaunchDuration, &
!!$           &    maxTimeoutDuration=maxTimeoutDuration ))
!!$   else if (useRemoteRunner) then
!!$      allocate(runner, source=RemoteRunner(numSkip=numSkip))
!!$   else
!!$      allocate(runner, source=newTestRunner(listeners))
!!$   end if
!!$
!!$   call suite%setName(suiteName)
!!$
!!$   context = get_context(useMpi)
!!$
!!$   result = runner%run(suite, context)
!!$
!!$   if (outputUnit /= OUTPUT_UNIT) then
!!$      close(outputUnit)
!!$   end if
!!$
!!$   if(printXmlFile) then
!!$      inquire(unit=xmlFileUnit, opened=xmlFileOpened)
!!$      if(xmlFileOpened) then
!!$         close(xmlFileUnit)
!!$      end if
!!$   end if
!!$
!!$   if (associated(extra_finalize)) call extra_finalize()
!!$   call finalize(result%wasSuccessful())
!!$
!!$contains
!!$
!!$   function getCommandLineArgument(i) result(argument)
!!$      integer, intent(in) :: i
!!$      character(:), allocatable :: argument
!!$
!!$      integer :: length
!!$
!!$      call get_command_argument(i, length=length)
!!$      allocate(character(len=length) :: argument)
!!$      call get_command_argument(i, value=argument)
!!$
!!$   end function getCommandLineArgument
!!$
!!$
!!$end subroutine main_sub

