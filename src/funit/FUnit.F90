module FUnit
   ! use these, but do not re-export:
   use FUnit_Core
   use PF_Assert
#ifndef SKIP_FHAMCREST
   use fHamcrest
#endif
   use iso_fortran_env, only: OUTPUT_UNIT
   implicit none
   ! add these

   public :: initialize
   public :: run
   public :: generic_run
   public :: finalize
   public :: LoadTests_interface
   public :: stub

contains

   subroutine initialize(extra)
      procedure(), optional :: extra  ! user-specific extra initialization steps

      if (present(extra)) call extra()
   end subroutine initialize


   logical function run(load_tests) result(status)
      procedure(LoadTests_interface) :: load_tests

      type (SerialContext) :: c

      status = generic_run(load_tests, c)

   end function run

   logical function generic_run(load_tests, context) result(status)
      use fArgParse
      use pf_StringUtilities
      use pf_AbstractPrinter
      procedure(LoadTests_interface) :: load_tests
      class(ParallelContext), intent(in) :: context

      type (TestSuite), target :: suite, unfiltered
      class(BaseTestRunner), allocatable :: runner
      type (TestResult) :: r
      type(ArgParser), target :: parser
      logical :: debug
      logical :: xml
      type (StringUnlimitedMap) :: options
      class(*), pointer :: option
      character(:), allocatable :: pattern
      integer :: unit
      integer :: n_skip
      character(:), allocatable :: ofile
      character(:), allocatable :: runner_class
      character(:), allocatable :: tap_file
      class(AbstractPrinter), allocatable :: printer

      call set_command_line_options()

      if (associated(options%at('output'))) then
         call cast(options%at('output'), ofile)
         ! Note: if run as remote, then file will be an existing named pipe.
         open(newunit=unit, file=ofile, status='unknown', form='formatted', access='sequential')
      else
         unit = OUTPUT_UNIT
      end if

      option => options%at('xml')
      if (associated(option)) then
         call cast(option, xml)
         if (xml) then
            printer = XmlPrinter(unit)
         else
            printer = ResultPrinter(unit)
         end if
      end if

      call cast(options%at('runner'),runner_class)
      select case (to_lower(runner_class))
#ifdef Robust
      case ('robust','robustrunner','robust_runner')
         allocate(runner, source=RobustRunner(printer))
      case ('remote','remoterunner','remote_runner')
         call cast(options%at('n_skip'), n_skip)
         allocate(runner, source=RemoteRunner(n_skip, unit))
#endif
      case ('default','testrunner')
         allocate(runner, source=TestRunner(printer))
      case default
         ERROR STOP 'unsupported runner'
      end select


      option => options%at('debug')
      if (associated(option)) then
         call cast(option, debug)
         if (debug) call runner%add_listener(DebugListener(unit))
      end if

      option => options%at('tap_file')
      if (associated(option)) then
         call cast(option, tap_file)
         if (tap_file /= '') then
            call runner%add_listener(TapListener(tap_file))
         end if
      end if



      option => options%at('filter')
      suite = TestSuite()
      if (associated(option)) then
         call cast(option, pattern)
         unfiltered = TestSuite()
         call load_tests(unfiltered)
         call unfiltered%filter_sub(NameFilter(pattern), suite)
      else
         call load_tests(suite)
      end if

      r = runner%run(suite, context)
      status = r%wasSuccessful()

   contains


      subroutine set_command_line_options()
         parser = ArgParser()
         call parser%add_argument('-d', '-v', '--debug', '--verbose', action='store_true', &
              & help='make output more verbose')

         call parser%add_argument('-f', '--filter', action='store', &
              & help='only run tests that match pattern')

         call parser%add_argument('-o', '--output', action='store', &
              & help='only run tests that match pattern')

         call parser%add_argument('-r', '--runner', action='store', default='TestRunner', &
              & help='use non-default runner run tests')

         call parser%add_argument('-s', '--skip', type='integer', &
              & dest='n_skip', action='store', default=0, &
              & help='skip the first n_skip tests; only used with RemoteRunner')

         call parser%add_argument('-t', '--tap', type='string', &
              & dest='tap_file', action='store', default=0, &
              & help='add a TAP listener and send results to file name')

         call parser%add_argument('-x', '--xml', action='store_true', &
              & help='print results with XmlPrinter')

#ifndef _GNU
         options = parser%parse_args()
#else
         call parser%parse_args_kludge(option_values=options)
#endif
      end subroutine set_command_line_options
   end function generic_run


   subroutine finalize(extra, successful)
#ifdef NAG
      use f90_unix_proc, only: exit
#endif
      procedure() :: extra  ! user-specific extra initialization steps
      logical, intent(in) :: successful

      call extra() ! user-specific finalize

      if (.not. successful) then
#if defined(PGI)
         call exit(-1)
#else
         write( &
            output_unit, &
            '("*** Encountered 1 or more failures/errors during testing ***")' &
         )
         stop 2
#endif
      end if

   end subroutine finalize

   function get_context(use_mpi) result(context)
      class (ParallelContext), allocatable :: context
      logical, intent(in) :: use_mpi

      if (use_mpi) then
         print*,'Cannot use MPI - need to link with pFUnit not FUnit.'
         stop 1
      end if
      context = SerialContext()
   end function get_context


   subroutine stub()
   end subroutine stub

end module FUnit
