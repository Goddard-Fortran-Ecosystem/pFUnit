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
      use gFTL2_StringVector, only: gFTL2_SV => StringVector
      use pf_Test
      use pf_TestVector
      use pf_GlobFilter
#ifndef _WIN32
      use pf_RegexFilter
#endif
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



      ! Load all tests first
      unfiltered = TestSuite()
      call load_tests(unfiltered)

      ! Apply include filters (-f)
      option => options%at('filter')
      if (associated(option)) then
         call apply_include_filters(option, unfiltered, suite, unit)
      else
         suite = unfiltered
      end if

      ! Apply exclude filters (-e)
      option => options%at('exclude')
      if (associated(option)) then
         call apply_exclude_filters(option, suite, unit)
      end if

      r = runner%run(suite, context)
      status = r%wasSuccessful()

   contains


      logical function is_regex_pattern(pattern)
         character(*), intent(in) :: pattern
         ! Check for regex metacharacters that don't exist in glob
         is_regex_pattern = index(pattern, '^') > 0 .or. &
                            index(pattern, '$') > 0 .or. &
                            index(pattern, '[') > 0 .or. &
                            index(pattern, ']') > 0 .or. &
                            index(pattern, '(') > 0 .or. &
                            index(pattern, ')') > 0 .or. &
                            index(pattern, '|') > 0 .or. &
                            index(pattern, '+') > 0 .or. &
                            index(pattern, '{') > 0 .or. &
                            index(pattern, '}') > 0
      end function is_regex_pattern


       subroutine apply_include_filters(option, unfiltered, suite, unit)
          class(*), pointer :: option
          type(TestSuite), intent(in) :: unfiltered
          type(TestSuite), intent(inout) :: suite
          integer, intent(in) :: unit

          type(gFTL2_SV) :: patterns
          type(TestSuite) :: temp_suite
          character(:), allocatable :: pattern
          integer :: i

          ! Check if patterns are provided
          select type (option)
          type is (character(*))
             ! Single pattern as string - convert to vector with one element
             patterns = gFTL2_SV()
             call patterns%push_back(option)
          class is (gFTL2_SV)
             patterns = option
          class default
             ! Try to cast
             call cast(option, patterns)
          end select

          if (patterns%size() == 0) then
            ! No patterns specified, include all tests
            suite = unfiltered
            return
         end if

         ! Initialize empty suite
         suite = TestSuite()

          ! OR logic: include if matches ANY pattern
          do i = 1, patterns%size()
             pattern = trim(adjustl(patterns%at(i)))
             temp_suite = TestSuite()

#ifndef _WIN32
             ! Unix: use regex
             call unfiltered%filter_sub(RegexFilter(pattern), temp_suite)
#else
            ! Windows: use glob, warn if regex syntax detected
            if (is_regex_pattern(pattern)) then
               write(unit, '(a)') 'WARNING: Regex syntax detected but not supported on Windows.'
               write(unit, '(a)') '         Using glob pattern matching instead.'
            end if
            call unfiltered%filter_sub(GlobFilter(pattern), temp_suite)
#endif

            ! Merge temp_suite into suite (union)
            call merge_suites(suite, temp_suite)
         end do
      end subroutine apply_include_filters


      subroutine apply_exclude_filters(option, suite, unit)
         class(*), pointer :: option
         type(TestSuite), intent(inout) :: suite
         integer, intent(in) :: unit

         type(gFTL2_SV) :: patterns
         type(TestSuite) :: excluded
         character(:), allocatable :: pattern
         integer :: i

          ! Check if patterns are provided
          select type (option)
          type is (character(*))
             ! Single pattern as string
             patterns = gFTL2_SV()
             call patterns%push_back(option)
             write(unit,'(a,i0,a)') 'DEBUG EXCLUDE: Got single pattern, size=', patterns%size()
          class is (gFTL2_SV)
             patterns = option
          class default
             call cast(option, patterns)
          end select

          if (patterns%size() == 0) then
             ! No exclusions
             return
          end if

          ! OR logic: exclude if matches ANY pattern
          do i = 1, patterns%size()
             pattern = trim(adjustl(patterns%at(i)))
             excluded = TestSuite()

             ! Always use glob for exclude (simpler, works everywhere)
             call suite%filter_sub(GlobFilter(pattern), excluded)

             ! Remove matches from suite
             call subtract_suite(suite, excluded)
          end do
      end subroutine apply_exclude_filters


      subroutine merge_suites(target_suite, source_suite)
         use PF_Test, only: Test
         use PF_TestVector, only: TestVector
         type(TestSuite), intent(inout) :: target_suite
         type(TestSuite), intent(in) :: source_suite

         type(TestVector) :: test_list
         class(Test), pointer :: t
         integer :: i

         ! Get all test cases from source suite
         call source_suite%getTestCases(test_list)

         ! Add each test to target suite if not already present
         do i = 1, test_list%size()
            t => test_list%at(i)
            if (.not. suite_contains_test(target_suite, t)) then
               call target_suite%tests%push_back(t)
            end if
         end do
      end subroutine merge_suites


      logical function suite_contains_test(suite, aTest) result(contains)
         use PF_Test, only: Test
         use PF_TestVector, only: TestVector
         type(TestSuite), intent(in) :: suite
         class(Test), pointer, intent(in) :: aTest

         type(TestVector) :: test_list
         class(Test), pointer :: t
         integer :: i

         contains = .false.

         call suite%getTestCases(test_list)
         do i = 1, test_list%size()
            t => test_list%at(i)
            if (t%getName() == aTest%getName()) then
               contains = .true.
               return
            end if
         end do
      end function suite_contains_test


       subroutine subtract_suite(target_suite, exclude_suite)
          use PF_Test, only: Test
          use PF_TestVector, only: TestVector
          type(TestSuite), intent(inout) :: target_suite
          type(TestSuite), intent(in) :: exclude_suite

          type(TestVector) :: exclude_list, new_list, target_list
          class(Test), pointer :: t
          integer :: i

          ! Get list of tests to exclude
          call exclude_suite%getTestCases(exclude_list)
          
          ! Get current tests from target
          call target_suite%getTestCases(target_list)

          ! Build new test vector without excluded tests
          new_list = TestVector()
          do i = 1, target_list%size()
             t => target_list%at(i)
             if (.not. is_in_exclude_list(t, exclude_list)) then
                call new_list%push_back(t)
             end if
          end do

          ! Replace target's test vector
          target_suite%tests = new_list
       end subroutine subtract_suite


       logical function is_in_exclude_list(aTest, exclude_list) result(excluded)
          use PF_Test, only: Test
          use PF_TestVector, only: TestVector
          class(Test), pointer, intent(in) :: aTest
          type(TestVector), intent(in) :: exclude_list

          class(Test), pointer :: t
          integer :: i

          excluded = .false.
          do i = 1, exclude_list%size()
             t => exclude_list%at(i)
             if (t%getName() == aTest%getName()) then
                excluded = .true.
                return
             end if
          end do
       end function is_in_exclude_list


      subroutine set_command_line_options()
         parser = ArgParser()
         call parser%add_argument('-d', '-v', '--debug', '--verbose', action='store_true', &
              & help='make output more verbose')

         call parser%add_argument('-f', '--filter', action='store', &
              & n_arguments='*', &
              & help='run tests matching pattern(s) (regex on Unix, glob on Windows)')

         call parser%add_argument('-e', '--exclude', action='store', &
              & n_arguments='*', &
              & help='skip tests matching glob pattern(s)')

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
