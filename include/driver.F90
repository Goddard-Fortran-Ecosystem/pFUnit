program main
   use pfunit_mod
   use ParallelContext_mod
   use AbstractPrinter_mod
   use iso_fortran_env, only: OUTPUT_UNIT
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
   character(len=:), allocatable :: xmlFileName
   integer :: iostat
   integer :: xmlFileUnit
   logical :: xmlFileOpened
   class (PrinterPointer), allocatable :: printers(:)

   class (ParallelContext), allocatable :: context

   useRobustRunner = .false.
   useSubsetRunner = .false.
   printXmlFile = .false.
   numSkip = 0

   call get_command_argument(0, length=length)
   allocate(character(len=length) :: executable)
   allocate(character(len=length+30) :: fullExecutable)
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
      case ('-xml')
         i = i + 1
         call get_command_argument(i, length=length)
         allocate(character(len=length) :: xmlFileName)
         call get_command_argument(i, value=xmlFileName)
         printXmlFile = .true.
      end select
      deallocate(argument)
   end do

   if(printXmlFile) then
      xmlFileUnit = newunit()
      open(unit=xmlFileUnit, file=xmlFileName, iostat=iostat)
      if(iostat /= 0) then
         write(*,*) 'Could not open XML file ', xmlFileName, &
              ', error: ', iostat
         allocate(printers(1))
      else
         allocate(printers(2))
         allocate(printers(2)%pPrinter, source=newXmlPrinter(xmlFileUnit))
      end if
   else
      allocate(printers(1))
   end if
   allocate(printers(1)%pPrinter, source=newResultPrinter(OUTPUT_UNIT))

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
      
      allocate(runner, source=RobustRunner(fullExecutable, printers))
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

   inquire(unit=xmlFileUnit, opened=xmlFileOpened)
   if(printXmlFile .and. xmlFileOpened) then
      close(xmlFileUnit)
   end if

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

   integer function newunit(unit)
     integer, intent(out), optional :: unit
     integer, parameter :: LUN_MIN=10, LUN_MAX=1000
     logical :: opened
     integer :: lun
     newunit=-1
     do lun=LUN_MIN,LUN_MAX
       inquire(unit=lun,opened=opened)
       if (.not. opened) then
         newunit=lun
         exit
       end if
     end do
     if (present(unit)) unit=newunit
   end function newunit

end program main


