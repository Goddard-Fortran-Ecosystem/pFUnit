#include "reflection.h"
module test_MpiTestCase_mod
  use pFUnit
  use MpiWasRun_mod
  implicit none
  private

  public :: test_MPI_HelloWorld1
  public :: test_MPI_HelloWorld4
  public :: test_Broken_MPI_Method
  public :: test_MPI_setup_teardown
  public :: test_MPI_Suite
  public :: test_MPI_CountTests

contains

   subroutine test_MPI_HelloWorld1()
      call testHelloWorld(1)
   end subroutine test_MPI_HelloWorld1

   subroutine test_MPI_HelloWorld4()
      call testHelloWorld(4)
   end subroutine test_MPI_HelloWorld4

   subroutine testHelloWorld(npes)
      use IO_Utilities_mod
      use MpiServices_mod
      use TestInfo_mod
      integer, intent(in) :: npes
      type (MpiTestCase_type) :: test
      type (TestResult_type) :: result

    integer             :: p
    integer :: unit, stat
    character(len=MAX_LEN_MSG) :: msg

    test = MpiTestCase(REFLECT(HelloWorld), numProcesses=npes)
    
    result = newTestResult()
    call Run(test, TestInfo(), result) ! should launch an MPI job and fill test result

    if (amRoot()) then

       call assertEqual(1, numRun(result),'numRun')
       call assertEqual(0, numFailed(result),'numFailed')
       
       if (catch(preserve=.true.)) then
          call clean(result)
          return
       end if
       
       do p = 0, npes - 1
          unit = openFile(fileName=fname(template_fname,p), status='old', form='formatted')
          if (catch(preserve=.true.)) print*,'problem from pe',p
          if (catch(preserve=.true.)) return
          read(unit, '(a)', iostat=stat) msg
          call AssertTrue(stat==0,'Error reading message.'); if (catch(preserve=.true.)) return
          
          call AssertEqual(assumed_msg(p,npes), msg, ignoreWhiteSpace=.true.)
          if (catch(preserve=.true.)) return
          close(unit, status='delete', iostat=stat)
          call AssertTrue(stat==0,'Error closing/deleting file.'); if (catch(preserve=.true.)) return
       end do
    end if
       
    call clean(result)
       
    end subroutine testHelloWorld

  subroutine test_Broken_MPI_Method()
     use MpiServices_mod
    type (MpiTestCase_type) :: test
    type (TestResult_type)   :: result
    type (Report_type)       :: rprt, expectedMessages

    test = MpiTestCase(REFLECT(Broken_MPI_Method), numProcesses=5)
    result = newTestResult()
    call Run(test, TestInfo(), result)
    if (catch(preserve=.true.)) return

    if (amRoot()) then
       call assertEqual(1, numRun(result),'numrun')
       call assertEqual(1, numFailed(result),'numfailed')
       call assertEqual(2, numSevere(result),'numSevere')

       if (catch(preserve=.true.)) return

       expectedMessages = Report((/ &
            & '2 failures in Broken_MPI_Method:    ', &
            & '  - (pe:    1) Broken MPI method.   ',    &
            & '  - (pe:    3) Broken MPI method.   ' /))
       rprt = GenerateReport(result)
       call AssertEqual(expectedMessages,rprt)
       call clean(expectedMessages)
    end if

    call clean(test)
    call clean(result)

  end subroutine test_Broken_MPI_Method

  subroutine test_MPI_setup_teardown()
     use IO_Utilities_mod
    type (MpiTestCase_type) :: test
    type (TestResult_type) :: result

    integer, parameter :: NPES = 3
    integer :: p
    integer :: unit, stat
    character(len=MAX_LEN_MSG) :: msg

    test = MpiTestCase(REFLECT(MPI_method), npes, setup, teardown, new, delete)
    result = newTestResult()
    call Run(test, TestInfo(), result)

    if (amRoot()) then

       if (catch(preserve=.true.)) return

       do p = 0, npes - 1
          unit = openFile(fileName=fname(template_fname,p), status='old', form='formatted')
          if (catch(preserve=.true.)) return
          
          read(unit, '(a)', iostat=stat) msg
          call AssertTrue(stat==0,'Error reading message.')
          if (catch(preserve=.true.)) return
          
          call AssertEqual('MPI setup MPI method MPI teardown', msg, ignoreWhiteSpace=.true.)
          if (catch(preserve=.true.)) return
          
          close(unit, status='delete', iostat=stat)
          call AssertTrue(stat==0,'Error closing/deleting file.')
          if (catch(preserve=.true.)) return
          
       end do

       call assertEqual(1, numRun(result))
       call assertEqual(0, numFailed(result))
       
       call clean(test)
       call clean(result)
    end if
       
 end subroutine test_MPI_setup_teardown

  subroutine test_MPI_Suite()
    type (MpiTestCase_type) :: test_a

    type (TestResult_type) :: result

    integer, parameter :: NPES_MAX = 5

    character(len=MAX_LEN_MSG) :: msg
    integer :: p

    type (TestSuite_type) :: suite

    suite = TestSuite('MPI Test Suite')
    do p = 1, NPES_MAX
       call add(suite, MpiTestCase(REFLECT(MPI_trivial), numProcesses=p))
    end do
    result = newTestResult()

    call Run(suite, result)
    
    call assertEqual(5, numRun(result), 'number ran')
    call assertEqual(0, numFailed(result), 'number failed')

    call clean(suite)
    call clean(result)


  end subroutine test_MPI_Suite

  subroutine test_MPI_CountTests()
    type (MpiTestCase_type) :: test

    integer, parameter  :: NPES = 4
    integer             :: p

    test = MpiTestCase(REFLECT(HelloWorld), numProcesses=NPES)
    call assertEqual(1, countTests(test))
    
  end subroutine test_MPI_CountTests

#ifdef USE_DSO
  subroutine test_MPI_DSO()
    use IO_Utilities_mod
    use Fortrannamemangle_mod
    type (MpiTestCase_type) :: test
    type (TestResult_type) :: result

    integer, parameter  :: NPES = 4
    integer             :: p
    integer :: unit, stat

    character(len=MAX_LEN_MSG) :: msg

    test = MPI_DSO_TestCase(dso = 'libmpitests.so', &
         & dso_proc = fortranNameMangle(procedureName='HelloWorld', &
         &                 moduleName = 'MPI_Methods_mod'), npes=NPES)
    result = newTestResult()

    call Run(test, result) ! should launch an MPI job and fill test result

    call assertEqual(1, numRun(result))
    call assertEqual(0, numFailed(result))

    call clean(result)

    if (catch(preserve=.true.)) return

    do p = 0, npes - 1
       unit = openFile(fileName=fname('mpidso.',p), status='old', form='formatted')
       if (catch(preserve=.true.)) return
       read(unit, '(a)', iostat=stat) msg
       call AssertTrue(stat==0,'Error reading message.'); if (catch(preserve=.true.)) return

       call AssertEqual(assumed_msg(p, npes), msg, ignoreWhiteSpace=.true.)
       if (catch(preserve=.true.)) return
       close(unit, status='delete', iostat=stat)
       call AssertTrue(stat==0,'Error closing/deleting file.'); if (catch(preserve=.true.)) return
    end do

  end subroutine test_MPI_DSO
#endif

end module test_MpiTestCase_mod
