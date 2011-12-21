#include "fortran_tokens.h"

program main
  use pFUnit

#define OPER(module) use CONCATENATE(module,_wrap), only: CONCATENATE(suite_,module) => suite

#include "suite_list"
#undef OPER

  implicit none

  type (TestSuite_type)  :: top
  type (TestResult_type) :: result
  type (Report_type)     :: rprt
  character(len=100)  :: summary_statement

  call pFUnit_init()
!!$$  call setDebug()
  top = TestSuite('top')

#define OPER(module) call Add(top, CONCATENATE(suite_,module)())

#include "suite_list"
#undef OPER

  result=newTestResult(mode=MODE_USE_STDOUT + MODE_USE_LOGFILE)
  call Run(top, result)

  if (amRoot()) then
     summary_statement=Summary(result)
     print*
     print*,trim(summary_statement)
  end if

  ! Catch remaining exceptions
  if (ExceptionRaised()) then
     print*,'Remaining exception: '
     rprt = GenerateExceptionReport()
     call Print(rprt)
     call clean(rprt)
  end if

  call clean(result)
  call clean(top)
  call pFUnit_finalize()

end program main
