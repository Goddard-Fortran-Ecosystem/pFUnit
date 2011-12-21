#include "reflection.h"
Module TestParameterizedTestCase_mod
  use pFUnit
  Use TestParamType_mod,            only: Params_type, Sequence_type
  Implicit None
  Private

  Public :: Fixture_type

  Public :: setUp_ptest
  Public :: tearDown_ptest

  Public :: testWasRun
  Public :: testMethod_uni
  Public :: testMethod_multi_summary
  public :: testCountTestsA
  public :: testCountTestsB

  Type Fixture_type
     private
     Type(Params_type),        Pointer :: params(:) => null()
     Type(BaseAddress_type), Pointer :: wrap_params(:) => null()
     Type (Sequence_type),     Pointer :: sequence => null()
     Type (ParameterizedTestCase_type), Pointer  :: test => null()
     Type (TestResult_type) :: result
  End Type Fixture_type

  Integer, Parameter :: n_cases = 5

  External :: BaseAddress
  Type (BaseAddress_type) :: BaseAddress

  integer :: ier
Contains

  Subroutine setUp_ptest(self)
    Use TestParamType_mod, only: set, testmethod
    Use TestParamType_mod, only: setup_p => setup, teardown_p => teardown
    Type (Fixture_type) :: self
    Integer :: i

    Allocate(self%sequence, self%test, STAT=ier)
    if (ier /= 0) print*,'IER: ',ier, __LINE__,__FILE__
        
    self%test = newParameterizedTestCase(BaseAddress(self%sequence), REFLECT(testmethod), setup_p, teardown_p)

    Allocate(self%params(n_cases), self%wrap_params(n_cases), STAT=ier)
    if (ier /= 0) print*,'IER: ',ier, __LINE__,__FILE__

    Call Set(self%params(1), n =  2, x0=1., r = 0.50)
    Call Set(self%params(2), n = 10, x0=1., r = 0.50)
    Call Set(self%params(3), n =  2, x0=1., r = 0.00)  ! illegal 
    Call Set(self%params(4), n = 10, x0=1., r = 0.25)
    Call Set(self%params(5), n = 10, x0=-1., r = 0.25)

    Do i = 1, n_cases
       self%wrap_params(i) = BaseAddress(self%params(i))
    End Do

    self%result = newTestResult()

  End Subroutine setUp_ptest

  Subroutine teardown_ptest(self)
    Type (Fixture_type) :: self

    Call Clean(self%test)
    Call Clean(self%result)

    Deallocate(self%params, self%wrap_params,STAT=ier)
    if (ier /= 0) print*,'IER: ',ier, __LINE__,__FILE__
    Deallocate(self%sequence, self%test,STAT=ier)
    if (ier /= 0) print*,'IER: ',ier, __LINE__,__FILE__

  End Subroutine teardown_ptest

  Subroutine testWasRun(self)
    Use TestParamType_mod, Only: GetLog

    Type (Fixture_type) :: self

    Call SetParams(self%test, self%wrap_params(1) )
    Call Run(self%test, self%result)
    Call AssertEqual('setup testmethod teardown',Trim(GetLog(self%sequence)))

  End Subroutine testWasRun

  Subroutine testMethod_uni(self)
    Use ParameterizedTestCase_mod
    Type (Fixture_type) :: self

    Call SetParams(self%test, self%wrap_params(1) )
    Call Run(self%test, self%result)
    call assertEqual(1, numRun(self%result))
    call assertEqual(0, numFailed(self%result))

  End Subroutine testMethod_uni

  Subroutine testMethod_multi_summary(self)
    Type (Fixture_type) :: self

    type (Report_type) :: expected, rprt

    Call SetReportMode(self%result, MODE_USE_BUFFER)
    Call SetParams(self%test, self%wrap_params )
    Call Run(self%test, self%result)
    call assertEqual(5, numRun(self%result))
    call assertEqual(2, numFailed(self%result))
    call assertEqual(2, numSevere(self%result))
    
    expected = Report( (/ 'Failure in testmethod - parameter set 3 of 5.', &
         & 'Failure in testmethod - parameter set 5 of 5.'/) )
    
    rprt = GenerateReport(self%result)
    Call AssertEqual(expected, rprt)
    call clean(expected)
    call clean(rprt)

  End Subroutine testMethod_multi_summary

  subroutine testCountTestsA(self)
    Type (Fixture_type) :: self

    call setParams(self%test, self%wrap_params(1) )
    call assertEqual(1, countTests(self % test))

 end Subroutine testCountTestsA

  subroutine testCountTestsB(self)
    Type (Fixture_type) :: self

    call setParams(self%test, self%wrap_params )
    call assertEqual(n_cases, countTests(self % test))

 end Subroutine testCountTestsB

End Module TestParameterizedTestCase_mod
