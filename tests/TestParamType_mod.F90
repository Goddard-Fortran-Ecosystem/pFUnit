Module TestParamType_mod
  Implicit None
  Private

  Public :: Params_type
  Public :: Set
  Public :: Sequence_type

  Public :: testMethod
  Public :: GetLog
  Public :: setUp
  Public :: tearDown

  ! Type for encapsulating parameters specifying a geometric series
  Type Params_type
     Private
     Integer :: n   ! number of terms in series
     Real    :: x0
     Real    :: r
  End Type Params_type

  Integer, Parameter :: MAXLEN=80
  Type Sequence_type
     Real, Pointer :: A(:) => Null()
     Character(Len=MAXLEN) :: log
  End Type Sequence_type

Contains

  Function GetLog(self) Result(log)
    Type (Sequence_type) :: self
    Character(Len=MAXLEN) :: log

    log = self%log
  End Function GetLog

  Subroutine Set(params, n, x0, r)
    Type(Params_type), Intent(InOut) :: params
    Integer, Optional, Intent(In) :: n
    Real,    Optional, Intent(In) :: x0
    Real,    Optional, Intent(In) :: r

    If (present(n)) params%n=n
    If (present(x0)) params%x0=x0
    If (present(r)) params%r=r

  End Subroutine Set

  Subroutine setUp(self, params)
    use pFUnit
    Type (Sequence_type) :: self
    Type (Params_type)   :: params
    
    Integer :: n, i
    Real :: x0, r

    n = params%n
    x0 = params%x0
    r = params%r
    
    call AssertTrue(r /= 1.0, 'illegal ratio r=1')
    if (catch(preserve=.true.)) return

    Allocate(self%A(n))

    self%A(1) = x0
    Do i = 2, n
       self%A(i) = r * self%A(i-1)
    End Do

    self%log = 'setup'

  End Subroutine setUp

  Subroutine tearDown(self, params)
    Type (Sequence_type) :: self
    Type (Params_type)   :: params

    Deallocate(self%A)

    self%log = trim(self%log) // ' teardown'
  End Subroutine tearDown

  Subroutine testmethod(self, params)
    Use Assert_mod
    Type (Sequence_type) :: self
    Type (Params_type)   :: params

    Real :: x0
    Real :: r
    Integer :: n

    Real :: actual
    Real :: expected

    x0 = params%x0
     n = params%n
     r = params%r

    actual   = ComputeSum(self%A)
    expected = x0 * (1-r**n) / (1-r)

    Call AssertEqual(expected, actual, tolerance=1.e-5)

    self%log = trim(self%log) // ' testmethod'
    
  End Subroutine testmethod

  Real Function ComputeSum(A)
    Real :: A(:)

    ComputeSum = Sum(abs(A)) ! should give incorrect result for negative values

  End Function ComputeSum

End Module TestParamType_mod
