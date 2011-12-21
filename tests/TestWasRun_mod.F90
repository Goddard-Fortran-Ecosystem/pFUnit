#include "reflection.h"
Module TestWasRun_mod
  Use pFUnit
  Implicit None
  Private

  Public :: WasRun_type
  Public :: WasRun
  Public :: testMethod
  Public :: BrokenMethod
  Public :: DoublyBrokenMethod
  Public :: BrokenSetup
  Public :: oneTest
  Public :: anotherTest
  Public :: Setup
  Public :: tearDown
  Public :: GetLog

  Integer, Parameter :: MAXLEN=80 ! maximum length of log
  Type WasRun_type
     Character(Len=MAXLEN) :: log
     Logical               :: oneTest_has_run
     Logical               :: anotherTest_has_run
  End Type WasRun_type

Contains

  Type (WasRun_type) Function WasRun()
    WasRun%log=' '
    WasRun%onetest_has_run     = .false.
    WasRun%anotherTest_has_run = .false.
  End Function WasRun

  Subroutine testMethod(self)
    Type (WasRun_type) :: self

    self%log=Trim(self%log)//" testMethod"

  End Subroutine testMethod

  Subroutine BrokenMethod(self)
    Type (WasRun_type) :: self

    Call throw(Exception('Broken method'))

  End Subroutine BrokenMethod

  Subroutine DoublyBrokenMethod(self)
    Type (WasRun_type) :: self

    Call throw(Exception('Broken method'))
    Call throw(Exception('Broken method'))

  End Subroutine DoublyBrokenMethod

  Subroutine setup(self)
    Type (WasRun_type) :: self

    Call ClearLog(self)
    self%log = Trim(self%log) // "setUp"

  End Subroutine setup

  Subroutine teardown(self)
    Type (WasRun_type) :: self

    self%log = Trim(self%log)//" tearDown"

  End Subroutine teardown

  Subroutine oneTest(self)
    Type (WasRun_type) :: self

    self%onetest_has_run = .true.

  End Subroutine oneTest

  Subroutine anotherTest(self)
    Type (WasRun_type) :: self

    self%anothertest_has_run = .true.

  End Subroutine anotherTest

  Subroutine ClearLog(self)
    Type (WasRun_type) :: self
    self%log =' '
  End Subroutine ClearLog

  Character(Len=MAXLEN) Function GetLog(self)
    Type (WasRun_type) :: self
    GetLog=self%log
  End Function GetLog

  Subroutine BrokenSetup(self)
    Type (WasRun_type) :: self

    Call throw(Exception('broken setup'))

  End Subroutine BrokenSetup

End Module TestWasRun_mod
