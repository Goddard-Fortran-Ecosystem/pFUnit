!
! Due to inherent limits in the OO capabilities of Fortran 2003, two
! or more classes that include components or call methods of each
! other must all be declared within a single module.  The TestResult
! class invokes TestCase methods, while Test, and TestCase
! all invoke TestResult methods.
! 
! Although it is tempting to eliminate the dependency of TestResult on
! TestCase, there are certain advantages to the JUnit architecture
! that are preserved in F2kUnit.
!
! An additional complexity arises from the fact that Fortran requires
! the types to be declared prior to any abstract interfaces that
! import them.
!
! To prevent too much complexity in any one file, the 4 component
! classes are all declared and implemented in separate file and merely
! 'included' here.  Apologies for the extra complexity and the
! otherwise heinous use of Fortran 'include'.
!

module BasicElements_mod
   use ParallelContext_mod
   use TestListener_mod
   use TestFailure_mod
   implicit none
   private

   include 'Test.inc'
   include 'GenericTestCase.inc'
   include 'TestResult.inc'

   include 'TestInterface.inc'
   include 'GenericTestCaseInterface.inc'

contains

   include 'TestImplementation.inc'
   include 'GenericTestCaseImplementation.inc'
   include 'TestResultImplementation.inc'

end module BasicElements_mod

module Test_mod
  use BasicElements_mod, only: Test
  implicit none
  private
  public :: Test
end module Test_mod

module GenericTestCase_mod
  use BasicElements_mod, only: GenericTestCase
  implicit none
  private
  public :: GenericTestCase
end module GenericTestCase_mod

module TestResult_mod
  use BasicElements_mod, only: TestResult
  use BasicElements_mod, only: newTestResult
  implicit none
  private
  public :: TestResult
  public :: newTestResult
end module TestResult_mod

