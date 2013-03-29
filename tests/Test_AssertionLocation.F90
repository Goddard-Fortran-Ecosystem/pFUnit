#include "reflection.h"
module Test_AssertionLocation_mod
   use TestSuite_mod
   use Assertionlocation_mod
   implicit none
   private

   public :: suite

contains

   function suite()
      use TestSuite_mod, only: TestSuite, newTestSuite
      use TestMethod_mod, only: newTestMethod
      type (TestSuite), pointer :: suite

      suite => newTestSuite('assertionLocationSuite')

#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))

      ADD(testNoInfo)
      ADD(testLineNumberOnly)
      ADD(testFileAndLineNumber)

   end function suite

   subroutine testNoInfo
      use Assert_mod, only: assertEqual
      type (AssertionLocation) :: location
      call assertEqual(" ", location%print())
   end subroutine testNoInfo

   subroutine testLineNumberOnly()
      use Assert_mod, only: assertEqual
      class (LineAssertionLocation), pointer :: location
      location => newAssertionLocation(lineNumber=5)
      call assertEqual('at line <5>', location%print())
   end subroutine testLineNumberOnly

   subroutine testFileAndLineNumber()
      use Exception_mod, only: MAXLEN_MESSAGE
      use Assert_mod, only: assertEqual
      class (AssertionLocation), pointer :: location
      character(len=MAXLEN_MESSAGE) :: message
      location => newAssertionLocation(lineNumber=5, fileName='foo.F90')
      message = location%print()
      call assertEqual('in foo.F90 at line <5>', location%print())
   end subroutine testFileAndLineNumber

end module Test_AssertionLocation_mod
