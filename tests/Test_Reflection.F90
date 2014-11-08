#include "reflection.h"
module Test_Reflection_mod
  use AssertBasic_mod
  use AssertInteger_mod
  use Exception_mod, only: NULL_MESSAGE
  use TestSuite_mod, only: TestSuite, newTestSuite
  use Exception_mod, only: catch

  use Exception_mod, only: getNumExceptions
  implicit none
  private

  public :: suite

contains

  function suite()
    use TestSuite_mod, only: TestSuite, newTestSuite
    use TestMethod_mod, only: newTestMethod
    use Test_mod
    type (TestSuite) :: suite
    suite = newTestSuite('ReflectionAndMessageTests')
#define ADD(method) call suite%addTest(newTestMethod(REFLECT(method)))
    ADD(testReflectName)
  end function suite

  subroutine testReflectName()
    
    integer aTestVariable

    aTestVariable = 999

    call assertEqual("aTestVariable",parseString(REFLECT(aTestVariable)))
    call assertEqual(aTestVariable,parseInteger(REFLECT(aTestVariable)))

  contains

    function parseString(s,i) result(res)
      character(*), intent(in) :: s
      integer, intent(in) :: i
      character(:), allocatable :: res
      allocate(res,source=s)
    end function parseString

    function parseInteger(s,i) result(res)
      character(*), intent(in) :: s
      integer, intent(in) :: i
      integer :: res
      res = i
    end function parseInteger

  end subroutine testReflectName

end module Test_Reflection_mod
