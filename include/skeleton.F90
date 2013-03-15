#include "fortran_tokens.h"

#define PRIVATE(obj) CONCATENATE(obj,_private)
#define PRIVATIZE(obj) use NAME_USER_MODULE, only: PRIVATE(obj) => obj

module NAME_WRAP_MODULE
#ifdef HAS_SETUP
  PRIVATIZE(fixture)
  PRIVATIZE(setup)
  PRIVATIZE(teardown)
#endif

#define OPER(method) PRIVATIZE(method)
#include METHODS_FILE
#undef OPER
  implicit none
  private

  public :: suite

#ifdef HAS_SETUP
  type fixture
     type (fixture_private)  :: user_fixture
     type (fixture), pointer :: self_reference
  end type fixture
#endif

contains

#ifdef HAS_SETUP
  subroutine new(addr)
    use pFUnit
    type (BaseAddress_type) :: addr
    type (fixture), pointer :: new_fixture
    external BaseAddress
    type (BaseAddress_type) :: BaseAddress

    allocate(new_fixture)
    new_fixture%self_reference => new_fixture
    addr =BaseAddress(new_fixture)

  end subroutine new
#endif

#ifdef HAS_SETUP
  subroutine delete(self)
    type (fixture) :: self

    deallocate(self%self_reference)

  end subroutine delete
#endif

#ifdef HAS_SETUP

#define DELEGATE(method)                    \
  subroutine method(self)                   \\
    type (fixture) :: self                  \\
    call PRIVATE(method)(self%user_fixture) \\
  end subroutine method                     \\

DELEGATE(setup)
DELEGATE(teardown)

#else

#define DELEGATE(method)   \
  subroutine method()      \\
    call PRIVATE(method)() \\
  end subroutine method    \\

#endif

#define OPER(method) DELEGATE(method)
#include METHODS_FILE
#undef OPER


  function suite()
    use pFUnit
    type (TestSuite_type) :: suite
    character(len=*), parameter :: &
         & suite_name = SUITE_NAME_STRING
    suite = TestSuite(suite_name)
#ifdef HAS_SETUP
#define ADDMETHOD(method) call Add(suite, TestCase(setup, teardown, "method", method))
#else
#define ADDMETHOD(method) call Add(suite, TestCase("method", method))
#endif
#define OPER(method) ADDMETHOD(method)
#include METHODS_FILE
#undef OPER
  end function suite
end module NAME_WRAP_MODULE
