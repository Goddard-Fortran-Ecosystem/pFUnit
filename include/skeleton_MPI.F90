#include "fortran_tokens.h"

#define PRIVATE(obj) CONCATENATE(obj,_private)
#define PRIVATIZE(obj) use NAME_USER_MODULE, only: PRIVATE(obj) => obj


module NAME_WRAP_MODULE
  use pFunit
#ifdef HAS_SETUP
  PRIVATIZE(fixture)
  PRIVATIZE(setup)
  PRIVATIZE(teardown)
#endif

#define OPER(method) PRIVATIZE(method), CONCATENATE(NPROC_,method)
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
    type (BaseAddress_type) :: addr
    type (fixture), pointer :: new_fixture
    external BaseAddress
    type (BaseAddress_type) :: BaseAddress

    allocate(new_fixture)
    new_fixture%self_reference => new_fixture
    addr = BaseAddress(new_fixture)

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
  subroutine method(self, info)          \\
    type (fixture) :: self                 \\
    type (TestInfo_type) :: info           \\
    call PRIVATE(method)(self%user_fixture, info) \\
  end subroutine method                     \\

DELEGATE(setup)
DELEGATE(teardown)

#else

#define DELEGATE(method)   \
  subroutine method(info)      \\
    type (TestInfo_type) :: info           \\
    call PRIVATE(method)(info) \\
  end subroutine method    \\

#endif

#define OPER(method) DELEGATE(method)
#include METHODS_FILE
#undef OPER

  function suite()
    type (TestSuite_type) :: suite
    character(len=*), parameter :: &
         & suite_name= SUITE_NAME_STRING

    integer :: i, nproc

    suite = TestSuite(suite_name)
#ifdef HAS_SETUP
#define ADDMETHOD(method)\
 do i = 1, size(CONCATENATE(NPROC_,method)) \\
    nproc = CONCATENATE(NPROC_,method)(i) \\
    call Add(suite, MpiTestCase("method", method, nproc, setup, teardown, new, delete)) \\
 end do
#else
#define ADDMETHOD(method) \
 do i = 1, size(CONCATENATE(NPROC_,method)) \\
    nproc = CONCATENATE(NPROC_,method)(i) \\
    call Add(suite, MpiTestCase("method", method, nproc)) \\
 enddo
#endif
#define OPER(method) ADDMETHOD(method)
#include METHODS_FILE
#undef OPER
  end function suite
end module NAME_WRAP_MODULE
