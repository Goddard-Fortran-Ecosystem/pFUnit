#include "reflection.h"
#define HERE print*,__LINE__,__FILE__
module SUT_mod
   implicit none
   private

   public :: SUT

   type SUT
      integer :: intComponent
   contains
      procedure :: method1
   end type SUT

contains

   subroutine method1(this)
      class (SUT), intent(in) :: this
   end subroutine method1

end module SUT_mod

module MockSUT_mod
   use MockRepository_mod
   use SUT_mod
   implicit none
   private

   public :: MockSUT
   public :: newMockSUT

   type, extends(SUT) :: MockSUT
      type (MockRepository), pointer :: mocker => null()
   contains
      final :: verifyMocking
   end type MOCKSUT

contains

   function newMockSUT(repository) result(mock)
      type (MockSUT), allocatable :: mock
      class (MockRepository), target :: repository

      allocate(mock)
      mock%mocker => repository

   end function newMockSUT

   subroutine verifyMocking(this)
      use Exception_mod
      type (MockSUT), intent(inout) :: this

      if (associated(this%mocker)) then
         call this%mocker%verifyMocking(this)
      end if

   end subroutine verifyMocking

end module MockSUT_mod

module Test_MockRepository_mod
   use TestSuite_mod
   use MockRepository_mod
   use Exception_mod
   use Assert_mod

   use SUT_mod
   use MockSUT_mod
   implicit none
   private

   public :: suite

   ! test that all registered objects are finalized (checked)

contains

#define ADD(method) call suite%addTest(newSimpleTestMethod(REFLECT(method)))

   function suite()
      use SimpleTestMethod_mod, only: newSimpleTestMethod
      type (TestSuite), pointer :: suite

      allocate(suite)
      suite => newTestSuite('Test_MockRepository')

      ADD(testNoAction)
      ADD(testExpectMethod_NotCalled)

   end function suite

   subroutine testNoAction()
      class (MockRepository), pointer :: mocker
      type (SUT) :: object
      type (MockSUT) :: mockObject
      
      mocker => newMockRepository()
      mockObject = newMockSUT(mocker)

   end subroutine testNoAction

   subroutine testExpectMethod_NotCalled()

      call internalProcedure() ! verification is when object is final-ized
      call assertTrue(catch('Expected method not called: method1() on object of class MockSUT.'))

   contains

      subroutine internalProcedure()
         class (MockRepository), pointer :: mocker
         type (SUT) :: object
         type (MockSUT) :: mockObject

         mocker => newMockRepository()
         mockObject = newMockSUT(mocker)
         call mocker%expectCall(mockObject,'method1')

      end subroutine internalProcedure

   end subroutine testExpectMethod_NotCalled

end module Test_MockRepository_mod
