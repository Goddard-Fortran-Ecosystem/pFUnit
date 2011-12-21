!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: ParameterizedTestCase_mod
!
!> @brief
!! Manages a list of parameter values to apply a test against.
!!
!! @author
!! Tom Clune,  NASA/GSFC SIVO
!!
!! @date
!! 16 Oct 2006
!!
!! @note 
!! In some instances it is desirable to run an essentially identical test case
!! for a large number of varying parameter values.  Directly applying TestCase
!! would result in a large amount of nearly duplicated code and would be 
!! correspondingly difficult to extend/modify.  
!!
! REVISION HISTORY:
! 16 Oct 2006 - Initial Version
! 12 Jul 2010 - Added prologue for Doxygen
!-------------------------------------------------------------------------------
module ParameterizedTestCase_mod
   use Assert_mod
   use Params_mod, only: MAX_LEN_NAME, MAX_LEN_MSG
   use BaseAddress_mod, only: BaseAddress_type
   use ProcedurePointer_mod, only: ProcedurePointer_type
   implicit none
   private
  
   public :: ParameterizedTestCase_type
   public :: newParameterizedTestCase
   public :: Run
   public :: SetParams
   public :: countTests
   public :: Clean

   type ParameterizedTestCase_type
      private
      character(len=MAX_LEN_NAME) :: name
      type (BaseAddress_type),   Pointer :: params(:) => Null()
      type (BaseAddress_type)   :: obj
      type (ProcedurePointer_type) :: method
      type (ProcedurePointer_type) :: setUp
      type (ProcedurePointer_type) :: tearDown
   end type ParameterizedTestCase_type

   interface Run
      module Procedure run_
   end interface

   interface Clean
      module procedure clean_
   end interface

   !-------------------------------------------------------------------------
   !> @interface SetParams
   !! Overload SetParams() interface to accept either a single parameter set,
   !! or a list of parameter values.
   !-------------------------------------------------------------------------
   interface SetParams
      module procedure setparams_uni
      module procedure setparams_multi
   end interface

   interface countTests
      module procedure countTests_ptest
   end interface

   integer :: ier

contains

   !---------------------------------------------------------------------------
   !> Initializes the single parameter set
   !!
   !! @param self - given self-object parameterized test case 
   !! @param params - given base address object
   !---------------------------------------------------------------------------
   subroutine SetParams_uni(self, params)
      type (ParameterizedTestCase_type)      :: self
      type (BaseAddress_type), intent(in) :: params

      call assertFalse(associated(self%params))

      allocate(self%params(1), STAT=ier)
      if(ier /= 0) print*,'ALLOCATE: IER=',ier, __LINE__,__FILE__
      self%params(1)=params
    
   end subroutine SetParams_uni

   !---------------------------------------------------------------------------
   !> Initializes the list of parameter values
   !!
   !! @param self - given self-object parameterized test case 
   !! @param params - given base address object in array
   !---------------------------------------------------------------------------
   subroutine SetParams_multi(self, params)
      type (ParameterizedTestCase_type) :: self
      type (BaseAddress_type), intent(in) :: params(:)

      call assertFalse(associated(self%params))

      allocate(self%params(size(params)),STAT=ier)
      if(ier /= 0) print*,'ALLOCATE: IER=',ier, __LINE__,__FILE__
      self%params=params
    
   end subroutine SetParams_multi

   !---------------------------------------------------------------------------
   !> Creates new object of the parameterized test case
   !!
   !! @param obj - given base address object 
   !! @param name - given name of the parameter
   !! @param method - given method function externally
   !! @param setup - given setup function externally
   !! @param teardown - given teardown function externally
   !!
   !! @return new object of the parameterized test case
   !---------------------------------------------------------------------------
   function newParameterizedTestCase(obj, name, method, setup, teardown) Result(test)
      use ProcedurePointer_mod, only: ProcedurePointer
      type (BaseAddress_type) :: obj
      character(len=*) :: name
      external :: method
      external :: setup
      external :: teardown
      type (ParameterizedTestCase_type) :: test

      test%setup = ProcedurePointer(setUp)
      test%teardown = ProcedurePointer(tearDown)
      test%method = ProcedurePointer(method)
      test%obj = obj
      test%name = trim(name)

   end function newParameterizedTestCase

   !---------------------------------------------------------------------------
   !> Executes the parameterized test case
   !!
   !! @param self - given self-object of the parameterized test case 
   !! @param result - given result from test
   !!
   !---------------------------------------------------------------------------
   subroutine Run_(self, result)
      use TestResult_mod
      use pFUnitException_mod
      use ProcedurePointer_mod, only: invoke

      type (ParameterizedTestCase_type) :: self
      type (TestResult_type)            :: result

      integer :: i, n
      character(len=MAX_LEN_MSG) :: msg

      n = size(self%params)
      do i = 1, n
         call TestStarted(result, self % name)
         call invoke(self%setup,  self%obj, self%params(i))
         call invoke(self%method, self%obj, self%params(i))

         ! Except
         if (catch()) then
            Write(msg,'(a,i0,a,i0,a)')'parameter set ',i,' of ',n,'.'
            call testFailed(result, trim(self%name), trim(msg))
            call clearAll()
         end if

         call invoke(self%teardown, self%obj)
         if (mode(result) == MODE_USE_STDOUT) write(*,'("p")',advance='no')

      end do

   end subroutine Run_

   !---------------------------------------------------------------------------
   !> Counts the number of tests 
   !!
   !! @param this - given self-object of the parameterized test case 
   !!
   !! @return countTests_ptest - number of tests
   !---------------------------------------------------------------------------
   integer function countTests_ptest(this)
      type (ParameterizedTestCase_type), intent(in) :: this
      countTests_ptest = size (this % params)
   end function countTests_ptest

   !---------------------------------------------------------------------------
   !> Cleans up the parameterized test case for deallocating its object
   !!
   !! @param self - given self-object of the parameterized test case 
   !---------------------------------------------------------------------------
   subroutine Clean_(self)
      type (ParameterizedTestCase_type) :: self

      deallocate(self%params,STAT=ier)
      if(ier /= 0) print*,'ALLOCATE: IER=',ier, __LINE__,__FILE__

   end subroutine Clean_

end module ParameterizedTestCase_mod
