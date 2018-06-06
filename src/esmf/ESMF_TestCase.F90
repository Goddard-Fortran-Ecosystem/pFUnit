module ESMF_TestCase_mod
   use ESMF
   use ESMF_TestParameter_mod
   use pfunit_mod, only: MpiTestCase
   use pfunit_mod, only: anyExceptions
   implicit none

   private

   public :: InternalState
   public :: ESMF_TestCase



   type Wrapper
      class (ESMF_TestCase), pointer :: testPtr => null()
   end type Wrapper

   type InternalState
      type (Wrapper), pointer :: wrapped
   end type InternalState

   type, extends(MpiTestCase) :: ESMF_TestCase
      type (InternalState), pointer :: wrapped
      type (ESMF_GridComp), pointer :: gc => null()
      integer :: val = 3
   contains
      procedure :: runBare
      procedure :: setInternalState
      procedure :: clearInternalState
      procedure :: getVM
      procedure :: getPetCount
      procedure :: getLocalPET
      procedure :: barrier
      procedure :: getNumPETsRequested
   end type ESMF_TestCase

contains


   recursive subroutine runBare(this)
      use Exception_mod
      use ParallelException_mod
      use ESMF
      class (ESMF_TestCase), intent(inout) :: this

      logical :: discard
      type (ESMF_GridComp), target :: gc
      integer :: rc
      integer :: pet


      ! Gridded component 
      gc = ESMF_GridCompCreate(petList=[(pet,pet=0,this%getNumPETsRequested()-1)], rc=rc)
      if (rc /= ESMF_SUCCESS) call throw('Insufficient PETs for request')
      if (rc /= ESMF_SUCCESS) call throw('Failed to get vm for gridded component.')

      this%gc => gc
      this%val = 4
      
      call this%setInternalState(gc)
      ! create subcommunicator
      this%context = this%parentContext%makeSubcontext(this%getNumPETsRequested())

      if (.not. anyExceptions(this%parentContext)) then
         if (this%context%isActive()) then
            call ESMF_GridCompInitialize(gc, rc=rc)

            if (.not. anyExceptions(this%context)) then
               call ESMF_GridCompRun(gc, rc=rc)
               call ESMF_GridCompFinalize(gc, rc=rc)
            end if
         end if
      else
         ! only report context failure on root PE
         if (.not. this%parentContext%isRootProcess()) then
            discard = catch()
         end if
      end if

      call gather(this%parentContext)

      call this%clearInternalState(gc)

   end subroutine runBare

   subroutine setInternalState(this, gc)
      class (ESMF_TestCase), target, intent(inout) :: this
      type (ESMF_GridComp), intent(inout) :: gc

      integer :: rc

      allocate(this%wrapped) ! note this is a memory leak.
      allocate(this%wrapped%wrapped) ! note this is a memory leak.
      this%wrapped%wrapped%testPtr => this

      ! Note - this%wrapped%wrapped%testPtr must be set outside. Cannot at target attribute to this
      ! interface.
      call ESMF_GridCompSetServices(gc, setServices, rc=rc)

      call ESMF_GridCompSetInternalState(gc, this%wrapped, rc)

   end subroutine setInternalState

   subroutine clearInternalState(this, gc)
      class (ESMF_TestCase), intent(inout) :: this
      type (ESMF_GridComp), intent(inout) :: gc

      integer :: rc

      deallocate(this%wrapped%wrapped)
      deallocate(this%wrapped)

      call ESMF_GridCompDestroy(gc, rc=rc)

   end subroutine clearInternalState

   subroutine initialize(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: comp
      type(ESMF_State) :: importState            ! must not be optional
      type(ESMF_State) :: exportState            ! must not be optional
      type(ESMF_Clock) :: clock                  ! must not be optional
      integer, intent(out)  :: rc                ! must not be optional

      type (InternalState), target :: wrap
      class (ESMF_TestCase), pointer :: testPtr
      integer :: finalrc

      type(ESMF_StateItem_Flag) :: itemtype

      ! To prevent "unused variable" warnings, we do something useless here.
      call ESMF_StateGet(importState,'item', itemtype)
      call ESMF_StateGet(exportState,'item', itemtype)
      call ESMF_ClockGet(clock)

      ! Get Internal State
      call ESMF_GridCompGetInternalState(comp, wrap, rc)
      if (rc .ne. ESMF_SUCCESS) then
         finalrc = ESMF_FAILURE
      else
         finalrc = ESMF_SUCCESS
      end if

      ! Access private data block and verify data
      testPtr => wrap%wrapped%testPtr 
      call testPtr%setUp()
   
      rc = finalrc

   end subroutine initialize


   subroutine run(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: comp                   ! must not be optional
      type(ESMF_State)      :: importState            ! must not be optional
      type(ESMF_State)      :: exportState            ! must not be optional
      type(ESMF_Clock)      :: clock                  ! must not be optional
      integer, intent(out)  :: rc                     ! must not be optional

      type (InternalState) :: wrap
      class (ESMF_TestCase), pointer :: testPtr

      integer :: finalrc

      type(ESMF_StateItem_Flag) :: itemtype

      ! To prevent "unused variable" warnings, we do something useless here.
      call ESMF_StateGet(importState,'item', itemtype)
      call ESMF_StateGet(exportState,'item', itemtype)
      call ESMF_ClockGet(clock)

      call ESMF_GridCompGetInternalState(comp, wrap, rc)
      if (rc .ne. ESMF_SUCCESS) then
         finalrc = ESMF_FAILURE
      else
         finalrc = ESMF_SUCCESS
      end if

      ! Access private data block and verify data
      testPtr => wrap%wrapped%testPtr 
      call testPtr%runMethod()

      rc = finalRc

   end subroutine run


   subroutine finalize(comp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: comp                   ! must not be optional
      type(ESMF_State)      :: importState            ! must not be optional
      type(ESMF_State)      :: exportState            ! must not be optional
      type(ESMF_Clock)      :: clock                  ! must not be optional
      integer, intent(out)  :: rc                     ! must not be optional


      type (InternalState) :: wrap
      class (ESMF_TestCase), pointer :: testPtr
      integer :: finalrc

      type(ESMF_StateItem_Flag) :: itemtype

      ! To prevent "unused variable" warnings, we do something useless here.
      call ESMF_StateGet(importState,'item', itemtype)
      call ESMF_StateGet(exportState,'item', itemtype)
      call ESMF_ClockGet(clock)

      ! This is where the model specific computation goes.

      call ESMF_GridCompGetInternalState(comp, wrap, rc)
      if (rc .ne. ESMF_SUCCESS) then
         finalrc = ESMF_FAILURE
      else
         finalrc = ESMF_FAILURE
      end if

      ! Access private data block and verify data
      testPtr => wrap%wrapped%testPtr 
      call testPtr%tearDown()

      rc = finalRc

   end subroutine finalize

   subroutine setServices(comp, rc)
      type(ESMF_GridComp)   :: comp   ! must not be optional
      integer, intent(out)  :: rc     ! must not be optional

      call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_INITIALIZE, &
                                userRoutine=initialize, rc=rc)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_RUN, &
                                userRoutine=run, rc=rc)
      call ESMF_GridCompSetEntryPoint(comp, ESMF_METHOD_FINALIZE, &
                                userRoutine=finalize, rc=rc)

      rc = ESMF_SUCCESS

   end subroutine setServices

   function getVM(this) result(vm)
      class (ESMF_TestCase), intent(in) :: this
      type (ESMF_VM) :: vm
      integer :: rc

      call ESMF_GridCompGet(this%gc, VM=vm, rc=rc)

   end function getVM

   integer function getPetCount(this) result(petCount)
      class (ESMF_TestCase), intent(in) :: this

      type (ESMF_VM) :: vm
      
      vm = this%getVM()
      call ESMF_VMGet(vm, petCount=petCount)

   end function getPetCount

   integer function getLocalPET(this) result(localPET)
      class (ESMF_TestCase), intent(in) :: this

      type (ESMF_VM) :: vm
      
      vm = this%getVM()
      call ESMF_VMGet(vm, localPET=localPET)

   end function getLocalPET


   subroutine barrier(this)
      class (ESMF_TestCase), intent(in) :: this

      type (ESMF_VM) :: vm
      
      vm = this%getVM()
      call ESMF_VMBarrier(vm)

   end subroutine barrier

   
   integer function getNumPETsRequested(this) result(numPETsRequested)
      use Exception_mod
      class (ESMF_TestCase), intent(in) :: this
      select type (p => this%testParameter)
      class is (ESMF_TestParameter)
         numPETsRequested = p%getNumPETsRequested()
      class default
         numPETsRequested = 0
         call throw('Incorrect type of test parameter in ESMF_TestCase::getNumPETsRequested()')
      end select
   end function getNumPETsRequested


end module ESMF_TestCase_mod
