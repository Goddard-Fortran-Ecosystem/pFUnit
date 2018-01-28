!-----------------------------------------------------------------------------
! This procedure cannot be inside of a module.  The particular version
! of this procedure that is used depends upon how the application is linked.
! The one in this file is for serial execution and uses module sFUnit.
!-----------------------------------------------------------------------------

subroutine funit_main(load_tests, extra_initialize, extra_finalize)
   use funit
   implicit none
   procedure(LoadTests_interface) :: load_tests
   procedure() :: extra_initialize, extra_finalize

   logical :: status ! .true. if no test failures/errors

   call initialize(extra_initialize)
   status = run(load_tests)
   call finalize(extra_finalize, status)
   
end subroutine funit_main
