!@before
subroutine initA()
end subroutine initA

!@after
subroutine finalA()
end subroutine finalA

! First test
!@test
subroutine testMethodA()
end subroutine testMethodA

! Second test
!@test
subroutine testMethodB
end subroutine testMethodB




module WrapbeforeAfter
   use FUnit
   implicit none
   private

contains


end module WrapbeforeAfter

function beforeAfter_suite() result(suite)
   use FUnit
   use WrapbeforeAfter
   implicit none
   type (TestSuite) :: suite

   class (Test), allocatable :: t

   external testMethodA
   external testMethodB

   external initA
   external finalA

   suite = TestSuite('beforeAfter_suite')

   t = TestMethod('testMethodA', testMethodA, initA, finalA)
   call suite%addTest(t)

   t = TestMethod('testMethodB', testMethodB, initA, finalA)
   call suite%addTest(t)


end function beforeAfter_suite

