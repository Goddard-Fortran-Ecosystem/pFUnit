module test_AssertArray_mod
   use pFUnit
   use AssertArray_mod
   implicit none
   private

   public :: test_areConformableA
   public :: test_areConformableB
   public :: test_areConformableC
   public :: test_areConformableD

contains

   ! scalar - scalar
   subroutine test_AreConformableA()
      integer :: shape1(0)
      integer :: shape2(0)

      call assertTrue(areConformable(shape1, shape2))

   end subroutine test_AreConformableA

   ! scalar - vector
   subroutine test_AreConformableB()
      integer :: shape1(0)
      integer :: shape2(1)
      
      
      call assertFalse(areConformable(shape1, shape2))
      call assertTrue(areConformable(shape2, shape1))   ! can have scalar on rhs

   end subroutine test_AreConformableB

   ! vector - vector
   subroutine test_AreConformableC()
      integer :: shape1(1) = (/ 1 /)
      integer :: shape2(1) = (/ 2 /)
      
      call assertTrue(areConformable(shape1, shape1))
      call assertTrue(areConformable(shape2, shape2))

      call assertFalse(areConformable(shape1, shape2))
      call assertFalse(areConformable(shape2, shape1))

   end subroutine test_AreConformableC

   ! miscellaneous
   subroutine test_AreConformableD()

      integer :: array1(1)
      integer :: array2(2)
      integer :: array3(3)
      integer :: array4(1,1)
      integer :: array5(2,1)
      integer :: array6(2,2)
      integer :: array7(2,3,2)

      call assertTrue(areConformable(shape(array1), shape(array1)))
      call assertTrue(areConformable(shape(array2), shape(array2)))
      call assertTrue(areConformable(shape(array3), shape(array3)))
      call assertTrue(areConformable(shape(array4), shape(array4)))
      call assertTrue(areConformable(shape(array5), shape(array5)))
      call assertTrue(areConformable(shape(array6), shape(array6)))
      call assertTrue(areConformable(shape(array7), shape(array7)))

      call assertFalse(areConformable(shape(array1), shape(array2)))
      call assertFalse(areConformable(shape(array2), shape(array3)))
      call assertFalse(areConformable(shape(array3), shape(array4)))
      call assertFalse(areConformable(shape(array4), shape(array5)))
      call assertFalse(areConformable(shape(array5), shape(array6)))
      call assertFalse(areConformable(shape(array6), shape(array7)))
      call assertFalse(areConformable(shape(array7), shape(array1)))

   end subroutine test_AreConformableD

end module test_AssertArray_mod
