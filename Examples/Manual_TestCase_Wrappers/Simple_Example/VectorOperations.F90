Module VectorOperations
   implicit none
   private

   public :: vectorCrossProduct

contains

! Compute the vector cross product of two 3D vectors.
! Result is a 3D vector.
   function vectorCrossProduct(a, b) result(c)
      implicit none
      integer, parameter :: N = 3
      real, intent(in) :: a(N)
      real, intent(in) :: b(N)
      real :: c(N)
      
      c(3) = a(1) * b(2) - b(1) * a(2)
      c(1) = a(2) * b(3) - b(2) * a(3)
      c(2) = a(3) * b(1) - b(3) * a(1)
      
   end function vectorCrossProduct
   
end Module VectorOperations
