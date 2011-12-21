module tests_mod
   implicit none
   private

   public :: testFailOnPurpose
   public :: testZeroVectors
   public :: testSameVector
   public :: testOrthogonalVectors

contains

   ! This first test routine is intended to fail so as to demonstrate!
   ! how failed tests appear to users.
   subroutine testFailOnPurpose()
      use pFUnit
      
      call assertEqual(1, 2, message = 'Intentional failure for instructional purposes.')
      
   end subroutine testFailOnPurpose
   
   subroutine testZeroVectors()
      use pFUnit
      use VectorOperations
      implicit none
      real :: a(3), b(3), c(3)
      
      a = (/ 0, 0, 0 /)
      b = (/ 0, 0, 0 /)
      c = vectorCrossProduct(a, b)
      
      call assertEqual(0, c) ! vector comparison with 0 tolerance
      
   end subroutine testZeroVectors
   
   ! cross product of vector with itself is 0
   subroutine testSameVector()
      use pFUnit
      use VectorOperations
      implicit none
      real :: a(3), c(3)
      
      a = (/ 1, 2, 3 /)
      c = vectorCrossProduct(a, a)
      
      call assertEqual(0, c) ! vector comparison with 0 tolerance
      
   end subroutine testSameVector
   
   subroutine testOrthogonalVectors()
      use pFUnit
      use VectorOperations
      implicit none
      real, dimension(3) :: e1, e2, e3
      
      e1 = (/1, 0, 0/)
      e2 = (/0, 1, 0/)
      e3 = (/0, 0, 1/)
      
      call assertEqual(e3, vectorCrossProduct(e1,e2))
      call assertEqual(e1, vectorCrossProduct(e2,e3))
      call assertEqual(e2, vectorCrossProduct(e3,e1))
      
   end subroutine testOrthogonalVectors
   
end module tests_mod
