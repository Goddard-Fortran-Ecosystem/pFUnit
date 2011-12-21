module TestSumInverse_mod
    use pFUnit, only : assertEqual
    use SumInverse_mod
    implicit none

    public :: testSumInverseEmpty
    public :: testSumInverseScalar
    public :: testSumInverseArray

contains

   subroutine testSumInverseEmpty()
      type(SumInverse_type) :: sumInverse 
      real :: a
      real :: sum

      a = 0.0
      sum = 0.0
      call assertEqual(0.0, sum, tolerance=0.00001)

   end subroutine testSumInverseEmpty

   subroutine testSumInverseScalar()
      type(SumInverse_type) :: sumInverse 
      real :: a(1)
      real :: sum

      a(1) = 1./2
      call setArray(sumInverse,a)

      sum = getSum(sumInverse)
      call assertEqual(2., sum, tolerance=0.00001)

   end subroutine testSumInverseScalar
        
   subroutine testSumInverseArray()
      type(SumInverse_type) :: sumInverse 
      real :: a(3)
      real :: sum

      a(1) = 1./2
      a(2) = 1./3
      a(3) = 1./5
      call setArray(sumInverse,a)

      sum = getSum(sumInverse)
      call assertEqual(2.+3.+5., sum, tolerance=0.00001)

   end subroutine testSumInverseArray

end module TestSumInverse_mod
