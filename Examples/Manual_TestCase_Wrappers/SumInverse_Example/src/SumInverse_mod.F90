module SumInverse_mod
   implicit none
   private
   
   public :: SumInverse_type
   public :: setArray
   public :: getSum 

   type SumInverse_type
      real, allocatable, dimension(:) :: array 
      integer :: size
   end type SumInverse_type

contains

   subroutine setArray(this, newArray)
      type(SumInverse_type), intent(inout) :: this
      real, intent(in) :: newArray(:)
      integer :: numOfArray
      integer :: index 

      numOfArray = size(newArray,1)
      allocate(this % array(numOfArray))
      do index=1,numOfArray
         this % array(index) = newArray(index)
      end do 
      this % size = numOfArray

   end subroutine setArray
   

   real function getSum(this) result(sum)
      type(SumInverse_type) :: this
      integer :: index

      sum = 0.0
      do index = 1, this % size
         sum = sum + 1.0/this % array(index)
      end do

   end function getSum

end module SumInverse_mod
