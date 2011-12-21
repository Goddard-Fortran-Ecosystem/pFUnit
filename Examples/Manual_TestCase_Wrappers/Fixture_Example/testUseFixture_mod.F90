! Tests F90 intrinsic routines "sum()" and "product()"
module testUseFixture_mod
   use pFUnit
   implicit none
   private

   public :: getTestSuite

   type fixture
      private

      integer :: sequenceLength
      real, pointer :: arithmeticSequence(:)  => null()
      real, pointer :: telescopingSequence(:) => null()

   end type fixture

contains

   function getTestSuite() result(suite)
      type (TestSuite_type) :: suite

      suite = TestSuite('fixture Example')

      call add(suite, TestCase1StepFixture(setUp, tearDown, 'testSum',     testSum))
      call add(suite, TestCase1StepFixture(setUp, tearDown, 'testProduct', testProduct))

   end function getTestSuite

   subroutine setUp(aFixture)
      type (fixture) :: aFixture
      integer :: i
      integer, parameter :: N = 100
      aFixture % sequenceLength = N

      allocate(aFixture % arithmeticSequence(N))
      aFixture % arithmeticSequence = (/ (i, i = 1, N) /)

      allocate(aFixture % telescopingSequence(N)) ! (i+1)/i
      aFixture % telescopingSequence = (/ ( (real(i+1) / real(i)), i = 1, N) /)
      
   end subroutine setUp

   subroutine tearDown(aFixture)
      type (fixture) :: aFixture

      deallocate(aFixture % telescopingSequence)
      deallocate(aFixture % arithmeticSequence)
      aFixture % sequenceLength = -1

   end subroutine tearDown

   subroutine testSum(aFixture)
      type (fixture) :: aFixture

      real :: expected
      real :: found
      real, parameter :: eps = 10 * epsilon(1.)
      integer :: n

      n = aFixture % sequenceLength

      expected = n * (n + 1) / 2
      found = sum(aFixture % arithmeticSequence)

      call assertEqual(expected, found, tolerance = eps)

   end subroutine testSum
   
   subroutine testProduct(aFixture)
      type (fixture) :: aFixture

      real :: expected
      real :: found
      integer :: n

      n = aFixture % sequenceLength

      expected = n + 1
      found = product(aFixture % telescopingSequence)

      call assertEqual(expected, found, tolerance = 1000 * epsilon(expected))

   end subroutine testProduct
   
end module testUseFixture_mod
