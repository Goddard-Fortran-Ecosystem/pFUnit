module SimpleParameterizedTest
   use pFUnit
   implicit none
   private

   public :: getTestSuite

   type MyParams
      private
      real :: inLatitude
      real :: inLongitude
      real :: outLatitude
      real :: outLongitude
   end type MyParams

   type Fixture
      real :: latitude
      real :: longitude
   end type Fixture

contains

   function getTestSuite() result(suite)
      type (TestSuite_type) :: suite
      type (ParameterizedTestCase_type) :: pTest
      type (MyParams) :: params(13)
      type (BaseAddress_type) :: wrapParams(13)

      type (Fixture) :: fixtureObj
      external :: BaseAddress
      type (BaseAddress_type) :: BaseAddress
      integer :: i

      suite = TestSuite('fixture Example')

      pTest = newParameterizedTestCase(BaseAddress(fixtureObj), 'testPrincipleDomain', testPrincipleDomain, setUp, tearDown)

      ! points already in principle domain
      params(1) = MyParams(   0.0,   0.0,   0.0,   0.0)
      params(2) = MyParams(   0.0, 359.9,   0.0, 359.9)
      params(3) = MyParams(  89.9,   0.0,  89.9,   0.0)
      params(4) = MyParams( -89.9,   0.0, -89.9,   0.0)
      ! fix longitude only
      params(5) = MyParams(   0.0, 360.0,   0.0,   0.0)
      params(6) = MyParams(  89.9, 361.0,  89.9,   1.0)
      params(7) = MyParams(   0.0,  -1.0,   0.0, 359.0)
      params(8) = MyParams(   0.0, 720.0,   0.0,   0.0)
      ! fix latitude
      params(9) = MyParams(  91.0,   0.0,  89.0, 180.0)
      params(10) = MyParams( -91.0,   0.0, -89.0, 180.0)
      params(11) = MyParams( 269.0,   0.0, -89.0, 180.0)
      params(12) = MyParams( 271.0,   0.0, -89.0,   0.0)
      params(13) = MyParams(-271.0,   0.0, +89.0,   0.0)

      do i = 1, size(params)
         wrapParams(i) = BaseAddress(params(i))
      end do

      call setParams(pTest, wrapParams)
      call add(suite, pTest)

   end function getTestSuite

   subroutine setup(this, params)
      type (Fixture), intent(out) :: this
      type (MyParams), intent(in) :: params
      this%latitude = params%inLatitude
      this%longitude = params%inLongitude
   end subroutine setup

   subroutine tearDown(this)
      type (Fixture), intent(inout) :: this
   end subroutine tearDown

   subroutine testPrincipleDomain(this, params)
      use PrincipleDomain_mod
      type (Fixture), intent(in) :: this
      type (MyParams), intent(in) :: params

      real :: latLon(2)
      latLon = principleDomain(this%latitude, this%longitude)
      call assertEqual(params%outLatitude, latLon(1),'latitude')
      call assertEqual(params%outLongitude, latLon(2),'longitude')
   end subroutine testPrincipleDomain
   
end module SimpleParameterizedTest
