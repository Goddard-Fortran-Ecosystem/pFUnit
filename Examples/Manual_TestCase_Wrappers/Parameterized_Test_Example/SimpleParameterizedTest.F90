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
   end type Fixture

contains

   function getTestSuite() result(suite)
      type (TestSuite_type) :: suite
      type (ParameterizedTestCase_type) :: pTest
      type (MyParams) :: params(3)
      type (BaseAddress_type) :: wrapParams(3)

      type (Fixture) :: fixtureObj
      external :: BaseAddress
      type (BaseAddress_type) :: BaseAddress

      suite = TestSuite('fixture Example')

      pTest = newParameterizedTestCase(BaseAddress(fixtureObj), 'testPrincipleDomain', testPrincipleDomain, setUp, tearDown)

      params(1) = MyParams(0.0, 0.0, 0.0, 0.0)
      params(2) = MyParams(0.0, 360.0, 0.0, 0.0)
      params(3) = MyParams(1.0, 1.0, 0.0, 0.0) ! intentionally fail

      wrapParams(1) = BaseAddress(params(1))
      wrapParams(2) = BaseAddress(params(2))
      wrapParams(3) = BaseAddress(params(3))

      call setParams(pTest, wrapParams)
      call add(suite, pTest)

   end function getTestSuite

   subroutine setup(this, params)
      type (Fixture), intent(out) :: this
      type (MyParams), intent(in) :: params
   end subroutine setup

   subroutine tearDown(this)
      type (Fixture), intent(inout) :: this
   end subroutine tearDown

   subroutine testPrincipleDomain(this, params)
      type (Fixture), intent(in) :: this
      type (MyParams), intent(in) :: params

      call assertTrue(params%inLongitude == params%outLongitude)
   end subroutine testPrincipleDomain
   
end module SimpleParameterizedTest
