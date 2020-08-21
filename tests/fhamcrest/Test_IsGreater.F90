module Test_IsGreater
   use iso_fortran_env

   use funit
   implicit none

   @suite(name='Hamcrest_IsGreater')
contains
   @test
   subroutine test_is_greater_integer()
      @assert_that(1, is(greater_than(0)))
      @assert_that(2, is(greater_than(0)))

      @assert_that(1_INT64, is(greater_than(0_INT64)))
      @assert_that(2_INT64, is(greater_than(0_INT64)))

      @assert_that(1.0 == 1, is(true()))
      @assert_that(1.0, is(equal_to(1)))
   end subroutine test_is_greater_integer
end module Test_IsGreater