#include "unused_dummy.fh"

module pf_IsRelativelyNear
  use iso_fortran_env, only: REAL32, REAL64
  use pf_TypeSafeMatcher
  use pf_MatcherDescription
  use pf_AbstractArrayWrapper
  use pf_ArrayWrapper
  implicit none
  private

  public :: IsRelativelyNear_32
  public :: IsRelativelyNear_64
  public :: relatively_near



  !------------------------------------------------------------
  ! 32-bit variant
  !------------------------------------------------------------
  type, extends(TypeSafeMatcher) :: IsRelativelyNear_32
     private
     real :: tolerance
     real :: value
     character(:), allocatable :: text_description
     class(*), allocatable :: array_value
   contains
     procedure :: matches_safely           => matches_safely_32
     procedure :: describe_mismatch_safely => describe_mismatch_safely_32
     procedure :: expects_type_of          => expects_type_of_32
     procedure :: delta                    => delta_32
     procedure :: describe_to              => describe_to_32
     procedure :: matches_array_1d         => matches_array_1d_32
     procedure :: matches_array_2d         => matches_array_2d_32
     procedure :: matches_array_3d         => matches_array_3d_32
     procedure :: matches_array_4d         => matches_array_4d_32
  end type IsRelativelyNear_32

  !------------------------------------------------------------
  ! 64-bit variant
  !------------------------------------------------------------
  type, extends(TypeSafeMatcher) :: IsRelativelyNear_64
     private
     real(kind=REAL64) :: tolerance
     real(kind=REAL64) :: value
     character(:), allocatable :: text_description
     class(*), allocatable :: array_value
   contains
     procedure :: matches_safely           => matches_safely_64
     procedure :: describe_mismatch_safely => describe_mismatch_safely_64
     procedure :: expects_type_of          => expects_type_of_64
     procedure :: delta                    => delta_64
     procedure :: describe_to              => describe_to_64
     procedure :: matches_array_1d         => matches_array_1d_64
     procedure :: matches_array_2d         => matches_array_2d_64
     procedure :: matches_array_3d         => matches_array_3d_64
     procedure :: matches_array_4d         => matches_array_4d_64
  end type IsRelativelyNear_64

  interface relatively_near
     ! scalar
     module procedure relatively_near_real
     module procedure relatively_near_double
#if (defined(_ISO_REAL32) && (_ISO_REAL32 != _REAL_DEFAULT_KIND) && (_ISO_REAL32 != _DOUBLE_DEFAULT_KIND))
     module procedure relatively_near_real32
#endif
#if (defined(_ISO_REAL64) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL64 != _DOUBLE_DEFAULT_KIND))
     module procedure relatively_near_real64
#endif
     ! arrays — 32-bit
     module procedure relatively_near_real_1d
     module procedure relatively_near_real_2d
     module procedure relatively_near_real_3d
     module procedure relatively_near_real_4d
     ! arrays — 64-bit
     module procedure relatively_near_double_1d
     module procedure relatively_near_double_2d
     module procedure relatively_near_double_3d
     module procedure relatively_near_double_4d
#if (defined(_ISO_REAL32) && (_ISO_REAL32 != _REAL_DEFAULT_KIND) && (_ISO_REAL32 != _DOUBLE_DEFAULT_KIND))
     module procedure relatively_near_real32_1d
     module procedure relatively_near_real32_2d
     module procedure relatively_near_real32_3d
     module procedure relatively_near_real32_4d
#endif
#if (defined(_ISO_REAL64) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL64 != _DOUBLE_DEFAULT_KIND))
     module procedure relatively_near_real64_1d
     module procedure relatively_near_real64_2d
     module procedure relatively_near_real64_3d
     module procedure relatively_near_real64_4d
#endif
  end interface relatively_near


contains

  !=============================================================
  ! Constructors
  !=============================================================

  function relatively_near_real(value, tolerance) result(matcher)
    type(IsRelativelyNear_32) :: matcher
    real, intent(in) :: value, tolerance
    matcher%value     = value
    matcher%tolerance = tolerance
  end function relatively_near_real

  function relatively_near_double(value, tolerance) result(matcher)
    type(IsRelativelyNear_64) :: matcher
    real(kind=REAL64), intent(in) :: value, tolerance
    matcher%value     = value
    matcher%tolerance = tolerance
  end function relatively_near_double

#if (defined(_ISO_REAL32) && (_ISO_REAL32 != _REAL_DEFAULT_KIND) && (_ISO_REAL32 != _DOUBLE_DEFAULT_KIND))
  function relatively_near_real32(value, tolerance) result(matcher)
    type(IsRelativelyNear_32) :: matcher
    real(kind=REAL32), intent(in) :: value, tolerance
    matcher%value     = real(value)
    matcher%tolerance = real(tolerance)
  end function relatively_near_real32
#endif

#if (defined(_ISO_REAL64) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL64 != _DOUBLE_DEFAULT_KIND))
  function relatively_near_real64(value, tolerance) result(matcher)
    type(IsRelativelyNear_64) :: matcher
    real(kind=REAL64), intent(in) :: value, tolerance
    matcher%value     = real(value, kind=REAL64)
    matcher%tolerance = real(tolerance, kind=REAL64)
  end function relatively_near_real64
#endif

  ! --- array constructors returning IsRelativelyNear_32 ---

  function relatively_near_real_1d(value, tolerance) result(matcher)
    type(IsRelativelyNear_32) :: matcher
    real, intent(in) :: value(:), tolerance
    matcher%tolerance = tolerance
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real_1d

  function relatively_near_real_2d(value, tolerance) result(matcher)
    type(IsRelativelyNear_32) :: matcher
    real, intent(in) :: value(:,:), tolerance
    matcher%tolerance = tolerance
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real_2d

  function relatively_near_real_3d(value, tolerance) result(matcher)
    type(IsRelativelyNear_32) :: matcher
    real, intent(in) :: value(:,:,:), tolerance
    matcher%tolerance = tolerance
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real_3d

  function relatively_near_real_4d(value, tolerance) result(matcher)
    type(IsRelativelyNear_32) :: matcher
    real, intent(in) :: value(:,:,:,:), tolerance
    matcher%tolerance = tolerance
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real_4d

  ! --- array constructors returning IsRelativelyNear_64 ---

  function relatively_near_double_1d(value, tolerance) result(matcher)
    type(IsRelativelyNear_64) :: matcher
    real(kind=REAL64), intent(in) :: value(:), tolerance
    matcher%tolerance = tolerance
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_double_1d

  function relatively_near_double_2d(value, tolerance) result(matcher)
    type(IsRelativelyNear_64) :: matcher
    real(kind=REAL64), intent(in) :: value(:,:), tolerance
    matcher%tolerance = tolerance
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_double_2d

  function relatively_near_double_3d(value, tolerance) result(matcher)
    type(IsRelativelyNear_64) :: matcher
    real(kind=REAL64), intent(in) :: value(:,:,:), tolerance
    matcher%tolerance = tolerance
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_double_3d

  function relatively_near_double_4d(value, tolerance) result(matcher)
    type(IsRelativelyNear_64) :: matcher
    real(kind=REAL64), intent(in) :: value(:,:,:,:), tolerance
    matcher%tolerance = tolerance
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_double_4d

#if (defined(_ISO_REAL32) && (_ISO_REAL32 != _REAL_DEFAULT_KIND) && (_ISO_REAL32 != _DOUBLE_DEFAULT_KIND))
  function relatively_near_real32_1d(value, tolerance) result(matcher)
    type(IsRelativelyNear_32) :: matcher
    real(kind=REAL32), intent(in) :: value(:), tolerance
    matcher%tolerance = real(tolerance)
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real32_1d

  function relatively_near_real32_2d(value, tolerance) result(matcher)
    type(IsRelativelyNear_32) :: matcher
    real(kind=REAL32), intent(in) :: value(:,:), tolerance
    matcher%tolerance = real(tolerance)
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real32_2d

  function relatively_near_real32_3d(value, tolerance) result(matcher)
    type(IsRelativelyNear_32) :: matcher
    real(kind=REAL32), intent(in) :: value(:,:,:), tolerance
    matcher%tolerance = real(tolerance)
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real32_3d

  function relatively_near_real32_4d(value, tolerance) result(matcher)
    type(IsRelativelyNear_32) :: matcher
    real(kind=REAL32), intent(in) :: value(:,:,:,:), tolerance
    matcher%tolerance = real(tolerance)
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real32_4d
#endif

#if (defined(_ISO_REAL64) && (_ISO_REAL64 != _REAL_DEFAULT_KIND) && (_ISO_REAL64 != _DOUBLE_DEFAULT_KIND))
  function relatively_near_real64_1d(value, tolerance) result(matcher)
    type(IsRelativelyNear_64) :: matcher
    real(kind=REAL64), intent(in) :: value(:), tolerance
    matcher%tolerance = real(tolerance, kind=REAL64)
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real64_1d

  function relatively_near_real64_2d(value, tolerance) result(matcher)
    type(IsRelativelyNear_64) :: matcher
    real(kind=REAL64), intent(in) :: value(:,:), tolerance
    matcher%tolerance = real(tolerance, kind=REAL64)
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real64_2d

  function relatively_near_real64_3d(value, tolerance) result(matcher)
    type(IsRelativelyNear_64) :: matcher
    real(kind=REAL64), intent(in) :: value(:,:,:), tolerance
    matcher%tolerance = real(tolerance, kind=REAL64)
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real64_3d

  function relatively_near_real64_4d(value, tolerance) result(matcher)
    type(IsRelativelyNear_64) :: matcher
    real(kind=REAL64), intent(in) :: value(:,:,:,:), tolerance
    matcher%tolerance = real(tolerance, kind=REAL64)
    matcher%array_value = ArrayWrapper(value)
  end function relatively_near_real64_4d
#endif


  !=============================================================
  ! IsRelativelyNear_32 implementations
  !=============================================================

  logical function matches_safely_32(this, actual_value)
    class(IsRelativelyNear_32), intent(in) :: this
    class(*), intent(in) :: actual_value

    if (allocated(this%array_value)) then
       select type (a => this%array_value)
       type is (ArrayWrapper_1d)
          matches_safely_32 = this%matches_array_1d(a%items, actual_value)
       type is (ArrayWrapper_2d)
          matches_safely_32 = this%matches_array_2d(a%items, actual_value)
       type is (ArrayWrapper_3d)
          matches_safely_32 = this%matches_array_3d(a%items, actual_value)
       type is (ArrayWrapper_4d)
          matches_safely_32 = this%matches_array_4d(a%items, actual_value)
       class default
          matches_safely_32 = .false.
       end select
       return
    end if

    select type (actual_value)
    type is (real(kind=REAL32))
       matches_safely_32 = this%delta(actual_value) <= this%tolerance
    type is (real(kind=REAL64))
       matches_safely_32 = this%delta(real(actual_value, kind=REAL32)) <= this%tolerance
    end select

  end function matches_safely_32


  logical function matches_array_1d_32(this, expected_items, actual_value)
    class(IsRelativelyNear_32), intent(in) :: this
    class(*), intent(in) :: expected_items(:)
    class(*), intent(in) :: actual_value

    integer :: i, n_items
    real(kind=REAL32) :: e_val, a_val

    select type (a => actual_value)
    type is (ArrayWrapper_1d)
       n_items = size(expected_items)
       if (size(a%items) == n_items) then
          do i = 1, n_items
             select type (e => expected_items(i))
             type is (real(kind=REAL32))
                e_val = e
             class default
                matches_array_1d_32 = .false.
                return
             end select
             select type (av => a%items(i))
             type is (real(kind=REAL32))
                a_val = av
             type is (real(kind=REAL64))
                a_val = real(av, kind=REAL32)
             class default
                matches_array_1d_32 = .false.
                return
             end select
             if (abs(a_val - e_val) / abs(e_val) > this%tolerance) then
                matches_array_1d_32 = .false.
                return
             end if
          end do
          matches_array_1d_32 = .true.
       else
          matches_array_1d_32 = .false.
       end if
    class default
       matches_array_1d_32 = .false.
    end select

  end function matches_array_1d_32


  logical function matches_array_2d_32(this, expected_items, actual_value)
    class(IsRelativelyNear_32), intent(in) :: this
    class(*), intent(in) :: expected_items(:,:)
    class(*), intent(in) :: actual_value

    integer :: i, j
    real(kind=REAL32) :: e_val, a_val

    select type (a => actual_value)
    type is (ArrayWrapper_2d)
       if (all(shape(a%items) == shape(expected_items))) then
          do j = 1, size(a%items,2)
             do i = 1, size(a%items,1)
                select type (e => expected_items(i,j))
                type is (real(kind=REAL32))
                   e_val = e
                class default
                   matches_array_2d_32 = .false.
                   return
                end select
                select type (av => a%items(i,j))
                type is (real(kind=REAL32))
                   a_val = av
                type is (real(kind=REAL64))
                   a_val = real(av, kind=REAL32)
                class default
                   matches_array_2d_32 = .false.
                   return
                end select
                if (abs(a_val - e_val) / abs(e_val) > this%tolerance) then
                   matches_array_2d_32 = .false.
                   return
                end if
             end do
          end do
          matches_array_2d_32 = .true.
       else
          matches_array_2d_32 = .false.
       end if
    class default
       matches_array_2d_32 = .false.
    end select

  end function matches_array_2d_32


  logical function matches_array_3d_32(this, expected_items, actual_value)
    class(IsRelativelyNear_32), intent(in) :: this
    class(*), intent(in) :: expected_items(:,:,:)
    class(*), intent(in) :: actual_value

    integer :: i, j, k
    real(kind=REAL32) :: e_val, a_val

    select type (a => actual_value)
    type is (ArrayWrapper_3d)
       if (all(shape(a%items) == shape(expected_items))) then
          do k = 1, size(a%items,3)
             do j = 1, size(a%items,2)
                do i = 1, size(a%items,1)
                   select type (e => expected_items(i,j,k))
                   type is (real(kind=REAL32))
                      e_val = e
                   class default
                      matches_array_3d_32 = .false.
                      return
                   end select
                   select type (av => a%items(i,j,k))
                   type is (real(kind=REAL32))
                      a_val = av
                   type is (real(kind=REAL64))
                      a_val = real(av, kind=REAL32)
                   class default
                      matches_array_3d_32 = .false.
                      return
                   end select
                   if (abs(a_val - e_val) / abs(e_val) > this%tolerance) then
                      matches_array_3d_32 = .false.
                      return
                   end if
                end do
             end do
          end do
          matches_array_3d_32 = .true.
       else
          matches_array_3d_32 = .false.
       end if
    class default
       matches_array_3d_32 = .false.
    end select

  end function matches_array_3d_32


  logical function matches_array_4d_32(this, expected_items, actual_value)
    class(IsRelativelyNear_32), intent(in) :: this
    class(*), intent(in) :: expected_items(:,:,:,:)
    class(*), intent(in) :: actual_value

    integer :: i, j, k, l
    real(kind=REAL32) :: e_val, a_val

    select type (a => actual_value)
    type is (ArrayWrapper_4d)
       if (all(shape(a%items) == shape(expected_items))) then
          do l = 1, size(a%items,4)
             do k = 1, size(a%items,3)
                do j = 1, size(a%items,2)
                   do i = 1, size(a%items,1)
                      select type (e => expected_items(i,j,k,l))
                      type is (real(kind=REAL32))
                         e_val = e
                      class default
                         matches_array_4d_32 = .false.
                         return
                      end select
                      select type (av => a%items(i,j,k,l))
                      type is (real(kind=REAL32))
                         a_val = av
                      type is (real(kind=REAL64))
                         a_val = real(av, kind=REAL32)
                      class default
                         matches_array_4d_32 = .false.
                         return
                      end select
                      if (abs(a_val - e_val) / abs(e_val) > this%tolerance) then
                         matches_array_4d_32 = .false.
                         return
                      end if
                   end do
                end do
             end do
          end do
          matches_array_4d_32 = .true.
       else
          matches_array_4d_32 = .false.
       end if
    class default
       matches_array_4d_32 = .false.
    end select

  end function matches_array_4d_32


  real function delta_32(this, actual)
    class(IsRelativelyNear_32), intent(in) :: this
    real, intent(in) :: actual
    delta_32 = abs(actual - this%value) / abs(this%value)
  end function delta_32


  subroutine describe_mismatch_safely_32(this, actual, description)
    class(IsRelativelyNear_32), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    real :: d

    select type (actual)
    type is (real(kind=REAL32))
       d = this%delta(actual)
       call description%append_value(actual)
       call description%append_text(" has a relative error of ")
       call description%append_value(d)
       call description%append_text(" which exceeds the tolerance by ")
       call description%append_value(d - this%tolerance)
    type is (real(kind=REAL64))
       d = this%delta(real(actual, kind=REAL32))
       call description%append_value(actual)
       call description%append_text(" has a relative error of ")
       call description%append_value(d)
       call description%append_text(" which exceeds the tolerance by ")
       call description%append_value(d - this%tolerance)
    end select

  end subroutine describe_mismatch_safely_32


  logical function expects_type_of_32(this, actual) result(supported)
    class(IsRelativelyNear_32), intent(in) :: this
    class(*), intent(in) :: actual

    _UNUSED_DUMMY(this)

    select type (actual)
    type is (real(kind=REAL32))
       supported = .true.
    type is (real(kind=REAL64))
       supported = .true.
    class is (ArrayWrapper_1d)
       supported = allocated(this%array_value)
    class is (ArrayWrapper_2d)
       supported = allocated(this%array_value)
    class is (ArrayWrapper_3d)
       supported = allocated(this%array_value)
    class is (ArrayWrapper_4d)
       supported = allocated(this%array_value)
    class default
       supported = .false.
    end select

  end function expects_type_of_32


  subroutine describe_to_32(this, description)
    class(IsRelativelyNear_32), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description

    if (allocated(this%array_value)) then
       call description%append_text("an array where each element's relative error is less than ")
       call description%append_value(this%tolerance)
    else
       call description%append_text("a numeric value whose relative error from ")
       call description%append_value(this%value)
       call description%append_text(" is less than ")
       call description%append_value(this%tolerance)
    end if

  end subroutine describe_to_32


  !=============================================================
  ! IsRelativelyNear_64 implementations
  !=============================================================

  logical function matches_safely_64(this, actual_value)
    class(IsRelativelyNear_64), intent(in) :: this
    class(*), intent(in) :: actual_value

    if (allocated(this%array_value)) then
       select type (a => this%array_value)
       type is (ArrayWrapper_1d)
          matches_safely_64 = this%matches_array_1d(a%items, actual_value)
       type is (ArrayWrapper_2d)
          matches_safely_64 = this%matches_array_2d(a%items, actual_value)
       type is (ArrayWrapper_3d)
          matches_safely_64 = this%matches_array_3d(a%items, actual_value)
       type is (ArrayWrapper_4d)
          matches_safely_64 = this%matches_array_4d(a%items, actual_value)
       class default
          matches_safely_64 = .false.
       end select
       return
    end if

    select type (actual_value)
    type is (real(kind=REAL32))
       matches_safely_64 = this%delta(real(actual_value, kind=REAL64)) <= this%tolerance
    type is (real(kind=REAL64))
       matches_safely_64 = this%delta(actual_value) <= this%tolerance
    end select

  end function matches_safely_64


  logical function matches_array_1d_64(this, expected_items, actual_value)
    class(IsRelativelyNear_64), intent(in) :: this
    class(*), intent(in) :: expected_items(:)
    class(*), intent(in) :: actual_value

    integer :: i, n_items
    real(kind=REAL64) :: e_val, a_val

    select type (a => actual_value)
    type is (ArrayWrapper_1d)
       n_items = size(expected_items)
       if (size(a%items) == n_items) then
          do i = 1, n_items
             select type (e => expected_items(i))
             type is (real(kind=REAL64))
                e_val = e
             class default
                matches_array_1d_64 = .false.
                return
             end select
             select type (av => a%items(i))
             type is (real(kind=REAL64))
                a_val = av
             class default
                matches_array_1d_64 = .false.
                return
             end select
             if (abs(a_val - e_val) / abs(e_val) > this%tolerance) then
                matches_array_1d_64 = .false.
                return
             end if
          end do
          matches_array_1d_64 = .true.
       else
          matches_array_1d_64 = .false.
       end if
    class default
       matches_array_1d_64 = .false.
    end select

  end function matches_array_1d_64


  logical function matches_array_2d_64(this, expected_items, actual_value)
    class(IsRelativelyNear_64), intent(in) :: this
    class(*), intent(in) :: expected_items(:,:)
    class(*), intent(in) :: actual_value

    integer :: i, j
    real(kind=REAL64) :: e_val, a_val

    select type (a => actual_value)
    type is (ArrayWrapper_2d)
       if (all(shape(a%items) == shape(expected_items))) then
          do j = 1, size(a%items,2)
             do i = 1, size(a%items,1)
                select type (e => expected_items(i,j))
                type is (real(kind=REAL64))
                   e_val = e
                class default
                   matches_array_2d_64 = .false.
                   return
                end select
                select type (av => a%items(i,j))
                type is (real(kind=REAL64))
                   a_val = av
                class default
                   matches_array_2d_64 = .false.
                   return
                end select
                if (abs(a_val - e_val) / abs(e_val) > this%tolerance) then
                   matches_array_2d_64 = .false.
                   return
                end if
             end do
          end do
          matches_array_2d_64 = .true.
       else
          matches_array_2d_64 = .false.
       end if
    class default
       matches_array_2d_64 = .false.
    end select

  end function matches_array_2d_64


  logical function matches_array_3d_64(this, expected_items, actual_value)
    class(IsRelativelyNear_64), intent(in) :: this
    class(*), intent(in) :: expected_items(:,:,:)
    class(*), intent(in) :: actual_value

    integer :: i, j, k
    real(kind=REAL64) :: e_val, a_val

    select type (a => actual_value)
    type is (ArrayWrapper_3d)
       if (all(shape(a%items) == shape(expected_items))) then
          do k = 1, size(a%items,3)
             do j = 1, size(a%items,2)
                do i = 1, size(a%items,1)
                   select type (e => expected_items(i,j,k))
                   type is (real(kind=REAL64))
                      e_val = e
                   class default
                      matches_array_3d_64 = .false.
                      return
                   end select
                   select type (av => a%items(i,j,k))
                   type is (real(kind=REAL64))
                      a_val = av
                   class default
                      matches_array_3d_64 = .false.
                      return
                   end select
                   if (abs(a_val - e_val) / abs(e_val) > this%tolerance) then
                      matches_array_3d_64 = .false.
                      return
                   end if
                end do
             end do
          end do
          matches_array_3d_64 = .true.
       else
          matches_array_3d_64 = .false.
       end if
    class default
       matches_array_3d_64 = .false.
    end select

  end function matches_array_3d_64


  logical function matches_array_4d_64(this, expected_items, actual_value)
    class(IsRelativelyNear_64), intent(in) :: this
    class(*), intent(in) :: expected_items(:,:,:,:)
    class(*), intent(in) :: actual_value

    integer :: i, j, k, l
    real(kind=REAL64) :: e_val, a_val

    select type (a => actual_value)
    type is (ArrayWrapper_4d)
       if (all(shape(a%items) == shape(expected_items))) then
          do l = 1, size(a%items,4)
             do k = 1, size(a%items,3)
                do j = 1, size(a%items,2)
                   do i = 1, size(a%items,1)
                      select type (e => expected_items(i,j,k,l))
                      type is (real(kind=REAL64))
                         e_val = e
                      class default
                         matches_array_4d_64 = .false.
                         return
                      end select
                      select type (av => a%items(i,j,k,l))
                      type is (real(kind=REAL64))
                         a_val = av
                      class default
                         matches_array_4d_64 = .false.
                         return
                      end select
                      if (abs(a_val - e_val) / abs(e_val) > this%tolerance) then
                         matches_array_4d_64 = .false.
                         return
                      end if
                   end do
                end do
             end do
          end do
          matches_array_4d_64 = .true.
       else
          matches_array_4d_64 = .false.
       end if
    class default
       matches_array_4d_64 = .false.
    end select

  end function matches_array_4d_64


  real(kind=REAL64) function delta_64(this, actual)
    class(IsRelativelyNear_64), intent(in) :: this
    real(kind=REAL64), intent(in) :: actual
    delta_64 = abs(actual - this%value) / abs(this%value)
  end function delta_64


  subroutine describe_mismatch_safely_64(this, actual, description)
    class(IsRelativelyNear_64), intent(in) :: this
    class(*), intent(in) :: actual
    class(MatcherDescription), intent(inout) :: description

    real(kind=REAL64) :: d

    select type (actual)
    type is (real(kind=REAL32))
       d = this%delta(real(actual, kind=REAL64))
       call description%append_value(actual)
       call description%append_text(" has a relative error of ")
       call description%append_value(d)
       call description%append_text(" which exceeds the tolerance by ")
       call description%append_value(d - this%tolerance)
    type is (real(kind=REAL64))
       d = this%delta(actual)
       call description%append_value(actual)
       call description%append_text(" has a relative error of ")
       call description%append_value(d)
       call description%append_text(" which exceeds the tolerance by ")
       call description%append_value(d - this%tolerance)
    end select

  end subroutine describe_mismatch_safely_64


  logical function expects_type_of_64(this, actual) result(supported)
    class(IsRelativelyNear_64), intent(in) :: this
    class(*), intent(in) :: actual

    _UNUSED_DUMMY(this)

    select type (actual)
    type is (real(kind=REAL32))
       supported = .true.
    type is (real(kind=REAL64))
       supported = .true.
    class is (ArrayWrapper_1d)
       supported = allocated(this%array_value)
    class is (ArrayWrapper_2d)
       supported = allocated(this%array_value)
    class is (ArrayWrapper_3d)
       supported = allocated(this%array_value)
    class is (ArrayWrapper_4d)
       supported = allocated(this%array_value)
    class default
       supported = .false.
    end select

  end function expects_type_of_64


  subroutine describe_to_64(this, description)
    class(IsRelativelyNear_64), intent(in) :: this
    class(MatcherDescription), intent(inout) :: description

    if (allocated(this%array_value)) then
       call description%append_text("an array where each element's relative error is less than ")
       call description%append_value(this%tolerance)
    else
       call description%append_text("a numeric value whose relative error from ")
       call description%append_value(this%value)
       call description%append_text(" is less than ")
       call description%append_value(this%tolerance)
    end if

  end subroutine describe_to_64

end module pf_IsRelativelyNear
