! This module allows pFUnit to treat arrays as single objects which
! greatly simplifies the overloading of hamcrest matcher interfaces.
! Once SELECT RANK (Fortran 2015) is widely available, this should
! be re-engineered.


module pf_Array
  implicit none
  private

  public :: wrap_array
  public :: internal_array
  public :: internal_array_1d
  public :: internal_array_2d
  public :: internal_array_3d


  interface wrap_array
     module procedure wrap_1d
     module procedure wrap_2d
     module procedure wrap_3d
  end interface wrap_array
     
  type, abstract :: internal_array
  end type internal_array

  type, extends(internal_array) :: internal_array_1d
     class(*), allocatable :: items(:)
  end type internal_array_1d

  type, extends(internal_array) :: internal_array_2d
     class(*), allocatable :: items(:,:)
  end type internal_array_2d

  type, extends(internal_array) :: internal_array_3d
     class(*), allocatable :: items(:,:,:)
  end type internal_array_3d

contains

  function wrap_1d(items) result(a)
    type(internal_array_1d) :: a
    class(*), intent(in) :: items(:)
    allocate(a%items, source=items)
  end function wrap_1d

  function wrap_2d(items) result(a)
    type(internal_array_2d) :: a
    class(*), intent(in) :: items(:,:)
    a%items = items
  end function wrap_2d
  
  function wrap_3d(items) result(a)
    type(internal_array_3d) :: a
    class(*), intent(in) :: items(:,:,:)
    a%items = items
  end function wrap_3d
  
end module pf_Array
