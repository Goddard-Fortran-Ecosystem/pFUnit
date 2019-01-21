! This module allows pFUnit to treat arrays as single objects which
! greatly simplifies the overloading of hamcrest matcher interfaces.
! Once SELECT RANK (Fortran 2015) is widely available, this should
! be re-engineered.


module pf_Array_mod
  implicit none
  private

  public :: internal_array
  public :: internal_array_1d
  public :: internal_array_2d
  public :: internal_array_3d

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

end module pf_Array_mod
