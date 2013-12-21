!-------------------------------------------------------------------------------
! NASA/GSFC, Software Integration & Visualization Office, Code 610.3
!-------------------------------------------------------------------------------
!  MODULE: ParameterizedTestCase
!
!> @brief
!! <BriefDescription>
!!
!! @author
!! Tom Clune,  NASA/GSFC 
!!
!! @date
!! 07 Nov 2013
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 07 Nov 2013 - Added the prologue for the compliance with Doxygen. 
!
!-------------------------------------------------------------------------------
module ParameterizedTestCase_mod
   use TestCase_mod
   implicit none
   private
   
   public :: AbstractTestParameter
   public :: ParameterizedTestCase
   public :: MAX_LEN_LABEL

   ! Currently the following type serves little purpose.  
   ! In the future, the hope is to add some generic services.
   type, abstract :: AbstractTestParameter
   end type AbstractTestParameter

   integer, parameter :: MAX_LEN_LABEL = 32
   type, abstract, extends(TestCase) :: ParameterizedTestCase
   contains
      procedure :: getName ! override from TestCase
      procedure(getParameterString), deferred :: getParameterString
   end type ParameterizedTestCase

   abstract interface
      function getParameterString(this) result(label)
         import ParameterizedTestCase
         class (ParameterizedTestCase), intent(in) :: this
         character(len=:), allocatable :: label
      end function getParameterString
   end interface

contains

   function getName(this) result(name)
      class (ParameterizedTestCase), intent(in) :: this
      character(:), allocatable :: name

      name = trim(this%baseName()) // '[' // trim(this%getParameterString()) // ']'

   end function getName

end module ParameterizedTestCase_mod
