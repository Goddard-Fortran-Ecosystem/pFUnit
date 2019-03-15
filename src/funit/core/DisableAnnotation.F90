module pf_DisableAnnotation
  use pf_TestAnnotation
  implicit none
  private

  public :: DisableAnnotation ! singleton
  public :: Disable
  
  type, extends(TestAnnotation) :: DisableAnnotation
   contains
     procedure, nopass :: type_name
  end type DisableAnnotation
  
  ! Instance is public, type is private; semi-singleton
  type (DisableAnnotation) :: Disable

contains


   function type_name()
      character(:), allocatable :: type_name

      type_name = 'Disable'
      
    end function type_name
  

end module pf_DisableAnnotation
