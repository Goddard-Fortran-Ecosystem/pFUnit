module pf_String_mod
  implicit none
  private

  public :: String 
  public :: to_string
  
  type :: String
     character(:), allocatable :: s
  end type String

contains

  function to_string(this) result(s)
    character(:), allocatable :: s
    type (String), intent(in) :: this

    s = this%s
    
  end function to_string

end module pf_String_mod
