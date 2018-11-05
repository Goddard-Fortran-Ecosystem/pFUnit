program main
   use, intrinsic :: iso_fortran_env, only: _KIND
   use, intrinsic :: ieee_arithmetic
   real(kind=_KIND) :: x

   if (.not. IEEE_SUPPORT_DATATYPE (x)) then
      stop 1
   else
      print*,'T'
   end if

end program main
