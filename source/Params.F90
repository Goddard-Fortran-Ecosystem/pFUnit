
module Params_mod

  implicit none

  integer, parameter :: R32 = selected_real_kind(p=6)
  integer, parameter :: R64 = selected_real_kind(p=14)
  integer, parameter :: C32 = selected_real_kind(p=6)
  integer, parameter :: C64 = selected_real_kind(p=14)

  integer, parameter :: NEQP=0, EQP=1, GTP=2, GEP=3, LTP=4, LEP=5, &
  &  RELEQP=6

contains

end module Params_mod

