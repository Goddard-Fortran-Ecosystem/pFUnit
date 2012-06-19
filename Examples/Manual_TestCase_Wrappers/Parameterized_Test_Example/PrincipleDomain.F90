! Principle domain is defined for latitude in [-90,90]  and for
! longitude in [0, 360)
! Note that for latitude at eithe pole, the longitude is arbitrary.

module PrincipleDomain_mod

contains

   function principleDomain(latitude, longitude) result(latLon)
      real :: latLon(2)
      real, intent(in) :: latitude
      real, intent(in) :: longitude

      real :: lat
      real :: lon

      lon = longitude
      lat = abs(latitude) ! fix sign later

      select case (floor(lat / 90))
      case (0)
      case (1,2)
         lat = 180 - lat
         lon = lon + 180
      case (3)
         lat = lat - 360
      end select
      
      ! fix sign of lat
      lat = lat * sign(1.0,latitude)

      if (lon >= 360) then
         lon = lon - 360 * floor(lon/360)
      else if (lon < 0) then
         lon = lon + 360 * ceiling(-lon / 360)
      end if

      latlon = [lat, lon]

   end function principleDomain

end module PrincipleDomain_mod
