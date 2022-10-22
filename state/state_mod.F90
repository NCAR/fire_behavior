  module state_mod

    implicit none

    private

    public :: state_t

    type :: state_t
      real, dimension (:, :), allocatable :: lats, lons, elevations, dz_dxs, dz_dys, fuel_cats
      real :: dx, dy, cen_lat, cen_lon
      integer :: ids, ide, jds, jde
    end type state_t

  end module state_mod
