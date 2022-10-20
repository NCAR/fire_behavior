  module state_mod

    implicit none

    private

    public :: state_t

    type :: state_t
      real, dimension (:, :), allocatable :: lats, lons, elevations, dz_dxs, dz_dys, fuel_cats
      real :: dx, dy
      integer :: ids, ide, jds, jde
!      character (len = 8) :: proj
    end type state_t

  end module state_mod
