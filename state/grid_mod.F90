  module grid_mod

    implicit none

    private

    public :: grid_t

    type :: grid_t
      real, dimension (:, :), allocatable :: lats, lons, elevation, dz_dx, dz_dy, nfuel_cat
      character (len = 8) :: proj
      real :: dx, dy
    end type grid_t

  end module grid_mod
