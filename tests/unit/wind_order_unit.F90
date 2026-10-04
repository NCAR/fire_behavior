program wind_order_unit

  use, intrinsic :: iso_fortran_env, only : REAL64
  use wrfdata_mod, only : wrfdata_t, G
  use namelist_mod, only : namelist_t

  implicit none

  type(wrfdata_t) :: atmosphere
  type(namelist_t) :: config
  real :: lat(3,2), lon(3,2), roughness(3,2), u(3,2), v(3,2)
  real, parameter :: interfaces(5) = [0.0, 20.0, 60.0, 120.0, 200.0]
  real(REAL64) :: factor, expected, old_order
  integer :: k

  ! The projection centre lies halfway between four atmospheric columns.
  ! A nonsquare fire patch also exercises every output row and column.
  atmosphere%ids = 1
  atmosphere%ide = 3
  atmosphere%jds = 1
  atmosphere%jde = 3
  atmosphere%kds = 1
  atmosphere%kde = 5
  atmosphere%cen_lat = 40.0
  atmosphere%cen_lon = -105.0
  atmosphere%stand_lon = -105.0
  atmosphere%truelat1 = 30.0
  atmosphere%truelat2 = 60.0
  atmosphere%dx = 100.0
  atmosphere%dy = 100.0
  allocate(atmosphere%lats(2,2), atmosphere%lons(2,2))
  allocate(atmosphere%u3d(2,2,4), atmosphere%v3d(2,2,4), atmosphere%phl(2,2,5))
  atmosphere%u3d(1,:,:) = 8.0
  atmosphere%u3d(2,:,:) = 12.0
  atmosphere%v3d = -2.0
  do k = 1, 5
    atmosphere%phl(:,:,k) = G * interfaces(k)
  end do
  lat = 40.0
  lon = -105.0
  roughness = 0.15
  config%hinterp_opt = 2
  config%fire_wind_height = 6.096
  config%fire_lsm_zcoupling = .false.

  call atmosphere%Interp_winds2grid(lat, lon, roughness, 1, 3, 1, 2, &
      1, 3, 1, 2, 1, [1], [3], [1], [2], config, u, v)

  ! Horizontal-first sampling uses mean U=10 and remapped z0=0.15 m.
  ! Vertical-first sampling with source z0=0.05/0.25 m gives a different wind.
  factor = log(6.096_REAL64 / 0.15_REAL64) / log(10.0_REAL64 / 0.15_REAL64)
  expected = 10.0_REAL64 * factor
  old_order = 0.5_REAL64 * (8.0_REAL64 * log(6.096_REAL64 / 0.05_REAL64) / log(10.0_REAL64 / 0.05_REAL64) + &
      12.0_REAL64 * log(6.096_REAL64 / 0.25_REAL64) / log(10.0_REAL64 / 0.25_REAL64))
  if (maxval(abs(real(u,REAL64) - expected)) > 2.0e-6_REAL64) error stop 'horizontal-first U differs'
  if (maxval(abs(real(v,REAL64) + 2.0_REAL64 * factor)) > 1.0e-6_REAL64) error stop 'horizontal-first V differs'
  if (abs(expected - old_order) < 0.01_REAL64) error stop 'fixture does not distinguish interpolation order'
  if (allocated(atmosphere%u3d) .or. allocated(atmosphere%v3d) .or. allocated(atmosphere%phl)) &
      error stop 'atmospheric profiles were not released'

  print *, 'horizontal-first wind checks passed'

end program wind_order_unit
