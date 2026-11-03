program wrf_wind_unit

  ! Check horizontal mapping of coupled WRF winds and interface heights before
  ! vertical sampling. Different source-column heights make the order matter.
  ! Tiles with nonunit bounds must be filled without changing their halos.
  ! One-tile and four-tile results must agree for both horizontal methods.
  use wrf_mod, only : Interp_wrfwinds_to_cfbm
  use proj_lc_mod, only : proj_lc_t
  use interp_mod, only : HINTERP_NEAREST, HINTERP_BILINEAR

  implicit none

  type(proj_lc_t) :: projection
  real :: u(2,4,3), v(2,4,3), z(2,4,3)
  real :: lat(-1:3,4:7), lon(-1:3,4:7), z0(-1:3,4:7), zsf(-1:3,4:7)
  real :: uf(-1:3,4:7), vf(-1:3,4:7), latitude, longitude
  real :: u_single(-1:3,4:7), v_single(-1:3,4:7), factor(-1:3,4:7)
  real, parameter :: interfaces(4) = [0.0, 20.0, 60.0, 120.0]
  integer, parameter :: i_start(4) = [0, 1, 0, 1], i_end(4) = [0, 2, 0, 2]
  integer, parameter :: j_start(4) = [5, 5, 6, 6], j_end(4) = [5, 5, 6, 6]
  integer :: i, j, k, method

  ! The projection centre is halfway between columns 1 and 2, at row 2.
  ! Their first mass levels are 10 and 20 m AGL, so mapping gives 15 m.
  projection = proj_lc_t(40.0, -105.0, 100.0, 100.0, -105.0, 30.0, 60.0, 2, 3)
  u(1,:,:) = 4.0
  u(2,:,:) = 16.0
  v = -2.0
  do k = 1, 4
    z(1,k,:) = 100.0 + interfaces(k)
    z(2,k,:) = 300.0 + 2.0 * interfaces(k)
  end do
  lat = 40.0
  lon = -105.0
  ! Vary local roughness so concurrent vertical samples have different values.
  ! This also exercises thread-private wind temporaries in the OpenMP loop.
  do j = 4, 7
    do i = -1, 3
      z0(i,j) = 0.10 + 0.01 * (i + 1) + 0.02 * (j - 4)
    end do
  end do
  ! Supply fire terrain in meters ASL; this input currently leaves the
  ! existing sampling relative to mapped atmospheric terrain unchanged.
  zsf = 200.0

  do method = HINTERP_NEAREST, HINTERP_BILINEAR
    if (method == HINTERP_BILINEAR) then
      lat = 40.0
      lon = -105.0
      ! At the centre, mapped U=10 m/s and the first mass level is 15 m AGL.
      factor = log(6.0 / z0) / log(15.0 / z0)
    else
      ! Nearest selection must reach row 3 of a 2-by-3 source grid.
      ! Its distinct wind catches a row index limited by the wrong bound.
      call projection%Calc_latlon(2.0, 3.0, latitude, longitude)
      lat = latitude
      lon = longitude
      u(2,:,3) = 24.0
      factor = log(6.0 / z0) / log(20.0 / z0)
    end if
    uf = -999.0
    vf = -999.0
    u_single = -999.0
    v_single = -999.0

    call Interp_wrfwinds_to_cfbm(u, v, z, 1, 2, 1, 4, 1, 3, -1, 3, 4, 7, &
        1, 4, 1, [0], [2], [5], [6], lat, lon, projection, z0, zsf, &
        .false., 10.0, 6.0, u_single, v_single, method)
    call Interp_wrfwinds_to_cfbm(u, v, z, 1, 2, 1, 4, 1, 3, -1, 3, 4, 7, &
        1, 4, 4, i_start, i_end, j_start, j_end, lat, lon, projection, z0, zsf, &
        .false., 10.0, 6.0, uf, vf, method)

    if (method == HINTERP_BILINEAR) then
      if (any(abs(uf(0:2,5:6) - 10.0 * factor(0:2,5:6)) > 2.0e-6)) error stop 'bilinear U differs'
    else
      if (any(abs(uf(0:2,5:6) - 24.0 * factor(0:2,5:6)) > 2.0e-6)) error stop 'nearest-neighbour U differs'
    end if
    if (any(abs(vf(0:2,5:6) + 2.0 * factor(0:2,5:6)) > 1.0e-6)) error stop 'WRF V mapping differs'
    if (any(uf /= u_single) .or. any(vf /= v_single)) error stop 'wind depends on tile layout'
    call check_halos()
  end do

  ! A partial tile list must only update its two cells. Both horizontal and
  ! vertical sampling must honor the tile list rather than its bounding patch.
  uf = -999.0
  vf = -999.0
  call Interp_wrfwinds_to_cfbm(u, v, z, 1, 2, 1, 4, 1, 3, -1, 3, 4, 7, &
      1, 4, 2, [0, 2], [0, 2], [5, 6], [5, 6], lat, lon, projection, z0, zsf, &
      .false., 10.0, 6.0, uf, vf, HINTERP_BILINEAR)
  if (uf(0,5) /= u_single(0,5) .or. uf(2,6) /= u_single(2,6)) error stop 'tile subset U differs'
  if (vf(0,5) /= v_single(0,5) .or. vf(2,6) /= v_single(2,6)) error stop 'tile subset V differs'
  uf(0,5) = -999.0
  uf(2,6) = -999.0
  vf(0,5) = -999.0
  vf(2,6) = -999.0
  if (any(uf /= -999.0) .or. any(vf /= -999.0)) error stop 'cells outside tiles changed'

  print *, 'WRF wind interpolation checks passed'

contains

  subroutine check_halos()

    if (any(uf(-1,:) /= -999.0) .or. any(uf(3,:) /= -999.0) .or. &
        any(uf(:,4) /= -999.0) .or. any(uf(:,7) /= -999.0)) error stop 'WRF U halo changed'
    if (any(vf(-1,:) /= -999.0) .or. any(vf(3,:) /= -999.0) .or. &
        any(vf(:,4) /= -999.0) .or. any(vf(:,7) /= -999.0)) error stop 'WRF V halo changed'

  end subroutine check_halos

end program wrf_wind_unit
