program wrf_wind_unit

  ! Check horizontal mapping of coupled WRF winds and interface heights before
  ! vertical sampling. Different source-column heights make the order matter.
  ! Tiles with nonunit bounds must be filled without changing their halos.
  ! One-tile and four-tile results must agree for both horizontal methods.
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
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
  integer, parameter :: methods(2) = [HINTERP_NEAREST, HINTERP_BILINEAR]
  integer :: i, j, k, method, method_index

  ! The projection centre is halfway between columns 1 and 2, at row 2.
  ! Their first mass levels are 10 and 20 m AGL, so mapping gives 15 m.
  projection = proj_lc_t(40.0, -105.0, 100.0, 100.0, -105.0, 30.0, 60.0, 2, 3)
  u(1,:,:) = 4.0
  u(2,:,:) = 16.0
  u(2,:,3) = 24.0
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

  do method_index = 1, size(methods)
    method = methods(method_index)
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
      factor = log(6.0 / z0) / log(20.0 / z0)
    end if
    uf = -999.0
    vf = -999.0
    u_single = -999.0
    v_single = -999.0

    call Interp_wrfwinds_to_cfbm(u, v, z, 1, 2, 1, 4, 1, 3, 1, 3, 1, 4, -1, 3, 4, 7, &
        1, 4, 1, [0], [2], [5], [6], lat, lon, projection, z0, zsf, &
        .false., 10.0, 6.0, u_single, v_single, method)
    call Interp_wrfwinds_to_cfbm(u, v, z, 1, 2, 1, 4, 1, 3, 1, 3, 1, 4, -1, 3, 4, 7, &
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

  ! A partial tile list must only update its two output cells. Reinitialize
  ! the bilinear query explicitly so this case is independent of method order.
  lat = 40.0
  lon = -105.0
  factor = log(6.0 / z0) / log(15.0 / z0)
  uf = -999.0
  vf = -999.0
  call Interp_wrfwinds_to_cfbm(u, v, z, 1, 2, 1, 4, 1, 3, 1, 3, 1, 4, -1, 3, 4, 7, &
      1, 4, 2, [0, 2], [0, 2], [5, 6], [5, 6], lat, lon, projection, z0, zsf, &
      .false., 10.0, 6.0, uf, vf, HINTERP_BILINEAR)
  if (.not.all(ieee_is_finite(uf)) .or. .not.all(ieee_is_finite(vf))) error stop 'tile subset wind is not finite'
  if (abs(uf(0,5) - 10.0*factor(0,5)) > 2.0e-6 .or. &
      abs(uf(2,6) - 10.0*factor(2,6)) > 2.0e-6) error stop 'tile subset U differs'
  if (abs(vf(0,5) + 2.0*factor(0,5)) > 1.0e-6 .or. &
      abs(vf(2,6) + 2.0*factor(2,6)) > 1.0e-6) error stop 'tile subset V differs'
  uf(0,5) = -999.0
  uf(2,6) = -999.0
  vf(0,5) = -999.0
  vf(2,6) = -999.0
  if (any(uf /= -999.0) .or. any(vf /= -999.0)) error stop 'cells outside tiles changed'

  ! Full-domain and rank-local source bounds must exclude poisoned storage
  ! outside the physical domain. MPI halo exchange is tested in coupled runs.
  call check_source_bounds(-1, 7, 0, 9, 2, 5, 3, 7)
  call check_source_bounds(3, 7, 0, 9, 2, 5, 3, 7)
  ! Small source extents reduce bilinear mapping to a line or a single column.
  call check_source_bounds(-1, 7, 0, 9, 2, 3, 3, 7)
  call check_source_bounds(-1, 7, 0, 9, 2, 5, 3, 4)
  call check_source_bounds(-1, 7, 0, 9, 2, 3, 3, 4)

  call check_vertical_storage()

  print *, 'WRF wind interpolation checks passed'

contains

  subroutine check_halos()

    if (any(uf(-1,:) /= -999.0) .or. any(uf(3,:) /= -999.0) .or. &
        any(uf(:,4) /= -999.0) .or. any(uf(:,7) /= -999.0)) error stop 'WRF U halo changed'
    if (any(vf(-1,:) /= -999.0) .or. any(vf(3,:) /= -999.0) .or. &
        any(vf(:,4) /= -999.0) .or. any(vf(:,7) /= -999.0)) error stop 'WRF V halo changed'

  end subroutine check_halos


  subroutine check_source_bounds(ims, ime, jms, jme, ids, ide, jds, jde)

    ! A linear wind field has an analytic horizontal result, followed by one
    ! common logarithmic height factor (6 m target, 10 m first mass level).
    ! Quiet NaNs outside physical cells expose even zero-weight ghost reads.
    ! Query all corners and edges, one interior point, and exterior points;
    ! the existing extrapolation policy must use the nearest valid endpoint.
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_finite
    integer, intent(in) :: ims, ime, jms, jme, ids, ide, jds, jde
    real :: us(ims:ime,4,jms:jme), vs(ims:ime,4,jms:jme), zs(ims:ime,4,jms:jme)
    real :: lats(11,1), lons(11,1), roughness(11,1), terrain(11,1), uout(11,1), vout(11,1)
    real :: x(11), y(11), xr, yr, xm, ym, scale, expected_u, expected_v, nan
    integer :: ilo, ihi, jlo, jhi, i, j, k, n, method, method_index

    ilo = max(ims, ids)
    ihi = min(ime, ide-1)
    jlo = max(jms, jds)
    jhi = min(jme, jde-1)
    nan = ieee_value(0.0, ieee_quiet_nan)
    us = nan
    vs = nan
    zs = nan
    do j = jlo, jhi
      do i = ilo, ihi
        us(i,:,j) = 2.0*i + 3.0*j
        vs(i,:,j) = -3.0*i + 2.0*j
        do k = 1, 4
          zs(i,k,j) = 100.0 + interfaces(k)
        end do
      end do
    end do
    xm = 0.5*(ilo+ihi)
    ym = 0.5*(jlo+jhi)
    x = [real(ilo), real(ihi), real(ilo), real(ihi), xm, xm, real(ilo), real(ihi), &
         xm, real(ilo)-0.75, real(ihi)+0.75]
    y = [real(jlo), real(jlo), real(jhi), real(jhi), real(jlo), real(jhi), ym, ym, &
         ym, real(jlo)-0.75, real(jhi)+0.75]
    do n = 1, 11
      call projection%Calc_latlon(x(n), y(n), lats(n,1), lons(n,1))
    end do
    roughness = 0.1
    terrain = 100.0
    scale = log(6.0/0.1) / log(10.0/0.1)
    do method_index = 1, size(methods)
      method = methods(method_index)
      call Interp_wrfwinds_to_cfbm(us, vs, zs, ims, ime, 1, 4, jms, jme, ids, ide, jds, jde, &
          1, 11, 1, 1, 1, 4, 2, [1,7], [6,11], [1,1], [1,1], lats, lons, projection, roughness, terrain, &
          .false., 10.0, 6.0, uout, vout, method)
      if (.not.all(ieee_is_finite(uout)) .or. .not.all(ieee_is_finite(vout))) &
          error stop 'physical ghost contaminated wind'
      do n = 1, 11
        ! Use the actual projection round trip to avoid ambiguity at nearest
        ! half-cell ties; source values remain independently analytic.
        call projection%Calc_ij(lats(n,1), lons(n,1), xr, yr)
        if (method == HINTERP_NEAREST) then
          xr = real(nint(xr))
          yr = real(nint(yr))
        end if
        xr = max(real(ilo), min(real(ihi), xr))
        yr = max(real(jlo), min(real(jhi), yr))
        expected_u = (2.0*xr + 3.0*yr)*scale
        expected_v = (-3.0*xr + 2.0*yr)*scale
        if (abs(uout(n,1)-expected_u) > 8.0e-6) error stop 'bounded U mapping differs'
        if (abs(vout(n,1)-expected_v) > 8.0e-6) error stop 'bounded V mapping differs'
      end do
    end do
  end subroutine check_source_bounds


  subroutine check_vertical_storage()

    ! WRF vertical memory can extend beyond initialized profile levels. Poison
    ! storage below the first physical level and above the consumed winds and
    ! interfaces. Neither horizontal method may evaluate these signaling NaNs.
    ! Sample below and above the first mass level to exercise both wind levels.
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_signaling_nan, ieee_invalid, &
        ieee_get_flag, ieee_set_flag, ieee_is_finite
    real :: us(2,0:6,3), vs(2,0:6,3), zs(2,0:6,3)
    real :: lats(1,1), lons(1,1), roughness(1,1), terrain(1,1), uout(1,1), vout(1,1)
    real :: target, expected_u, expected_v, fraction, nan
    integer :: k, method, method_index, height_case
    logical :: invalid

    nan = ieee_value(0.0, ieee_signaling_nan)
    us = nan
    vs = nan
    zs = nan
    do k = 1, 2
      us(:,k,:) = 4.0*k
      vs(:,k,:) = -2.0*k
    end do
    do k = 1, 3
      zs(:,k,:) = 100.0 + interfaces(k)
    end do
    lats = 40.0
    lons = -105.0
    roughness = 0.1
    terrain = 100.0

    do height_case = 1, 2
      if (height_case == 1) then
        target = 6.0
        fraction = log(target/0.1) / log(10.0/0.1)
        expected_u = 4.0*fraction
        expected_v = -2.0*fraction
      else
        target = 30.0
        fraction = log(target/10.0) / log(40.0/10.0)
        expected_u = 4.0 + 4.0*fraction
        expected_v = -2.0 - 2.0*fraction
      end if
      do method_index = 1, size(methods)
        method = methods(method_index)
        call ieee_set_flag(ieee_invalid, .false.)
        call Interp_wrfwinds_to_cfbm(us, vs, zs, 1, 2, 0, 6, 1, 3, 1, 3, 1, 4, &
            1, 1, 1, 1, 1, 4, 1, [1], [1], [1], [1], lats, lons, projection, roughness, terrain, &
            .false., 10.0, target, uout, vout, method)
        call ieee_get_flag(ieee_invalid, invalid)
        if (invalid) error stop 'unused vertical storage was evaluated'
        if (.not.all(ieee_is_finite(uout)) .or. .not.all(ieee_is_finite(vout))) &
            error stop 'unused vertical storage contaminated wind'
        if (abs(uout(1,1)-expected_u) > 2.0e-6) error stop 'bounded vertical U differs'
        if (abs(vout(1,1)-expected_v) > 1.0e-6) error stop 'bounded vertical V differs'
      end do
    end do
  end subroutine check_vertical_storage

end program wrf_wind_unit
