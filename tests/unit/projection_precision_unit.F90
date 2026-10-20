program projection_precision_unit

  ! Convert prescribed latitudes and longitudes to Lambert grid indices and
  ! compare with independent reference values. Check both hemispheres,
  ! coincident standard parallels, and longitude wrapping, allowing only the
  ! rounding needed to return single-precision indices.

  use, intrinsic :: iso_fortran_env, only : REAL64
  use proj_lc_mod, only : proj_lc_t

  implicit none

  type(proj_lc_t) :: projection

  ! Reference indices were evaluated independently from the spherical Lambert
  ! equations with R=6370000 m, dx=100 m, and the centre at (38,38).
  ! The test latitudes and longitudes are exactly representable in real32.
  ! A forward/inverse round trip alone would conflate two precision policies:
  ! the forward transform still defines the existing single-precision grid.
  projection = proj_lc_t(40.0, -105.0, 100.0, 100.0, -105.0, 30.0, 60.0, 75, 75)
  call check_indices(40.0, -105.0, 38.0_REAL64, 38.0_REAL64)
  call check_indices(40.015625, -104.96875, 63.817009468862302_REAL64, 54.859969983619521_REAL64)

  ! Check hemisphere signs, coincident standard parallels, and longitude wrap.
  projection = proj_lc_t(-40.0, -105.0, 100.0, 100.0, -105.0, -30.0, -60.0, 75, 75)
  call check_indices(-39.984375, -104.96875, 63.830165964220420_REAL64, 54.850328223750694_REAL64)

  projection = proj_lc_t(40.0, -105.0, 100.0, 100.0, -105.0, 30.0, 30.0, 75, 75)
  call check_indices(40.015625, -104.96875, 65.035644271219581_REAL64, 55.653540645667817_REAL64)

  projection = proj_lc_t(40.0, 179.0, 100.0, 100.0, 178.0, 30.0, 60.0, 75, 75)
  call check_indices(40.015625, -179.96875, 889.59232965281012_REAL64, 70.979009838425554_REAL64)

  print *, 'projection precision checks passed'

contains

  subroutine check_indices(latitude, longitude, expected_i, expected_j)

    real, intent(in) :: latitude, longitude
    real(REAL64), intent(in) :: expected_i, expected_j
    real :: i, j

    call projection%Calc_ij(latitude, longitude, i, j)
    ! Allow final real32 index rounding, but not cancellation in the geometry.
    if (abs(real(i, REAL64) - expected_i) > 2 * epsilon(i) * max(1.0_REAL64, abs(expected_i))) &
        error stop 'inverse Lambert eastward index lost precision'
    if (abs(real(j, REAL64) - expected_j) > 2 * epsilon(j) * max(1.0_REAL64, abs(expected_j))) &
        error stop 'inverse Lambert northward index lost precision'

  end subroutine check_indices

end program projection_precision_unit
