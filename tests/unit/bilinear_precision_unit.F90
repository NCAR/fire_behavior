program bilinear_precision_unit

  ! Check that horizontal interpolation preserves atmospheric forcing values.
  ! Uniform fields test preservation of constants; a planar field tests the
  ! mapped value when the forcing varies in both horizontal directions.

  use, intrinsic :: iso_fortran_env, only : REAL64
  use proj_lc_mod, only : proj_lc_t
  use interp_mod, only : Interp_horizontal_bilinear

  implicit none

  type(proj_lc_t) :: projection
  real :: field(75,75), lat(7,5), lon(7,5), mapped(7,5), value
  real, parameter :: constants(4) = [300.0, 0.008, 90000.0, 1600.0]
  real :: expected_i, expected_j, expected
  integer :: i, j, n

  projection = proj_lc_t(40.0, -105.0, 100.0, 100.0, -105.0, 30.0, 60.0, 75, 75)

  ! Test 1: a uniform input must give exactly the same value at every output
  ! location. Fractional weights previously introduced small artificial changes
  ! in otherwise uniform temperature, humidity and pressure fields, which can
  ! affect fuel moisture and spread. Terrain magnitudes are also checked.
  do j = 1, 5
    do i = 1, 7
      lat(i,j) = 40.0 + real(i - 4) / 512.0
      lon(i,j) = -105.0 + real(j - 3) / 512.0
    end do
  end do
  do n = 1, size(constants)
    value = constants(n)
    field = value
    call map_field()
    if (any(mapped /= value)) error stop 'bilinear mapping changed a constant field'
  end do

  ! Test 2: a field with constant slopes in x and y must map to the analytical
  ! value of that plane. This checks nonuniform interpolation and detects extra
  ! rounding in its weighted sum. The independent Lambert indices also appear
  ! in projection_precision_unit. Round those indices to the existing interface,
  ! then allow only the final rounding into the output field.
  do j = 1, 75
    do i = 1, 75
      field(i,j) = 16000.0 + 4.0 * (i - 1) + 8.0 * (j - 1)
    end do
  end do
  lat = 40.015625
  lon = -104.96875
  expected_i = real(63.817009468862302_REAL64)
  expected_j = real(54.859969983619521_REAL64)
  expected = real(16000.0_REAL64 + 4.0_REAL64 * (real(expected_i, REAL64) - 1.0_REAL64) + &
      8.0_REAL64 * (real(expected_j, REAL64) - 1.0_REAL64))
  call map_field()
  if (any(mapped /= expected)) error stop 'bilinear mapping lost planar-field precision'

  print *, 'bilinear precision checks passed'

contains

  subroutine map_field()

    call Interp_horizontal_bilinear(field, projection, 1, 75, 1, 75, &
        1, 7, 1, 5, 1, 7, 1, 5, lat, lon, mapped)

  end subroutine map_field

end program bilinear_precision_unit
