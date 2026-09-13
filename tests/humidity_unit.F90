program humidity_unit

  use, intrinsic :: ieee_arithmetic, only : ieee_positive_inf, ieee_quiet_nan, ieee_value
  use humidity_mod, only : Is_valid_mixing_ratio, Is_valid_specific_humidity, Mixing_ratio_to_specific_humidity, &
      Specific_humidity_to_mixing_ratio, Vapor_pressure_from_mixing_ratio

  implicit none

  real, parameter :: TOLERANCE = 2.0e-7
  real :: q2, r2, value


  r2 = 0.006
  q2 = Mixing_ratio_to_specific_humidity (r2)
  call Assert_close (Specific_humidity_to_mixing_ratio (q2), r2, TOLERANCE, 'humidity round trip')
  call Assert_close (Vapor_pressure_from_mixing_ratio (r2, 90000.0), 859.872611, TOLERANCE, 'vapor pressure')

  if (.not. Is_valid_specific_humidity (0.0)) error stop 'zero specific humidity rejected'
  if (.not. Is_valid_specific_humidity (0.999)) error stop 'valid specific humidity rejected'
  if (Is_valid_specific_humidity (-0.001)) error stop 'negative specific humidity accepted'
  if (Is_valid_specific_humidity (1.0)) error stop 'unit specific humidity accepted'

  value = ieee_value (value, ieee_quiet_nan)
  if (Is_valid_specific_humidity (value)) error stop 'NaN specific humidity accepted'
  value = ieee_value (value, ieee_positive_inf)
  if (Is_valid_specific_humidity (value)) error stop 'infinite specific humidity accepted'

  if (.not. Is_valid_mixing_ratio (0.0)) error stop 'zero mixing ratio rejected'
  if (Is_valid_mixing_ratio (-0.001)) error stop 'negative mixing ratio accepted'

contains

  subroutine Assert_close (actual, expected, relative_tolerance, label)

    implicit none

    real, intent (in) :: actual, expected, relative_tolerance
    character (len = *), intent (in) :: label


    if (abs (actual - expected) > relative_tolerance * abs (expected)) then
      write (*, '(a,2(1x,es16.8))') trim (label), actual, expected
      error stop 'humidity calculation differs from reference'
    end if

  end subroutine Assert_close

end program humidity_unit
