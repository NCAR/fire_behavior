program fmc_initialization_unit

  ! Use two owned tiles surrounded by unavailable halos. Check the effective
  ! initialization timestamp and a subsequent drying increment independently.
  use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_finite
  use fmc_wrffire_mod, only : fmc_wrffire_t
  use fuel_anderson_mod, only : fuel_anderson_t
  use ros_wrffire_mod, only : ros_wrffire_t
  use netcdf, only : NF90_FILL_FLOAT
#ifdef DM_PARALLEL
  use mpi
#endif
  implicit none

  type (fmc_wrffire_t) :: moisture
  type (fuel_anderson_t) :: fuels
  type (ros_wrffire_t) :: ros
  real :: rain(0:5,0:3), t(0:5,0:3), q(0:5,0:3), p(0:5,0:3)
  real :: rain_old(0:5,0:3), t_old(0:5,0:3), q_old(0:5,0:3), p_old(0:5,0:3)
  real :: rh(0:5,0:3), fmc(0:5,0:3), category(0:5,0:3)
  real :: initial(4,5,2), initial_fmc(4,2), interval, elapsed, expected, d, w, change
  integer :: first_step, next_step, frequency, step, ierr
  integer :: i_start(2) = [1,1], i_end(2) = [4,4], j_start(2) = [1,2], j_end(2) = [1,2]
  character (len = 32) :: scenario

#ifdef DM_PARALLEL
  call MPI_Init (ierr)
#endif
  call get_command_argument (1, scenario)
  call fuels%Initialization (0.8)
  call moisture%Init (fuels, 0.08, 0.30, 0, 5, 0, 3, 0, 4.0)
  call ros%Init (0, 5, 0, 3)
  if (moisture%initialized) error stop 'allocation initialized moisture before forcing'
  moisture%fmc_gc = NF90_FILL_FLOAT
  t = NF90_FILL_FLOAT
  q = NF90_FILL_FLOAT
  p = NF90_FILL_FLOAT
  rain = NF90_FILL_FLOAT
  t(1:4,1:2) = 300.0
  q(1:4,1:2) = 0.008
  p(1:4,1:2) = 90000.0
  rain(1:4,1:2) = 7.0
  t_old = NF90_FILL_FLOAT
  q_old = NF90_FILL_FLOAT
  p_old = NF90_FILL_FLOAT
  rain_old = NF90_FILL_FLOAT
  category = 1.0
  frequency = 3
  interval = 16.0
  first_step = 1
  next_step = 3
  select case (scenario)
    case ('each')
      frequency = 1
      next_step = 2
    case ('time')
      frequency = 0
      next_step = 5
    case ('late_first_call')
      first_step = 7
      next_step = 9
    case ('missing')
      t(4,2) = NF90_FILL_FLOAT
    case ('nan')
      q(4,2) = ieee_value (0.0, ieee_quiet_nan)
    case ('temperature')
      t(4,2) = 0.0
    case ('pressure')
      p(4,2) = 0.0
    case ('humidity')
      q(4,2) = -0.001
    case ('rain')
      rain(4,2) = -1.0
  end select

  call Advance (first_step)
  if (.not. moisture%initialized) error stop 'delayed cadence skipped initialization'
  if (moisture%initialization_time /= first_step * 4.0) error stop 'wrong initialization time'
  if (moisture%fmoist_lasttime /= first_step * 4.0) error stop 'wrong first moisture time'
  if (moisture%dt_moisture /= 0.0) error stop 'artificial elapsed interval at initialization'
  if (.not. all (ieee_is_finite (moisture%fmc_gc(1:4,:,1:2)))) error stop 'invalid initial class state'
  if (any (moisture%fmc_gc(1:4,5,1:2) /= 0.30)) error stop 'initial live moisture advanced'
  call Equilibria (300.0, 0.008, d, w)
  expected = 0.5 * (d + w)
  if (any (abs (moisture%fmc_gc(1:4,1:4,1:2) - expected) > 2.0e-7)) error stop 'wrong initial equilibrium'
  if (any (t_old(1:4,1:2) /= t(1:4,1:2)) .or. any (q_old(1:4,1:2) /= q(1:4,1:2)) .or. &
      any (p_old(1:4,1:2) /= p(1:4,1:2)) .or. any (rain_old(1:4,1:2) /= rain(1:4,1:2))) &
      error stop 'initial forcing history was not copied'
  initial = moisture%fmc_gc(1:4,:,1:2)
  initial_fmc = fmc(1:4,1:2)
  if (scenario == 'duplicate') call Advance (first_step * frequency)
  if (scenario == 'old_forcing') q_old(4,2) = ieee_value (0.0, ieee_quiet_nan)

  ! Dry the forcing after initialization. Held classes and previous forcing must
  ! persist until the scheduled update, including the first post-restart call.
  t(1:4,1:2) = 310.0
  q(1:4,1:2) = 0.002
  do step = first_step + 1, next_step - 1
    call Advance (step)
    if (any (moisture%fmc_gc(1:4,:,1:2) /= initial)) error stop 'off-cadence class advance'
    if (any (fmc(1:4,1:2) /= initial_fmc)) error stop 'off-cadence effective moisture advance'
    if (any (t_old(1:4,1:2) /= 300.0)) error stop 'off-cadence forcing history changed'
  end do
  call Advance (next_step)
  elapsed = (next_step - first_step) * 4.0
  if (moisture%dt_moisture /= elapsed) error stop 'wrong elapsed interval after initialization'
  call Equilibria (305.0, 0.005, d, w)
  change = elapsed / 3600.0
  expected = initial(1,1,1) + (max (min (initial(1,1,1), d), w) - initial(1,1,1)) * change * (1.0 - 0.5 * change)
  if (any (abs (moisture%fmc_gc(1:4,1,1:2) - expected) > 2.0e-7)) error stop 'first drying increment differs'
  if (any (moisture%fmc_gc(1:4,1,1:2) >= initial(:,1,:))) error stop 'drying did not advance moisture'
  if (moisture%initialization_time /= first_step * 4.0) error stop 'initialization was repeated'
  if (frequency == 0 .and. moisture%fmoist_nexttime /= next_step * 4.0 + interval) error stop 'time cadence changed'
  print *, 'moisture initialization checks passed: ', trim (scenario)
#ifdef DM_PARALLEL
  call MPI_Finalize (ierr)
#endif

contains

  subroutine Advance (n)
    integer, intent (in) :: n
    call moisture%Advance_fmc_model (frequency, interval, n, 4.0, 0, 5, 0, 3, &
        i_start, i_end, j_start, j_end, 2, rain, t, q, p, rain_old, t_old, q_old, p_old, rh, &
        0.08, fmc, category, fuels, ros)
  end subroutine Advance

  subroutine Equilibria (temperature, humidity, drying, wetting)
    real, intent (in) :: temperature, humidity
    real, intent (out) :: drying, wetting
    real :: vapor_pressure, saturation_pressure, percent_rh
    ! Retain the existing thermodynamic expression here. P01 owns its correction.
    vapor_pressure = humidity * 90000.0 / (0.622 + 0.378 * humidity)
    saturation_pressure = exp (54.842763 - 6763.22 / temperature - 4.210 * log (temperature) + &
        0.000367 * temperature + tanh (0.0415 * (temperature - 218.8)) * &
        (53.878 - 1331.22 / temperature - 9.44523 * log (temperature) + 0.014025 * temperature))
    percent_rh = min (vapor_pressure / saturation_pressure, 1.0) * 100.0
    drying = (0.942 * percent_rh ** 0.679 + 0.000499 * exp (0.1 * percent_rh) + &
        0.18 * (294.25 - temperature) * (1.0 - exp (-0.115 * percent_rh))) * 0.01
    wetting = (0.618 * percent_rh ** 0.753 + 0.000454 * exp (0.1 * percent_rh) + &
        0.18 * (294.25 - temperature) * (1.0 - exp (-0.115 * percent_rh))) * 0.01
  end subroutine Equilibria

end program fmc_initialization_unit
