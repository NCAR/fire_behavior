program fuel_precision_unit

  ! Evaluate a uniformly burning cell against analytical exponential decay.
  ! Check that double-precision storage preserves small consumption increments,
  ! that accumulated consumption matches the loss of remaining fuel, and that
  ! an unignited cell consumes no fuel.

  use, intrinsic :: iso_fortran_env, only : REAL64
  use state_mod, only : state_fire_t
  use level_set_mod, only : Calc_fuel_left
#ifdef DM_PARALLEL
  use mpi
#endif

  implicit none

  type(state_fire_t) :: grid
  real :: lfn(5,5), tign(5,5), fuel_time(5,5), fire_area(5,5), time_now
  real(REAL64) :: expected_remaining, expected_burnt, previous_remaining, burnt_sum
  integer :: step, ierr

#ifdef DM_PARALLEL
  call MPI_Init(ierr)
#endif

  allocate(grid%fuel_frac(5,5), grid%fuel_frac_burnt_dt(5,5))
  if (storage_size(grid%fuel_frac) /= 64) error stop 'remaining fuel storage lost precision'
  if (storage_size(grid%fuel_frac_burnt_dt) /= 64) error stop 'consumed fuel storage lost precision'

  ! A uniformly burning stencil has an analytical exponential decay.
  ! Its tiny increments expose rounding of fractions near one, without any
  ! wind, projection, perimeter geometry, or MPI decomposition difference.
  lfn = -1.0
  tign = 0.0
  fuel_time = 1000000.0
  grid%fuel_frac = 1.0_REAL64
  previous_remaining = 1.0_REAL64
  burnt_sum = 0.0_REAL64
  do step = 1, 4
    time_now = 0.25 * step
    call Calc_fuel_left(1,5,1,5,3,3,3,3,3,3,3,3,lfn,tign,fuel_time,time_now, &
        grid%fuel_frac,fire_area,grid%fuel_frac_burnt_dt)
    expected_remaining = exp(-real(time_now, REAL64) / 1000000.0_REAL64)
    expected_burnt = previous_remaining - expected_remaining
    if (abs(grid%fuel_frac_burnt_dt(3,3) - expected_burnt) > 1.0e-8_REAL64 * expected_burnt) &
        error stop 'small fuel-consumption increment lost precision'
    if (abs(grid%fuel_frac(3,3) - expected_remaining) > 1.0e-14_REAL64) &
        error stop 'remaining fuel differs from exponential decay'
    if (fire_area(3,3) /= 1.0) error stop 'uniformly burning cell lost its area'
    burnt_sum = burnt_sum + grid%fuel_frac_burnt_dt(3,3)
    previous_remaining = expected_remaining
  end do
  if (abs(burnt_sum - (1.0_REAL64 - grid%fuel_frac(3,3))) > 1.0e-14_REAL64) &
      error stop 'consumed and remaining fuel are inconsistent'

  ! The same routine must leave unignited fuel exactly intact.
  lfn = 1.0
  grid%fuel_frac = 1.0_REAL64
  call Calc_fuel_left(1,5,1,5,3,3,3,3,3,3,3,3,lfn,tign,fuel_time,time_now, &
      grid%fuel_frac,fire_area,grid%fuel_frac_burnt_dt)
  if (grid%fuel_frac(3,3) /= 1.0_REAL64 .or. grid%fuel_frac_burnt_dt(3,3) /= 0.0_REAL64) &
      error stop 'unignited cell consumed fuel'

  print *, 'fuel precision checks passed'
#ifdef DM_PARALLEL
  call MPI_Finalize(ierr)
#endif

end program fuel_precision_unit
