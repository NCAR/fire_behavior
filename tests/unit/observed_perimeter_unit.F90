program observed_perimeter_unit

  use state_mod, only : state_fire_t

  implicit none

  type (state_fire_t) :: state
  real, dimension(2, 2) :: initial_lfn
  real :: unignited


  state%ifps = 1
  state%ifpe = 2
  state%jfps = 1
  state%jfpe = 2
  allocate (state%lfn(1:2, 1:2), state%lfn_hist(1:2, 1:2), state%tign_g(1:2, 1:2))

  unignited = epsilon (unignited)
  state%tign_g = unignited
  initial_lfn = reshape ([-2.0, 0.0, 1.0, 3.0], shape (initial_lfn))

  state%lfn = 100.0
  call state%Init_observed_perimeter (initial_lfn, 8.0)

  if (state%observed_perimeter_active) error stop 'delayed perimeter activated during initialization'
  if (any (state%lfn /= 100.0)) error stop 'delayed perimeter changed the active level set before its start time'
  if (any (state%lfn_hist /= initial_lfn)) error stop 'delayed perimeter history does not match supplied perimeter'

  call state%Activate_observed_perimeter (8.0)

  if (any (state%lfn /= initial_lfn)) error stop 'active level set does not match supplied perimeter'
  if (any (state%lfn_hist /= initial_lfn)) error stop 'perimeter history does not match supplied perimeter'
  if (state%tign_g(1, 1) /= 8.0 .or. state%tign_g(2, 1) /= 8.0) &
      error stop 'burned perimeter cells do not have the scheduled ignition time'
  if (state%tign_g(1, 2) /= unignited .or. state%tign_g(2, 2) /= unignited) &
      error stop 'unburned perimeter cells lost their ignition-time sentinel'

  state%observed_perimeter_active = .false.
  state%lfn = 100.0
  state%tign_g = unignited
  call state%Init_observed_perimeter (initial_lfn, 0.0)
  if (.not. state%observed_perimeter_active) error stop 'zero-time perimeter was not initialized'
  if (any (state%lfn /= initial_lfn)) error stop 'zero-time perimeter does not match supplied perimeter'
  if (state%tign_g(1, 1) /= 0.0 .or. state%tign_g(2, 1) /= 0.0) &
      error stop 'zero-time perimeter cells do not have zero ignition time'

end program observed_perimeter_unit
