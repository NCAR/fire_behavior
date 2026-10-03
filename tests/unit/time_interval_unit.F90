program time_interval_unit

  ! Advance a small idealized fire grid to check that the first interval includes
  ! zero-time ignition and that an invalid timestep counter is rejected. Other
  ! CTest scenarios check delayed perimeter installation at an interval endpoint
  ! and propagation on the following interval, including roundoff-sized timing
  ! offsets and a fractional-second timestep.

  use namelist_mod, only : namelist_t
  use state_mod, only : state_fire_t
  use initialize_mod, only : Init_fire_state
  use advance_mod, only : Advance_state
  use fire_model_mod, only : Advance_fire_model
  use netcdf_mod, only : NF90_FILL_FLOAT
#ifdef DM_PARALLEL
  use mpi
#endif

  implicit none

  type (namelist_t) :: config
  type (state_fire_t) :: grid
  real, allocatable :: initial_lfn(:, :)
  character (len = 40) :: scenario
  integer :: i, j, ierr
  real :: endpoint

#ifdef DM_PARALLEL
  call MPI_Init (ierr)
  call grid%Set_mpi_comm_cfbm (MPI_COMM_WORLD)
#endif
  call get_command_argument (1, scenario)
  config%ideal_opt = 1
  config%fire_num_ignitions = 1
  config%dt = 4.0
  config%nx = 40
  config%ny = 40
  config%dx = 100.0
  config%dy = 100.0
  config%num_tiles = 4
  config%tile_strategy = 3
  config%zonal_wind = 0.0
  config%meridional_wind = 0.0
  config%fire_lsm_reinit = .false.
  config%start_year = 2020
  config%start_month = 1
  config%start_day = 1
  config%start_hour = 0
  config%start_minute = 0
  config%start_second = 0
  config%end_year = 2020
  config%end_month = 1
  config%end_day = 1
  config%end_hour = 0
  config%end_minute = 1
  config%end_second = 0
  config%interval_output = 4
  config%fire_ignition_ros1 = 100.0
  config%fire_ignition_radius1 = 50.0
  config%fire_ignition_start_time1 = 0.0
  config%fire_ignition_end_time1 = 2.0

  if (index (scenario, 'perimeter') == 1) then
    if (scenario == 'perimeter_fractional') config%dt = 0.1
    endpoint = 2.0 * config%dt
    config%fire_ignition_start_time1 = endpoint
    if (scenario == 'perimeter_above') config%fire_ignition_start_time1 = nearest (endpoint, 1.0)
    if (scenario == 'perimeter_below') config%fire_ignition_start_time1 = nearest (endpoint, -1.0)
  end if

  ! This fixture assigns settings directly, bypassing Init_namelist validation.
  call config%Check_nml ()
  call Init_fire_state (grid, config)
  ! Ideal initialization leaves unavailable forcing marked, including old fields.
  if (grid%fire_t2(20,20) /= NF90_FILL_FLOAT .or. grid%fire_t2_old(20,20) /= NF90_FILL_FLOAT) &
      error stop 'ideal initialization mode did not preserve atmospheric fill values'

  if (scenario == 'invalid') then
    call Advance_fire_model (config, grid)
    error stop 'invalid counter unexpectedly advanced'
  else if (scenario == 'first') then
    ! Put the zero-time ignition exactly on an interior node. The historical
    ! [dt,2dt] first interval misses its short prescribed ignition window.
    grid%ignition_lines%start_x = grid%lons(20,20)
    grid%ignition_lines%end_x = grid%lons(20,20)
    grid%ignition_lines%start_y = grid%lats(20,20)
    grid%ignition_lines%end_y = grid%lats(20,20)
    call Advance_state (grid, config)
    if (grid%lfn(20,20) >= 0.0 .or. grid%tign_g(20,20) /= 0.0) &
        error stop 'first advance missed zero-time ignition'
    if (grid%itimestep /= 1) error stop 'first advance changed counter semantics'
  else if (index (scenario, 'perimeter') == 1) then
    ! Build the synthetic grid first, then supply the test perimeter explicitly.
    ! Ideal initialization itself does not support reading an observed perimeter.
    config%fire_is_real_perim = .true.
    call config%Check_nml ()
    allocate (initial_lfn(grid%ifps:grid%ifpe, grid%jfps:grid%jfpe))
    do j = grid%jfps, grid%jfpe
      do i = grid%ifps, grid%ifpe
        initial_lfn(i,j) = sqrt (real ((i-20)**2 + (j-20)**2)) * grid%dx - 250.0
      end do
    end do
    call grid%Init_fire_perimeter (initial_lfn, config%fire_ignition_start_time1)
    call Advance_state (grid, config)
    if (grid%fire_perimeter_ignited .or. any (grid%lfn(grid%ifps:grid%ifpe, grid%jfps:grid%jfpe) < 0.0)) &
        error stop 'perimeter ignited before scheduled endpoint'
    call Advance_state (grid, config)
    if (.not. grid%fire_perimeter_ignited) error stop 'perimeter missing at scheduled endpoint'
    if (any (grid%lfn(grid%ifps:grid%ifpe, grid%jfps:grid%jfpe) /= initial_lfn)) &
        error stop 'perimeter propagated before installation at endpoint'
    if (grid%tign_g(20,20) /= endpoint) error stop 'ignition time differs from normalized endpoint'
    call Advance_state (grid, config)
    if (.not. any (grid%lfn(grid%ifps:grid%ifpe, grid%jfps:grid%jfpe) < initial_lfn)) &
        error stop 'perimeter did not propagate on following interval'
    if (grid%tign_g(20,20) /= endpoint) error stop 'perimeter was reinstalled'
  else
    error stop 'unknown advance scenario'
  end if

  write (*, '(a)') 'advance behavior passed: ' // trim (scenario)
#ifdef DM_PARALLEL
  call MPI_Finalize (ierr)
#endif

end program time_interval_unit
