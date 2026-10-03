program namelist_validation_unit

  ! Exercise configuration checks for atmospheric intervals, ignition counts,
  ! and perimeter activation times. CTest selects valid or invalid scenarios
  ! and checks the expected rejection diagnostics. Atmospheric-only scenarios
  ! validate their timing without requiring the fire component's settings.

  use namelist_mod, only : namelist_t, FIRE_MAX_IGNITIONS_IN_NAMELIST
#ifdef DM_PARALLEL
  use mpi
#endif

  implicit none

  type (namelist_t) :: config
  character (len = 40) :: scenario
  logical :: require_atm
  integer :: ierr

#ifdef DM_PARALLEL
  call MPI_Init (ierr)
#endif
  call get_command_argument (1, scenario)
  config%fire_num_ignitions = 1
  config%dt = 4.0
  config%interval_atm = 60
  require_atm = .true.

  select case (trim (scenario))
    case ('atm_only', 'atm_nonmultiple', 'atm_missing', 'atm_zero_dt')
      ! A partial atmospheric configuration has no fire ignition settings.
      config%fire_num_ignitions = 0
      if (scenario == 'atm_nonmultiple') config%interval_atm = 6
      if (scenario == 'atm_missing') config%interval_atm = -1
      if (scenario == 'atm_zero_dt') config%dt = 0.0
    case ('aligned')
    case ('reversed')
      config%dt = 8.0
      config%interval_atm = 4
    case ('nonmultiple')
      config%interval_atm = 6
    case ('zero_dt')
      config%dt = 0.0
    case ('missing_atm')
      config%interval_atm = -1
    case ('wrf_unset')
      config%interval_atm = -1
      require_atm = .false.
    case ('ideal_unset')
      config%ideal_opt = 1
      config%interval_atm = -1
      require_atm = .false.
    case ('zero_count')
      config%fire_num_ignitions = 0
    case ('negative_count')
      config%fire_num_ignitions = -1
    case ('excess_count')
      config%fire_num_ignitions = FIRE_MAX_IGNITIONS_IN_NAMELIST + 1
    case ('perimeter_count')
      config%fire_is_real_perim = .true.
      config%fire_num_ignitions = 2
    case ('negative_time', 'interior_time', 'boundary_time', 'boundary_above', 'boundary_below')
      config%fire_is_real_perim = .true.
      config%fire_ignition_start_time1 = 8.0
      if (scenario == 'negative_time') config%fire_ignition_start_time1 = -1.0
      if (scenario == 'interior_time') config%fire_ignition_start_time1 = 6.0
      if (scenario == 'boundary_above') config%fire_ignition_start_time1 = nearest (8.0, 1.0)
      if (scenario == 'boundary_below') config%fire_ignition_start_time1 = nearest (8.0, -1.0)
    case ('line_interior')
      config%fire_ignition_start_time1 = 6.0
    case default
      error stop 'unknown namelist validation scenario'
  end select

  if (index (scenario, 'atm_') == 1) then
    call config%Check_time_intervals (require_atm_interval = .true.)
  else
    call config%Check_nml (require_atm_interval = require_atm)
  end if
  write (*, '(a)') 'namelist validation accepted: ' // trim (scenario)
#ifdef DM_PARALLEL
  call MPI_Finalize (ierr)
#endif

end program namelist_validation_unit
