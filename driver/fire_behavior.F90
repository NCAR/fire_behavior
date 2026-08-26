  program fire_behavior

#ifdef DM_PARALLEL
    use mpi
#endif
    use state_mod, only : state_fire_t
    use namelist_mod, only : namelist_t
    use initialize_mod, only : Init_fire_state, Init_atm_state
    use advance_mod, only : Advance_state
    use wrfdata_mod, only : wrfdata_t
    use datetime_mod, only : datetime_t
    use stderrout_mod, only : Stop_simulation
    use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT

    implicit none

    integer :: ierr, rank, mpi_comm_cfbm
    integer :: restart_step_interval
    real :: restart_interval_seconds, restart_interval_error
    type (state_fire_t) :: grid
    type (wrfdata_t) :: atm_state
    type (namelist_t) :: config_flags
    type (datetime_t) :: datetime_check
    character (len = 256) :: msg
    logical, parameter :: DEBUG_LOCAL = .false.
    real, parameter :: RESTART_INTERVAL_TOL = 1.0e-6


    if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Running fire_behavior...'

#ifdef DM_PARALLEL
    call mpi_init (ierr)
    if (ierr /= MPI_SUCCESS) then
      write (ERROR_UNIT, *) 'ERROR: mpi_init failed'
      stop
    end if

    call MPI_Comm_dup (MPI_COMM_WORLD, mpi_comm_cfbm, ierr)
    if (ierr /= MPI_SUCCESS) then
      write (ERROR_UNIT, *) 'ERROR: mpi_comm_dup failed'
      stop
    end if
#endif

      ! Read namelist
#ifdef DM_PARALLEL
    call Mpi_comm_rank (mpi_comm_cfbm, rank, ierr)
    if (ierr /= MPI_SUCCESS) then
      write (ERROR_UNIT, *) 'ERROR: mpi_comm_rank failed'
      stop
    end if
#else
    rank = 0
#endif

    if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Reading namelist...'
    if (rank == 0) call config_flags%Initialization (file_name = 'namelist.fire')

#ifdef DM_PARALLEL
    call config_flags%Broadcast_nml (mpi_comm_cfbm)
#endif

    if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Initialization fire state...'
#ifdef DM_PARALLEL
    call grid%Set_mpi_comm_cfbm (mpi_comm_cfbm)
#endif
    select case (config_flags%ideal_opt)
      case (0)
        call Init_atm_state (atm_state, config_flags)
        call Init_fire_state (grid, config_flags, atm_state)

      case (1)
        call Init_fire_state (grid, config_flags)

      case default
        write (ERROR_UNIT, *) 'ERROR: ideal_opt option not supported: ', config_flags%ideal_opt
        stop

    end select

    if (config_flags%restart) then
      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Reading restart state...'
      call grid%Read_restart (config_flags)

    else
      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Saving fire state...'
      call grid%Save_state ()
    end if

    restart_step_interval = -1
    if (config_flags%restart_interval > 0) then
      restart_step_interval = nint (real (config_flags%restart_interval) / grid%dt)
      restart_interval_seconds = restart_step_interval * grid%dt
      restart_interval_error = abs (restart_interval_seconds - real (config_flags%restart_interval))

      if (restart_step_interval <= 0 .or. &
          restart_interval_error > max (RESTART_INTERVAL_TOL, abs (real (config_flags%restart_interval)) * RESTART_INTERVAL_TOL)) then
        write (msg, '(a, i0, a, f12.6)') 'restart_interval must map to an integer number of time steps: restart_interval = ', &
            config_flags%restart_interval, ', dt = ', grid%dt
        call Stop_simulation (msg)
      end if
    end if

    if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Starting temporal loop...'
    do while (grid%datetime_now < grid%datetime_end)
      call Advance_state (grid, config_flags)

      datetime_check = grid%datetime_start
      call datetime_check%Add_seconds (grid%itimestep * grid%dt)
      if (datetime_check /= grid%datetime_now) call Stop_simulation ('Model clock is inconsistent with itimestep and dt')

      call grid%Handle_output (config_flags)
      if (config_flags%ideal_opt == 0) call grid%Handle_wrfdata_update (atm_state, config_flags)

      if (restart_step_interval > 0) then
        if (mod (grid%itimestep, restart_step_interval) == 0) call grid%Write_restart (config_flags)
      end if
    end do
    if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Completed temporal loop'

#ifdef DM_PARALLEL
    call mpi_finalize (ierr)
    if (ierr /= MPI_SUCCESS) then
      write (ERROR_UNIT, *) 'ERROR: mpi_finalize failed'
      stop
    end if
#endif

    if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Completed running fire_behavior'

  end program fire_behavior
