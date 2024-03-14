  program fire_behavior

#ifdef DM_PARALLEL
    use mpi
#endif
    use state_mod, only : state_fire_t
    use namelist_mod, only : namelist_t
    use initialize_mod, only : Init_fire_state, Init_atm_state
    use advance_mod, only : Advance_state
    use wrf_mod, only : wrf_t
    use, intrinsic :: iso_fortran_env, only : ERROR_UNIT

    implicit none

    integer :: ierr
    type (state_fire_t) :: grid
    type (wrf_t) :: atm_state
    type (namelist_t) :: config_flags

#ifdef DM_PARALLEL
    call mpi_init (ierr)
    if (ierr /= MPI_SUCCESS) then
      write (ERROR_UNIT, *) 'ERROR: mpi_init failed'
      stop
    end if
#endif

      ! Read namelist
    call config_flags%Initialization (file_name = 'namelist.fire')

    call Init_atm_state (atm_state, config_flags)
    call Init_fire_state (grid, config_flags, atm_state)
    call grid%Save_state ()

    do while (grid%datetime_now < grid%datetime_end)
      call Advance_state (grid, config_flags)
      call grid%Handle_output (config_flags)
      call grid%Handle_wrfdata_update (atm_state, config_flags)
    end do

#ifdef DM_PARALLEL
    call mpi_finalize (ierr)
    if (ierr /= MPI_SUCCESS) then
      write (ERROR_UNIT, *) 'ERROR: mpi_finalize failed'
      stop
    end if
#endif

  end program fire_behavior
