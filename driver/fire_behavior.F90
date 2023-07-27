  program fire_behavior

    use state_mod, only : state_fire_t
    use module_fr_fire_fuel_anderson_mod, only: fuel_anderson_t, Init_anderson_fuel_model
    use namelist_mod, only : namelist_t
    use initialize_mod, only : Init_fire_state, Init_atm_state
    use advance_mod, only : Advance_state
    use wrf_mod, only : wrf_t

    implicit none

    type (state_fire_t) :: grid
    type (fuel_anderson_t) :: fuel_model
    type (wrf_t) :: atm_state
    type (namelist_t) :: config_flags


      ! Read namelist
    call config_flags%Initialization (file_name = 'namelist.input')

    call Init_atm_state (atm_state, config_flags)
    call Init_anderson_fuel_model(fuel_model)
    call Init_fire_state (grid, fuel_model, config_flags, atm_state)
    call grid%Save_state ()

    do while (grid%datetime_now < grid%datetime_end)
      call Advance_state (grid, fuel_model, config_flags)
      call grid%Handle_output (config_flags)
      call grid%Provide_atm_feedback (atm_state, config_flags)
      call grid%Handle_wrfdata_update (atm_state, config_flags)
    end do

  end program fire_behavior
