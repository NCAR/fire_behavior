  program fire_behavior

    use state_mod, only : domain
    use namelist_mod, only : namelist_t
    use initialize_mod, only : Init_state
    use advance_mod, only : Advance_state

    implicit none

    type (domain) :: grid
    type (namelist_t) :: config_flags


    call Init_state (grid, config_flags)
    if (config_flags%atm_model == 'wrfdata') Call grid%Handle_wrfdata_update (config_flags)
    call grid%Save_state ()

    do while (grid%datetime_now < grid%datetime_end)
      call Advance_state (grid, config_flags)
      call grid%Handle_output (config_flags)
    end do

  end program fire_behavior
