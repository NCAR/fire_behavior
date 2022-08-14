  program fire_behavior

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

    use wrf_atmosphere_mod, only : domain, grid_config_rec_type

    implicit none

    type (domain) :: grid
    type (grid_config_rec_type) :: config_flags
    logical, parameter :: DEBUG = .true.


    call config_flags%Set_default ()
    if (DEBUG) then
      write (OUTPUT_UNIT, *) 'Contents of config_flags:'
      call config_flags%Print ()
    end if

    call grid%Set_default ()
    if (DEBUG) then
      write (OUTPUT_UNIT, *) ''
      write (OUTPUT_UNIT, *) 'Contents of grid:'
      call grid%Print ()
    end if

  end program fire_behavior
