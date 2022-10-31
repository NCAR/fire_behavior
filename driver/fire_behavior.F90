  program fire_behavior

!    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

    use state_mod, only : domain
    use namelist_mod, only : namelist_t
    use initialize_mod, only : Init_state
    use advance_mod, only : Advance_state

    implicit none

    type (domain) :: grid
    type (namelist_t) :: config_flags
    integer :: n
!    logical, parameter :: DEBUG = .true.


    call Init_state (grid, config_flags)

    do n = 1, config_flags%n_steps
      call Advance_state (grid, config_flags)
    end do

  end program fire_behavior
