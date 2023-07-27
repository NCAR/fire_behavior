  module advance_mod

    use state_mod, only : state_fire_t
    use module_fr_fire_fuel_anderson_mod, only: fuel_anderson_t
    use namelist_mod, only : namelist_t
    use module_fr_fire_driver, only : fire_driver_em
    use module_fr_fire_util, only : message

    implicit none

    private

    public :: Advance_state

  contains

    subroutine Advance_state (grid, fuel_model, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      type (state_fire_t), intent (in out) :: grid
      type (fuel_anderson_t), intent (in) :: fuel_model
      type (namelist_t), intent (in out) :: config_flags

      logical, parameter :: DEBUG_LOCAL = .true.


      if (DEBUG_LOCAL) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) '  Entering Advance_state... '
      end if

      grid%itimestep = grid%itimestep + 1

        ! Fire advance
      call message('Advance_state: FIRE step start',config_flags%fire_print_msg)
      call fire_driver_em (grid, fuel_model, config_flags)
      call message('Advance_state: FIRE step complete',config_flags%fire_print_msg)

      grid%datetime_now = grid%datetime_start
      call grid%datetime_now%Add_seconds (grid%itimestep * grid%dt)
      call grid%datetime_now%Print_datetime ()

      if (DEBUG_LOCAL) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) '  Leaving Advance_state... '
      end if

    end subroutine Advance_state

  end module advance_mod
