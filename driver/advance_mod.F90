  module advance_mod

    use state_mod, only : domain
    use namelist_mod, only : namelist_t
    use module_fr_fire_driver_wrf, only : fire_driver_em_step

    implicit none

    private

    public :: Advance_state

  contains

    subroutine Advance_state (grid, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      type (domain), intent (in out) :: grid
      type (namelist_t), intent (in out) :: config_flags

      logical, parameter :: DEBUG_LOCAL = .true.


      if (DEBUG_LOCAL) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) '  Entering Advance_state... '
      end if

      grid%itimestep = grid%itimestep + 1

      call fire_driver_em_step (grid , config_flags & 
            ,grid%ids,grid%ide, grid%kds, grid%kde, grid%jds, grid%jde  &
            ,grid%ims, grid%ime, grid%kms, grid%kme, grid%jms, grid%jme &
            ,grid%ips, grid%ipe, grid%kps, grid%kpe, grid%jps, grid%jpe)


      if (config_flags%atm_model == 'test1') write (34, *) grid%rthfrten(42, 1, 42), grid%rqvfrten(42, 1, 42)

      grid%datetime_now = grid%datetime_start
      call grid%datetime_now%Add_seconds (grid%itimestep * grid%dt)
      call grid%datetime_now%Print_datetime ()

      if (DEBUG_LOCAL) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) '  Leaving Advance_state... '
      end if

    end subroutine Advance_state

  end module advance_mod
