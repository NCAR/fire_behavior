  module initialize_mod
 
    use state_mod, only : domain
    use namelist_mod, only : namelist_t
    use module_fr_fire_driver_wrf, only : fire_driver_em_init

    private

    public :: Init_state

    contains

      subroutine Init_state (grid, config_flags)

        use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

        implicit none

        type (domain), intent (in out) :: grid
        type (namelist_t), intent (in out) :: config_flags

        logical, parameter :: DEBUG_LOCAL = .true.


        if (DEBUG_LOCAL) then
          write (OUTPUT_UNIT, *) ''
          write (OUTPUT_UNIT, *) '  Entering subroutine Init_fire_behavior_model'
        end if

        call fire_driver_em_init (grid , config_flags                        &
                ,grid%ids, grid%ide, grid%kds, grid%kde, grid%jds, grid%jde  &
                ,grid%ims, grid%ime, grid%kms, grid%kme, grid%jms, grid%jme  &
                ,grid%ips, grid%ipe, grid%kps, grid%kpe, grid%jps, grid%jpe)

        if (DEBUG_LOCAL) then
          write (OUTPUT_UNIT, *) ''
          write (OUTPUT_UNIT, *) '  Leaving subroutine Init_fire_behavior_model'
        end if

    end subroutine Init_state

  end module initialize_mod
