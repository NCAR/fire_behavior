! WRF:MEDIATION_LAYER:FIRE_MODEL


#define DEBUG_OUT

module module_fr_fire_driver_wrf
! wrf-specific driver

use module_fr_fire_driver, only : fire_driver_em
use module_fr_fire_util, only : message
use state_mod, only: domain
use namelist_mod , only : namelist_t

implicit none

  private

  public :: fire_driver_em_init, fire_driver_em_step

contains

    ! stub to call fire_driver_em with irun=0 and omit last 3 args
subroutine fire_driver_em_init (grid , config_flags)

    implicit none

    type (domain), target :: grid
    type (namelist_t), intent(in) :: config_flags

    call message ('fire_driver_em_init: FIRE initialization start')

    call fire_driver_em (grid, config_flags            &
            ,1,2,0                                     & ! ifun start, end, test steps
            ,grid%ifds,grid%ifde - grid%sr_x, grid%jfds,grid%jfde - grid%sr_y &
            ,grid%ifms,grid%ifme, grid%jfms,grid%jfme  &
            ,grid%ifts,grid%ifte - grid%sr_x, grid%jfts,grid%jfte - grid%sr_y  &
            )

    call message ('fire_driver_em_init: FIRE initialization complete')

end subroutine fire_driver_em_init

    ! stub to call fire_driver_em
subroutine fire_driver_em_step (grid, config_flags)

    USE module_fr_fire_util, only : fire_test_steps

    implicit none

    type (domain), target :: grid
    type (namelist_t), intent(in) :: config_flags

    call message('fire_driver_em_step: FIRE step start')

      ! PAJM: These calls only necessary if the atm state has been updated
      ! So we are doing the interpolation more than needed

    call fire_driver_em (grid, config_flags            &
            ,3,6,fire_test_steps                       &
            ,grid%ifds,grid%ifde - grid%sr_x, grid%jfds,grid%jfde - grid%sr_y  &
            ,grid%ifms,grid%ifme, grid%jfms,grid%jfme  &
            ,grid%ifts,grid%ifte - grid%sr_x, grid%jfts,grid%jfte - grid%sr_y  &
            )

    call message('fire_driver_em_step: FIRE step complete')

end subroutine fire_driver_em_step

end module module_fr_fire_driver_wrf

