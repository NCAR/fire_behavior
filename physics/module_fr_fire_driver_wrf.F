! WRF:MEDIATION_LAYER:FIRE_MODEL


#define DEBUG_OUT

module module_fr_fire_driver_wrf
! wrf-specific driver

use module_fr_fire_driver, only : fire_driver_em
use module_fr_fire_util, only : message
use state_mod, only: domain, Get_ijk_from_subgrid
use namelist_mod , only : namelist_t

implicit none

  private

  public :: fire_driver_em_init, fire_driver_em_step

contains

subroutine fire_driver_em_init (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,its,ite, kts,kte, jts,jte)

    ! stub to call fire_driver_em with irun=0 and omit last 3 args


    implicit none

    TYPE(domain) , TARGET          :: grid   ! data
    TYPE (namelist_t) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,its,ite, kts,kte, jts,jte

    ! local
    integer :: &  ! fire mesh sizes
             ifds,ifde, kfds,kfde, jfds,jfde,                              &
             ifms,ifme, kfms,kfme, jfms,jfme,                              &
             ifps,ifpe, kfps,kfpe, jfps,jfpe,                              &
             ifts,ifte, kfts,kfte, jfts,jfte

    
    call message('fire_driver_em_init: FIRE initialization start')

    ! get fire mesh dimensions
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe,                        &
                            ifts,ifte, jfts,jfte,kfts,kfte)

    call fire_driver_em ( grid , config_flags               & 
            ,1,2,0                        & ! ifun start, end, test steps
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,ifts,ifte, jfts,jfte                                   &
            ) 

    call message('fire_driver_em_init: FIRE initialization complete')

end subroutine fire_driver_em_init

!
!***
!

subroutine fire_driver_em_step (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe)
            

    ! stub to call fire_driver_em 

    USE module_fr_fire_util, only : fire_test_steps

    implicit none

    TYPE(domain) , TARGET          :: grid   ! data
    TYPE (namelist_t) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe

    ! local
    integer :: &  ! fire mesh sizes
             ifds,ifde, kfds,kfde, jfds,jfde,                              &
             ifms,ifme, kfms,kfme, jfms,jfme,                              &
             ifps,ifpe, kfps,kfpe, jfps,jfpe,                              &
             ifts,ifte, kfts,kfte, jfts,jfte
    integer :: its,ite,jts,jte,kts,kte            ! atm tile
    integer:: ij

    call message('fire_driver_em_step: FIRE step start')

    ! get fire mesh dimensions
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe,                        &
                            ifts,ifte, jfts,jfte,kfts,kfte)

      ! PAJM: These calls only necessary if the atm state has been updated
      ! So we are doing the interpolation more than needed

    call fire_driver_em ( grid , config_flags               & 
            ,3,6,fire_test_steps                                &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,ifts,ifte, jfts,jfte                                   &
            )
 
    call message('fire_driver_em_step: FIRE step complete')
            
end subroutine fire_driver_em_step

end module module_fr_fire_driver_wrf

