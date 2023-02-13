! WRF:MEDIATION_LAYER:FIRE_MODEL


#define DEBUG_OUT

module module_fr_fire_driver_wrf
! wrf-specific driver

use module_fr_fire_driver, only : fire_driver_em
use module_fr_fire_atm, only : fire_tendency, calc_smoke_emissions, add_fire_tracer_emissions
use module_fr_fire_util, only : message, crash, print_3d_stats, sum_2d_cells
  use wrf_mod, only : G
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
 
    call sum_2d_fire_vars (grid, config_flags)

    if (config_flags%tracer_opt.eq.3)then
       call calc_smoke_emissions(grid,config_flags,ifts,ifte,jfts,jfte)

       call add_fire_tracer_emissions(config_flags%tracer_opt,grid%dt,grid%dx,grid%dy, &
                 ifms,ifme,jfms,jfme,    &
                 ifps,ifpe,jfps,jfpe,    &
                 ids,ide,kds,kde,jds,jde,      &
                 ims,ime,kms,kme,jms,jme,      &
                 ips,ipe,kps,kpe,jps,jpe,      &
                 grid%rho,grid%dz8w,           &
                 grid%burnt_area_dt,grid%fgip, &
                 grid%tracer,grid%emis_smoke,config_flags%fire_tracer_smoke)
    end if

    ! --- add heat and moisture fluxes to tendency variables by postulated decay
    do ij=1,grid%num_tiles
       ! FIRE works on domain by 1 smaller, in last row&col winds are not set properly
       its = grid%i_start(ij)             ! start atmospheric tile in i
       ite = min(grid%i_end(ij),ide-1)    ! end atmospheric tile in i
       jts = grid%j_start(ij)             ! start atmospheric tile in j
       jte = min(grid%j_end(ij),jde-1)    ! end atmospheric tile in j
       kts=kds
       kte=kde

       call fire_tendency(                 &
            ids,ide-1, kds,kde, jds,jde-1, & ! domain dimensions
            ims,ime, kms,kme, jms,jme,      &
            its,ite, kts,kte, jts,jte,      & ! 
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx,            & ! fluxes on atm grid 
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt, &
            grid%z_at_w,grid%dz8w,grid%mut,grid%c1h,grid%c2h,grid%rho,  &
            grid%rthfrten,grid%rqvfrten)                ! out

     enddo

       ! debug print to compare

       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,grid%rthfrten,'fire_driver_phys:rthfrten')
       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,grid%rqvfrten,'fire_driver_phys:rqvfrten')

    call message('fire_driver_em_step: FIRE step complete')
            
end subroutine fire_driver_em_step

subroutine sum_2d_fire_vars (grid, config_flags)

    implicit none
!*** arguments
    TYPE(domain) , TARGET :: grid                   ! state
    TYPE (namelist_t) , INTENT(IN)  :: config_flags ! namelist

!*** local
    character(len=256)::msg
    logical :: use_atm_vars=.true.   !  interpolate wind from atm mesh and average output fluxes back
    real :: s
    integer :: i, j

    ! sum the fluxes over atm cells
    if(use_atm_vars)then
      call sum_2d_cells(        &
        grid%ifms,grid%ifme,grid%jfms,grid%jfme,  &
        grid%ifts,grid%ifte,grid%jfts,grid%jfte,  &
        grid%fuel_frac,              &
        grid%ims, grid%ime, grid%jms, grid%jme,   &
        grid%its,grid%ite,grid%jts,grid%jte,      &
        grid%avg_fuel_frac)
      call sum_2d_cells(        &
        grid%ifms,grid%ifme,grid%jfms,grid%jfme,  &
        grid%ifts,grid%ifte,grid%jfts,grid%jfte,  &
        grid%fgrnhfx,              &
        grid%ims, grid%ime, grid%jms, grid%jme,   &
        grid%its,grid%ite,grid%jts,grid%jte,      &
        grid%grnhfx)
!comment out the next call to get results as before commit 55fd92051196b796891b60cb7ec1c4bdb8800078
      call sum_2d_cells(        &
        grid%ifms,grid%ifme,grid%jfms,grid%jfme,  &
        grid%ifts,grid%ifte,grid%jfts,grid%jfte,  &
        grid%fgrnqfx,              &
        grid%ims, grid%ime, grid%jms, grid%jme,   &
        grid%its,grid%ite,grid%jts,grid%jte,      &
        grid%grnqfx)

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
      write(msg,'(a,f6.3)')'fire-atmosphere feedback scaling ',config_flags%fire_atm_feedback
!$OMP end CRITICAL(FIRE_DRIVER_CRIT)
      call message(msg)
      s = 1./(grid%sr_x*grid%sr_y)
      do j=grid%jts,grid%jte
        do i=grid%its,grid%ite
            ! DME heat fluxes contribution for the case wiythout feedback
            grid%grnhfx_fu(i,j)=grid%grnhfx(i,j)*s
            grid%grnqfx_fu(i,j)=grid%grnqfx(i,j)*s
            ! scale surface fluxes to get the averages
            grid%avg_fuel_frac(i,j)=grid%avg_fuel_frac(i,j)*s
            grid%grnhfx(i,j)=config_flags%fire_atm_feedback*grid%grnhfx(i,j)*s
            grid%grnqfx(i,j)=config_flags%fire_atm_feedback*grid%grnqfx(i,j)*s
            ! we do not have canopy fluxes yet...
            grid%canhfx(i,j)=0
            grid%canqfx(i,j)=0
        enddo
      enddo
   endif

end subroutine sum_2d_fire_vars

end module module_fr_fire_driver_wrf

