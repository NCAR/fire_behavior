! WRF:MEDIATION_LAYER:FIRE_MODEL


#define DEBUG_OUT

module module_fr_fire_driver_wrf
! wrf-specific driver

use module_fr_fire_driver
use module_fr_fire_atm
use wrf_atmosphere_mod, only: reradius,    & ! 1/earth radiusw
                                  pi2        ! 2*pi
implicit none

contains

subroutine fire_driver_em_init (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,its,ite, kts,kte, jts,jte)

    ! stub to call fire_driver_em with irun=0 and omit last 3 args

    USE wrf_atmosphere_mod, only: get_ijk_from_subgrid
    USE state_mod, only: domain
    USE namelist_mod, only : namelist_t
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
    ! dummies

    real,dimension(1,1,1)::rho,z_at_w,dz8w
    
    call message('fire_driver_em_init: FIRE initialization start')

    ! get fire mesh dimensions
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe,                        &
                            ifts,ifte, jfts,jfte,kfts,kfte)

    call calc_unit_fxlat_fxlong (grid, config_flags)

    call calc_fxlat_fxlong (grid, config_flags)

    call fire_driver_em ( grid , config_flags               & 
            ,1,2,0                        & ! ifun start, end, test steps
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ) 

    call message('fire_driver_em_init: FIRE initialization complete')

end subroutine fire_driver_em_init

!
!***
!

subroutine fire_driver_em_step (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,rho,z_at_w,dz8w ) 

    ! stub to call fire_driver_em 

    USE wrf_atmosphere_mod, only: get_ijk_from_subgrid
    USE state_mod, only: domain
    USE namelist_mod , only : namelist_t
    USE module_fr_fire_util, only : fire_test_steps
    implicit none

    TYPE(domain) , TARGET          :: grid   ! data
    TYPE (namelist_t) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe
    real,dimension(ims:ime, kms:kme, jms:jme),intent(in)::rho,z_at_w,dz8w

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

    call fire_driver_em ( grid , config_flags               & 
            ,3,6,fire_test_steps                                &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,rho,z_at_w,dz8w                                        &
            )
 

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
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx,        & ! fluxes on atm grid 
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt,                &
            grid%ht,z_at_w,dz8w,grid%mut,grid%c1h,grid%c2h,rho,          &
            grid%rthfrten,grid%rqvfrten)                ! out

     enddo

       ! debug print to compare

       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,grid%rthfrten,'fire_driver_phys:rthfrten')
       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,grid%rqvfrten,'fire_driver_phys:rqvfrten')

    call message('fire_driver_em_step: FIRE step complete')
            
end subroutine fire_driver_em_step

subroutine calc_unit_fxlat_fxlong (grid, config_flags)

    USE state_mod, only: domain
    USE namelist_mod , only : namelist_t
    implicit none
!*** arguments
    TYPE(domain) , TARGET :: grid                   ! state
    TYPE (namelist_t) , INTENT(IN)  :: config_flags ! namelist

!*** local
    logical :: real,ideal

    ! figure out which kind of coordinates from the first given
    ideal=config_flags%fire_ignition_start_x1 .ne.0. .or. config_flags%fire_ignition_start_y1 .ne. 0.
    real=config_flags%fire_ignition_start_lon1 .ne. 0. .or. config_flags%fire_ignition_start_lat1 .ne. 0.
    if(ideal)grid%fire_ignition_longlat = 0
    if(ideal)call message('Using ideal ignition coordinates, m from the lower left domain corner')
    if(real)grid%fire_ignition_longlat = 1
    if(real)call message('Using real ignition coordinates, longitude and latitude')
    if(ideal.and.real)call crash('Only one of the ideal or real coordinates may be given')

    if(grid%fire_ignition_longlat .eq. 0)then
       ! ideal
       !  ignition is in m
       grid%unit_fxlong=1.
       grid%unit_fxlat=1.
       ! will set fire mesh coordinates to uniform mesh below
    else
       ! real
       ! 1 degree in m (approximate OK)
       grid%unit_fxlat=pi2/(360.*reradius)  ! earth circumference in m / 360 degrees
       grid%unit_fxlong=cos(config_flags%cen_lat*pi2/360.)*grid%unit_fxlat  ! latitude
    endif

end subroutine calc_unit_fxlat_fxlong

subroutine calc_fxlat_fxlong (grid, config_flags)

    USE state_mod, only: domain
    USE namelist_mod , only : namelist_t
    implicit none
!*** arguments
    TYPE(domain) , TARGET :: grid                   ! state
    TYPE (namelist_t) , INTENT(IN)  :: config_flags ! namelist

!*** local
    logical :: use_atm_vars=.true.   !  interpolate wind from atm mesh and average output fluxes back
    integer :: pid, id

    if(config_flags%restart)then

          call message('restart - topo initialization skipped')

    else

        if(grid%fire_ignition_longlat .eq.0)then
            ! set ideal fire mesh coordinates - used for ignition only
            ! do not forget to set unit_fxlong, unit_fxlat outside of parallel loop

            ! DME: Next call added to fixe a bug when
            ! initializing a nested domain (before, fxlong & fxlat where not
            ! assigned, they were supposed to be set by a mod in WPS
            ! but here we use standard WPS therefore were zero and fire does not ignite)
            call set_ideal_coord( grid%dxf,grid%dyf, &
                grid%ifds,grid%ifde,grid%jfds,grid%jfde,  &
                grid%ifms,grid%ifme,grid%jfms,grid%jfme,  &
                grid%ifts,grid%ifte,grid%jfts,grid%jfte,  &
                grid%fxlong,grid%fxlat          )

        elseif(use_atm_vars)then
            ! assume halo xlong xlat
            ! interpolate nodal coordinates

        pid = 1
        id = ifval(pid.ne.0,pid+1*10000,0) ! for print

        call interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
            grid%ids,grid%ide,  grid%jds,grid%jde,                    & ! atm grid dimensions
            grid%ims,grid%ime,  grid%jms,grid%jme,                    &
            grid%ips,grid%ipe,grid%jps,grid%jpe,                              &
            grid%its,grid%ite,grid%jts,grid%jte,                              &
            grid%ifds, grid%ifde, grid%jfds, grid%jfde,                       & ! fire grid dimensions
            grid%ifms, grid%ifme, grid%jfms, grid%jfme,                       &
            grid%ifts,grid%ifte,grid%jfts,grid%jfte,                          &
            grid%sr_x,grid%sr_y,                                        & ! atm/fire grid ratio
            grid%xlat,                                       & ! atm grid arrays in
            grid%fxlat,0)                                      ! fire grid arrays out

        call interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
            grid%ids,grid%ide,  grid%jds,grid%jde,                    & ! atm grid dimensions
            grid%ims,grid%ime,  grid%jms,grid%jme,                    &
            grid%ips,grid%ipe,grid%jps,grid%jpe,                              &
            grid%its,grid%ite,grid%jts,grid%jte,                              &
            grid%ifds, grid%ifde, grid%jfds, grid%jfde,                       & ! fire grid dimensions
            grid%ifms, grid%ifme, grid%jfms, grid%jfme,                       &
            grid%ifts,grid%ifte,grid%jfts,grid%jfte,                          &
            grid%sr_x,grid%sr_y,                                        & ! atm/fire grid ratio
            grid%xlong,                                       & ! atm grid arrays in
            grid%fxlong,0)                                      ! fire grid arrays out

        end if

    end if

end subroutine calc_fxlat_fxlong

end module module_fr_fire_driver_wrf

