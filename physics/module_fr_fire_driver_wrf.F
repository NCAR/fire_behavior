! WRF:MEDIATION_LAYER:FIRE_MODEL


#define DEBUG_OUT

module module_fr_fire_driver_wrf
! wrf-specific driver

use module_fr_fire_driver, only : fire_driver_em
use module_fr_fire_atm, only : fire_tendency, calc_smoke_emissions, add_fire_tracer_emissions
use module_fr_fire_util, only : ifval, set_ideal_coord, message, interpolate_z2fire, crash, &
                                print_2d_stats, print_3d_stats, snode, write_array_m, write_array_m3, &
                                continue_at_boundary, interpolate_2d, print_2d_stats_vec, sum_2d_cells
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
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,rho,z_at_w,dz8w ) 

    ! stub to call fire_driver_em 

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

      ! PAJM: These calls only necessary if the atm state has been updated
      ! So we are doing the interpolation more than needed
    call interpolate_winds2fire (grid, config_flags)

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
                 ids,ide,kds,kde,jds,jde,          &
                 ims,ime,kms,kme,jms,jme,          &
                 ips,ipe,kps,kpe,jps,jpe,          &
                 rho,dz8w,                         &
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

subroutine calc_fxlat_fxlong (grid, config_flags)

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

subroutine interpolate_winds2fire (grid, config_flags)

    implicit none
!*** arguments
    TYPE(domain) , TARGET :: grid                   ! state
    TYPE (namelist_t) , INTENT(IN)  :: config_flags ! namelist

!*** local
    logical :: use_atm_vars=.true.   !  interpolate wind from atm mesh and average output fluxes back
    integer :: pid, id

    pid = 1
    id = ifval(pid.ne.0,pid+1*10000,0) ! for print

    if(use_atm_vars)then

    call interpolate_atm2fire(config_flags, id,           & ! flag for debug output
            config_flags%fire_wind_height,                & ! height to interpolate to
            grid%ids,grid%ide-1, grid%kds,grid%kde, grid%jds,grid%jde-1,                &
            grid%ims,grid%ime, grid%kms,grid%kme, grid%jms,grid%jme,                    &
            grid%ips,min(grid%ipe,grid%ide-1), grid%jps,min(grid%jpe,grid%jde-1), &
            grid%i_start(1),min(grid%i_end(1),grid%ide-1),           &
            grid%j_start(1),min(grid%j_end(1),grid%jde-1),           &
            grid%ifds,grid%ifde-grid%sr_x, grid%jfds,grid%jfde-grid%sr_y,                    &
            grid%ifms, grid%ifme, grid%jfms, grid%jfme,                       &
            grid%ifps,min(grid%ifpe,grid%ifde-grid%sr_x), grid%jfps,min(grid%jfpe,grid%jfde-grid%sr_y),      &
            grid%ifts, grid%ifte, grid%jfts, grid%jfte,                       &
            grid%sr_x,grid%sr_y,                          & ! atm/fire grid ratio
            grid%u_frame, grid%v_frame,                   & ! velocity frame correction
            grid%u_2,grid%v_2,                            & ! 3D atm grid arrays in
            grid%ph_2,grid%phb,                           &
            grid%z0,grid%ht,                              & ! 2D atm grid arrays in
            grid%uah,grid%vah,                            & ! 2D atm grid out
            grid%uf,grid%vf,grid%fz0)                       ! fire grid arrays out

    end if

end subroutine interpolate_winds2fire

subroutine interpolate_atm2fire(config_flags, id, & ! for debug output, <= 0 no output
    fire_wind_height,                             & ! interpolation height
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe,jps,jpe,                              &
    its,ite,jts,jte,                              &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
    ifts,ifte,jfts,jfte,                          &
    ir,jr,                                        & ! atm/fire grid ratio
    u_frame, v_frame,                             & ! velocity frame correction
    u,v,                                          & ! atm grid arrays in
    ph,phb,                                       &
    z0,zs,                                        &
    uah,vah,                                      &
    uf,vf,z0f)                                          ! fire grid arrays out

implicit none
!*** purpose: interpolate winds and height

!*** arguments
TYPE (namelist_t) , INTENT(IN)          :: config_flags
integer, intent(in)::id
real, intent(in):: fire_wind_height                 ! height above the terrain for vertical interpolation
integer, intent(in)::                             &
    ids,ide, kds,kde, jds,jde,                    & ! atm domain bounds
    ims,ime, kms,kme, jms,jme,                    & ! atm memory bounds
    ips,ipe,jps,jpe,                              &
    its,ite,jts,jte,                              & ! atm tile bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
    ifts,ifte,jfts,jfte,                          & ! fire tile bounds
    ir,jr                                         ! atm/fire grid refinement ratio
real, intent(in):: u_frame, v_frame                 ! velocity frame correction
real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::&
    u,v,                                          & ! atm wind velocity, staggered
    ph,phb                                          ! geopotential
real,intent(in),dimension(ims:ime,jms:jme)::&
    z0,                                           & ! roughness height
    zs                                              ! terrain height
real,intent(out),dimension(ims:ime,jms:jme)::&
    uah,                                           & ! atm wind at fire_wind_height, diagnostics
    vah                                              !
real,intent(out), dimension(ifms:ifme,jfms:jfme)::&
    uf,vf                                           ! wind velocity fire grid nodes
real,intent(in),dimension(ifms:ifme,jfms:jfme)::z0f ! roughness length in fire grid


!*** local
character(len=256)::msg
#define TDIMS its-2,ite+2,jts-2,jte+2
real, dimension(its-2:ite+2,jts-2:jte+2):: ua,va   ! atm winds, interpolated over height, still staggered grid
real, dimension(its-2:ite+2,kds:kde,jts-2:jte+2):: altw,altub,altvb,hgtu,hgtv ! altitudes
integer:: i,j,k,ifts1,ifte1,jfts1,jfte1,ite1,jte1
integer::itst,itet,jtst,jtet,itsu,iteu,jtsu,jteu,itsv,itev,jtsv,jtev
integer::kdmax,its1,jts1,ips1,jps1
integer::itsou,iteou,jtsou,jteou,itsov,iteov,jtsov,jteov
real:: ground,loght,loglast,logz0,logfwh,ht,zr
real::r_nan
integer::i_nan
equivalence (i_nan,r_nan)
real::fire_wind_height_local,z0fc
real::ust_d,wsf,wsf1,uf_temp,vf_temp
real,parameter::vk_kappa=0.4

!*** executable

! debug init local arrays
i_nan=2147483647
ua=r_nan
va=r_nan
altw=r_nan
altub=r_nan
hgtu=r_nan
hgtv=r_nan


if(kds.ne.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,*)'WARNING: bottom index kds=',kds,' should be 1?'
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif

!                            ^ j
!        ------------        |
!        |          |         ----> i
!        u    p     |
!        |          |    nodes in cell (i,j)
!        ------v-----    view from top
!
!             v(ide,jde+1)
!            -------x------
!            |            |
!            |            |
! u(ide,jde) x      x     x u(ide+1,jde)
!            | p(ide,hde) |
!            |            |   p=ph,phb,z0,...
!            -------x------
!              v(ide,jde)
!
! staggered values set u(ids:ide+1,jds:jde) v(ids:ide,jds:jde+1)
! p=ph+phb set at (ids:ide,jds:jde)
! location of u(i,j) needs p(i-1,j) and p(i,j)
! location of v(i,j) needs p(i,j-1) and p(i,j)
! *** NOTE need HALO in ph, phb
! so we can compute only u(ids+1:ide,jds:jde) v(ids:ide,jds+1,jde)
! unless we extend p at the boundary
! but because we care about the fire way in the inside it does not matter
! if the fire gets close to domain boundary the simulation is over anyway

    ite1=snode(ite,ide,1)
    jte1=snode(jte,jde,1)
    ! do this in any case to check for nans
    call print_3d_stats(its,ite1,kds,kde,jts,jte,ims,ime,kms,kme,jms,jme,u,'wind U in')
    call print_3d_stats(its,ite,kds,kde,jts,jte1,ims,ime,kms,kme,jms,jme,v,'wind V in')

    if(config_flags%fire_print_msg.gt.0)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
       write(msg,'(a,f7.2,a)')'interpolate_atm2fire: log-interpolation of wind to',fire_wind_height,' m'
       call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
    endif

    ! indexing

    ! file for u
    itst=ifval(ids.eq.its,its,its-1)
    itet=ifval(ide.eq.ite,ite,ite+1)
    jtst=ifval(jds.ge.jts,jts,jts-1)
    jtet=ifval(jde.eq.jte,jte,jte+1)

if(config_flags%fire_print_msg.ge.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,7001)'atm input  ','tile',its,ite,jts,jte
  call message(msg)
  write(msg,7001)'altw       ','tile',itst,itet,jtst,jtet
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif
7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)

    kdmax=kde-1   ! max layer to interpolate from, can be less

    do j = jtst,jtet
      do k=kds,kdmax+1
        do i = itst,itet
          altw(i,k,j) = (ph(i,k,j)+phb(i,k,j)) / G             ! altitude of the bottom w-point
        enddo
      enddo
    enddo

! values at u points
    itsu=ifval(ids.eq.its,its+1,its)  ! staggered direction
    iteu=ifval(ide.eq.ite,ite,ite+1)  ! staggered direction
    jtsu=ifval(jds.ge.jts,jts,jts-1)
    jteu=ifval(jde.eq.jte,jte,jte+1)

if(config_flags%fire_print_msg.ge.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,7001)'u interp at','tile',itsu,iteu,jtsu,jteu
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif

    do j = jtsu,jteu
      do k=kds,kdmax+1
        do i = itsu,iteu
          altub(i,k,j)= 0.5*(altw(i-1,k,j)+altw(i,k,j))      ! altitude of the bottom point under u-point
        enddo
      enddo
      do k=kds,kdmax
        do i = itsu,iteu
          hgtu(i,k,j) =  0.5*(altub(i,k,j)+altub(i,k+1,j)) - altub(i,kds,j)  ! height of the u-point above the ground
        enddo
      enddo
    enddo

! values at v points
    jtsv=ifval(jds.eq.jts,jts+1,jts)  ! staggered direction
    jtev=ifval(jde.eq.jte,jte,jte+1)  ! staggered direction
    itsv=ifval(ids.ge.its,its,its-1)
    itev=ifval(ide.eq.ite,ite,ite+1)

if(config_flags%fire_print_msg.ge.1)then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
  write(msg,7001)'v interp at','tile',itsv,itev,jtsv,jtev
  call message(msg)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
endif
    do j = jtsv,jtev
      do k=kds,kdmax+1
        do i = itsv,itev
          altvb(i,k,j)= 0.5*(altw(i,k,j-1)+altw(i,k,j))      ! altitude of the bottom point under v-point
        enddo
      enddo
      do k=kds,kdmax
        do i = itsv,itev
          hgtv(i,k,j) =  0.5*(altvb(i,k,j)+altvb(i,k+1,j)) - altvb(i,kds,j)  ! height of the v-point above the ground
        enddo
      enddo
    enddo

#ifdef DEBUG_OUT
        call write_array_m3(itsu,iteu,kds,kdmax,jtsu,jteu,its-2,ite+2,kds,kde,jts-2,jte+2,altub,'altub',id)
        call write_array_m3(itsv,itev,kds,kdmax,jtsv,jtev,its-2,ite+2,kds,kde,jts-2,jte+2,altvb,'altvb',id)
        call write_array_m3(itsu,iteu,kds,kdmax,jtsu,jteu,its-2,ite+2,kds,kde,jts-2,jte+2,hgtu,'hgtu',id)
        call write_array_m3(itsv,itev,kds,kdmax,jtsv,jtev,its-2,ite+2,kds,kde,jts-2,jte+2,hgtv,'hgtv',id)
#endif
! DME
if (config_flags%fire_lsm_zcoupling) then
  logfwh = log(config_flags%fire_lsm_zcoupling_ref)
  fire_wind_height_local = config_flags%fire_lsm_zcoupling_ref
else
  logfwh = log(fire_wind_height)
  fire_wind_height_local = fire_wind_height
endif

    ! interpolate u, staggered in X

    do j = jtsu,jteu            ! compute on domain by 1 smaller, otherwise z0 and ph not available
!      do i = itsu,iteu        ! compute with halo 2
      do i = itsu,iteu
        zr = 0.5*(z0(i,j)+z0(i-1,j)) ! interpolated roughness length under this u point
        if(fire_wind_height_local > zr)then       !
          do k=kds,kdmax
            ht = hgtu(i,k,j)      ! height of this u point above the ground
            if( .not. ht < fire_wind_height_local) then ! found layer k this point is in
              loght = log(ht)
              if(k.eq.kds)then               ! first layer, log linear interpolation from 0 at zr
                logz0 = log(zr)
                ua(i,j)= u(i,k,j)*(logfwh-logz0)/(loght-logz0)
              else                           ! log linear interpolation
                loglast=log(hgtu(i,k-1,j))
                ua(i,j)= u(i,k-1,j) + (u(i,k,j) - u(i,k-1,j)) * ( logfwh - loglast) / (loght - loglast)
              endif
              goto 10
            endif
            if(k.eq.kdmax)then                 ! last layer, still not high enough
              ua(i,j)=u(i,k,j)
            endif
          enddo
10        continue
        else  ! roughness higher than the fire wind height
          ua(i,j)=0.
        endif
      enddo
    enddo

    do j = jtsu,jteu
      ua(itsu-1,j)=ua(itsu,j)
    enddo


    ! interpolate v, staggered in Y

    do j = jtsv,jtev
      do i = itsv,itev
        zr = 0.5*(z0(i,j-1)+z0(i,j)) ! roughness length under this v point
        if(fire_wind_height_local > zr)then       !
          do k=kds,kdmax
            ht = hgtv(i,k,j)      ! height of this u point above the ground
            if( .not. ht < fire_wind_height_local) then ! found layer k this point is in
              loght = log(ht)
              if(k.eq.kds)then               ! first layer, log linear interpolation from 0 at zr
                logz0 = log(zr)
                va(i,j)= v(i,k,j)*(logfwh-logz0)/(loght-logz0)
              else                           ! log linear interpolation
                loglast=log(hgtv(i,k-1,j))
                va(i,j)= v(i,k-1,j) + (v(i,k,j) - v(i,k-1,j)) * ( logfwh - loglast) / (loght - loglast)
              endif
              goto 11
            endif
            if(k.eq.kdmax)then                 ! last layer, still not high enough
              va(i,j)=v(i,k,j)
            endif
          enddo
11        continue
        else  ! roughness higher than the fire wind height
          va(i,j)=0.
        endif
      enddo
    enddo

    do i = itsv,itev
      va(i,jtsv-1)=va(i,jtsv)
    enddo

#ifdef DEBUG_OUT
!   store the output for diagnostics
    do j = jts,jte1
      do i = its,ite1
        uah(i,j)=ua(i,j)
        vah(i,j)=va(i,j)
      enddo
    enddo

    call write_array_m(its,ite1,jts,jte,ims,ime,jms,jme,uah,'uah_n',id) ! no reflection
    call write_array_m(its,ite,jts,jte1,ims,ime,jms,jme,vah,'vah_n',id)
#endif

    ips1 = ifval(ips.eq.ids,ips+1,ips)

    call continue_at_boundary(1,1,0., & ! x direction
       TDIMS,                  &! memory dims atm grid tile
       ids+1,ide,jds,jde, &     ! domain dims - where u defined
       ips1,ipe,jps,jpe, &     ! patch dims
       itsu,iteu,jtsu,jteu, & ! tile dims - in nonextended direction one beyond if at patch boundary but not domain
       itsou,iteou,jtsou,jteou, & ! out, where set
       ua)                           ! array

    jps1 = ifval(jps.eq.jds,jps+1,jps)

    call continue_at_boundary(1,1,0., & ! y direction
       TDIMS,                  & ! memory dims atm grid tile
       ids,ide,jds+1,jde, &      ! domain dims - where v wind defined
       ips,ipe,jps1,jpe, &        ! patch dims
       itsv,itev,jtsv,jtev, & ! tile dims
       itsov,iteov,jtsov,jteov, & ! where set
       va)                           ! array

!   store the output for diagnostics
    do j = jts,jte1
      do i = its,ite1
        uah(i,j)=ua(i,j)
        vah(i,j)=va(i,j)
      enddo
    enddo

#ifdef DEBUG_OUT
        call write_array_m(itsou,iteou,jtsou,jteou,TDIMS,ua,'ua',id)
        call write_array_m(itsov,iteov,jtsov,jteov,TDIMS,va,'va',id)
#endif

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    ! don't have all values valid, don't check
     write(msg,12)'atm mesh wind U at',fire_wind_height,' m'
     call print_2d_stats(itsou,iteou,jtsou,jteou,TDIMS,ua,msg)
     write(msg,12)'atm mesh wind V at',fire_wind_height,' m'
     call print_2d_stats(itsov,iteov,jtsov,jteov,TDIMS,va,msg)
12  format(a,f6.2,a)
    call print_2d_stats(its,ite1,jts,jte,ims,ime,jms,jme,uah,'UAH')
    call print_2d_stats(its,ite,jts,jte1,ims,ime,jms,jme,vah,'VAH')
    !call write_array_m(its,ite1,jts,jte,ims,ime,jms,jme,uah,'uah',id)
    !call write_array_m(its,ite,jts,jte1,ims,ime,jms,jme,vah,'vah',id)
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)

!      ---------------
!     | F | F | F | F |   Example of atmospheric and fire grid with
!     |-------|-------|   ir=jr=4.
!     | F | F | F | F |   Winds are given at the midpoints of the sides of the atmosphere grid,
!     ua------z-------|   interpolated to midpoints of the cells of the fine fire grid F.
!     | F | F | F | F |   This is (1,1) cell of atmosphere grid, and [*] is the (1,1) cell of the fire grid.
!     |---------------|   ua(1,1) <--> uf(0.5,2.5)
!     | * | F | F | F |   va(1,1) <--> vf(2.5,0.5)
!      -------va------    za(1,1) <--> zf(2.5,2.5)
!
!   ^ x2
!   |  --------va(1,2)---------
!   | |            |           |   Example of atmospheric and fire grid with
!   | |            |           |   ir=jr=1.
!   | |          za,zf         |   Winds are given at the midpoints of the sides of the atmosphere grid,
!   | ua(1,1)----uf,vf-----ua(2,1) interpolated to midpoints of the cells of the (the same) fire grid
!   | |           (1,1)        |   ua(1,1) <--> uf(0.5,1)
!   | |            |           |   va(1,1) <--> vf(1,0.5)
!   | |            |           |   za(1,1) <--> zf(1,1)
!   |  --------va(1,1)---------
!   |--------------------> x1
!
! Meshes are aligned by the lower left cell of the domain. Then in the above figure
! u = node with the ua component of the wind at (ids,jds), midpoint of side
! v = node with the va component of the wind at (ids,jds), midpoint of side
! * = fire grid node at (ifds,jfds)
! z = node with height, midpoint of cell
!
! ua(ids,jds)=uf(ifds-0.5,jfds+jr*0.5-0.5)         = uf(ifds-0.5,jfds+(jr-1)*0.5)
! va(ids,jds)=vf(ifds+ir*0.5-0.5,jfds-0.5)         = vf(ifds+(ir-1)*0.5,jfds-0.5)
! za(ids,jds)=zf(ifds+ir*0.5-0.5,jfds+jr*0.5-0.5)  = zf(ifds+(ir-1)*0.5,jfds+(jr-1)*0.5)

    ! ifts1=max(snode(ifts,ifps,-1),ifds) ! go 1 beyond patch boundary but not at domain boundary
    ! ifte1=min(snode(ifte,ifpe,+1),ifde)
    ! jfts1=max(snode(jfts,jfps,-1),jfds)
    ! jfte1=min(snode(jfte,jfpe,+1),jfde)

    call interpolate_2d(  &
        TDIMS,                  & ! memory dims atm grid tile
        itsou,iteou,jtsou,jteou,& ! where set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts,ifte,jfts,jfte,& ! dimensions on the fire grid to interpolate to
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds-0.5,jfds+(jr-1)*0.5, & ! line up by lower left corner of domain
        ua,                     & ! in atm grid
        uf)                      ! out fire grid

    call interpolate_2d(  &
        TDIMS,                  & ! memory dims atm grid tile
        itsov,iteov,jtsov,jteov,& ! where set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts,ifte,jfts,jfte,& ! dimensions on the fire grid to interpolate to
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds+(ir-1)*0.5,jfds-0.5, & ! line up by lower left corner of domain
        va,                     & ! in atm grid
        vf)                      ! out fire grid

! DME here code to extrapolate mid-flame height velocity -> fire_lsm_zcoupling = .true.
if (config_flags%fire_lsm_zcoupling) then
  do j = jfts,jfte
    do i = ifts,ifte
      uf_temp=uf(i,j)
      vf_temp=vf(i,j)
      wsf=max(sqrt(uf_temp**2.+vf_temp**2.),0.1)
      z0fc=z0f(i,j)
      ust_d=wsf*vk_kappa/log(config_flags%fire_lsm_zcoupling_ref/z0fc)
      wsf1=(ust_d/vk_kappa)*log((fire_wind_height+z0fc)/z0fc)
      uf(i,j)=wsf1*uf_temp/wsf
      vf(i,j)=wsf1*vf_temp/wsf
    enddo
  enddo
endif


call print_2d_stats_vec(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,uf,vf,'fire wind (m/s)')
! call print_2d_stats_vec(ifts1,ifte1,jfts1,jfte1,ifms,ifme,jfms,jfme,uf,vf,'fire wind extended')
#ifdef DEBUG_OUT
        call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,uf,'uf1',id)
        call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,vf,'vf1',id)
        ! call write_array_m(ifts1,ifte1,jfts1,jfte1,ifms,ifme,jfms,jfme,uf,'uf1',id)
        ! call write_array_m(ifts1,ifte1,jfts1,jfte1,ifms,ifme,jfms,jfme,vf,'vf1',id)
#endif
return

end subroutine interpolate_atm2fire

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

