
! SFIRE - Spread fire model in WRF-Fire
!
! This module is the entry point from WRF ARW to the wildland 
! fire module. The call to fire_driver advances the fire module by 
! one timestep. The fire module inputs the wind and outputs 
! temperature and humidity tendencies. The fire module also inputs a 
! number of constant arrays (fuel data, topography). Additional 
! arguments are model state (for data assimilation) and constant arrays 
! the model gives to WRF for safekeeping because it is not allowed 
! to save anything.
!
! Contributions to this wildland fire module have come from individuals at
! NCAR, the U.S.D.A. Forest Service, the Australian Bureau of Meteorology, 
! and the University of Colorado at Denver. 
!

module module_fr_fire_driver
! use this module for standalone call, you only need to provide some mock-up wrf modules  

use module_fr_fire_model
use module_fr_fire_phys, only : fire_params , init_fuel_cats
use module_fr_fire_util
use module_fr_fire_core, only: ignition_line_type
use module_fr_fire_atm, only: add_fire_tracer_emissions 

USE state_mod, only: domain
USE namelist_mod, only: namelist_t
use wrf_atmosphere_mod, only: g ! gravitation acceleration

implicit none


private
public fire_driver_em,use_atm_vars

logical:: use_atm_vars=.true.   !  interpolate wind from atm mesh and average output fluxes back

logical:: fmoist_run, fmoist_interp, fire_run  ! which kind of model to run overall

contains

#define DEBUG_OUT
subroutine fire_driver_em ( grid , config_flags                    & 
            ,fire_ifun_start,fire_ifun_end,tsteps                   &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,rho,z_at_w,dz8w           &  
            )

!*** purpose: driver from grid structure

! Driver layer modules
#ifdef DM_PARALLEL
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm , ONLY : halo_fire_fuel_sub, halo_fire_tign_sub, halo_fire_wind_f_sub, &
halo_fire_wind_a_sub, halo_fire_ph_sub, halo_fire_zsf_sub, halo_fire_longlat_sub, &
halo_fire_phb_sub, halo_fire_z0_sub, halo_fire_lfn_sub, halo_fire_mfg_sub, &
halo_fire_mag_sub
#endif

    implicit none
!*** arguments
    TYPE(domain) , TARGET :: grid                             ! state 
    TYPE (namelist_t) , INTENT(IN)  :: config_flags ! namelist
    integer, intent(in)::     fire_ifun_start,fire_ifun_end,tsteps ! driver cycle controls
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde,                              &
             ims,ime, kms,kme, jms,jme,                              &
             ips,ipe, kps,kpe, jps,jpe,                              &
             ifds,ifde, jfds,jfde,                                   &
             ifms,ifme, jfms,jfme,                                   &
             ifps,ifpe, jfps,jfpe 
    real,dimension(ims:ime, kms:kme, jms:jme),intent(in), optional::rho,z_at_w,dz8w 

!*** local
    INTEGER:: fire_num_ignitions
    integer, parameter::fire_max_ignitions=5
    TYPE(ignition_line_type), DIMENSION(fire_max_ignitions):: ignition_line
    integer::fire_ifun,ir,jr,istep,itimestep
    logical::need_lfn_update,restart
    real, dimension(ifms:ifme, jfms:jfme)::lfn_out  
    real:: corner_ll,corner_ul,corner_ur,corner_lr
    character(len=128)msg
    type(fire_params)::fp
    real:: time_start, dt
    real:: moisture_time
    logical:: run_advance_moisture,run_fuel_moisture, moisture_initializing
    real::    dt_moisture



!*** executable

! populate our structures from wrf

    ! pointers to be passed to fire rate of spread formulas
    fp%vx => grid%uf         ! W-E winds used in fire module
    fp%vy => grid%vf         ! S-N winds used in fire module
    fp%zsf => grid%zsf       ! terrain height 
    fp%dzdxf => grid%dzdxf   ! terrain grad 
    fp%dzdyf => grid%dzdyf   ! terrain grad 
    fp%bbb => grid%bbb       ! a rate of spread formula coeff 
    fp%betafl => grid%betafl ! a rate of spread formula variable 
    fp%phiwc => grid%phiwc   ! a rate of spread formula coeff 
    fp%r_0 => grid%r_0       ! a rate of spread formula variable
    fp%fgip => grid%fgip     ! a rate of spread formula coeff 
    fp%ischap => grid%ischap ! a rate of spread formula switch
    fp%iboros => grid%iboros ! Ib divided by ROS
    fp%fmc_g => grid%fmc_g   ! fuel moisture, ground
            
    ! get ignition data 
    call fire_ignition_convert (config_flags,fire_max_ignitions,grid%fire_ignition_longlat, &
        ignition_line,fire_num_ignitions)

    ! copy configuration flags to fire internal structures
    call set_flags(config_flags)

    ! refinement r
    ir=grid%sr_x ! refinement ratio
    jr=grid%sr_y
    itimestep=grid%itimestep
    restart=config_flags%restart

    ! time - assume dt does not change
    dt = grid%dt
    time_start = itimestep * dt

    ! **** moisture model

    ! decide what to run - moisture, interpolation, or fire model itself
    fmoist_run    = config_flags%fmoist_run
    fmoist_interp = config_flags%fmoist_interp 
    if(fire_fmc_read.ne.0.and.fmoist_run)call crash('fmoist_run=T requires fire_fmc_read=0')
    fire_run = .not. config_flags%fmoist_only

    !decide what to run
    moisture_time = time_start
    run_advance_moisture = .false. ! default
    run_fuel_moisture = .false. ! default
    moisture_initializing = fire_ifun_start < 3
    
    
    
    if(fmoist_run)then
        if(moisture_initializing)then
            if(fire_ifun_end>2)call crash('initialization must be run separately')
            grid%fmoist_lasttime=moisture_time ! initialize the last time the model has run to start of run
            grid%fmoist_nexttime=moisture_time 
            call message('moisture initialization')
            run_advance_moisture = .true.
        else ! regular timestep
            if(config_flags%fmoist_freq > 0)then  ! regular timestep. go by multiples?
                if(mod(grid%itimestep,config_flags%fmoist_freq) .eq. 0)then
                    write(msg,'(a,i10,a,i10)')'moisture model runs because timestep ',grid%itimestep,' is a multiple of ', &
                        config_flags%fmoist_freq
                    call message(msg)
                    run_advance_moisture = .true.
                endif
            else
                if(.not. moisture_time  < grid%fmoist_nexttime) then ! no, by time interval
                    write(msg,'(a,f12.2,a)')'moisture model runs because time ',grid%fmoist_nexttime,'s has arrived'
                    call message(msg)
                    run_advance_moisture = .true.
                endif
            endif
            if(run_advance_moisture)then ! decide on timing
                dt_moisture  = moisture_time - grid%fmoist_lasttime  ! Time since moisture model run the last time. Should be long.
                grid%fmoist_lasttime = moisture_time
                if(config_flags%fmoist_freq > 0)then
                    write(msg,'(a,f12.2,a,i10,a)')'moisture time step is ',dt_moisture,'s running every ', &
                        config_flags%fmoist_freq,' steps'
                    call message(msg)
                else
                    grid%fmoist_nexttime = moisture_time + config_flags%fmoist_dt
                    write(msg,'(a,f12.2,a,f12.2,a)')'moisture time step is ',dt_moisture,'s next run at ',grid%fmoist_nexttime,'s'
                    call message(msg)
                endif
                if(fmoist_interp)then
                    call message('moisture interpolation to fuels will run because moisture model does')
                    run_fuel_moisture=.true.
                endif
            endif
        endif
    elseif(itimestep.eq.1.and.fmoist_interp)then
            call message('initializing, moisture interpolation to fuels will run from input data')
            run_fuel_moisture=.true.
    endif

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,'(a,i1,a,i1,a,i4)') &
       'fire_driver_em: ifun from ',fire_ifun_start,' to ',fire_ifun_end,' test steps',tsteps
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
    call message(msg)

    do istep=0,tsteps ! istep >0 is for testing only, exit after the first call
      itimestep = grid%itimestep + istep ! in the first call, do fire_test_steps steps of the fire model

      need_lfn_update=.false.
      do fire_ifun=fire_ifun_start,fire_ifun_end

        ! 1 = run pass 1: interpolate height to zsf=terrain, moisture_model initialize
        ! 2 = initialize run pass 2: set fuel data, terrain gradient
        ! 3 = initialize timestep: interpolate winds, check for ignition, time step on moisture model
        ! 4 = do one timestep 
        ! 5 = copy timestep output to input
        ! 6 = compute output fluxes

#ifdef DM_PARALLEL

       if(fire_run)then
        if(need_lfn_update)then
!           halo exchange on lfn width 3
#include "HALO_FIRE_LFN.inc"
        endif

        if(fire_ifun.eq.1)then
!       halo exchange on topography
#include "HALO_FIRE_LONGLAT.inc"
!!            if(fire_topo_from_atm.eq.1)then
!!#include "HALO_FIRE_HT.inc"
!!            endif 
! base geopotential and roughness
#include "HALO_FIRE_PHB.inc"

#include "HALO_FIRE_Z0.inc"

        elseif(fire_ifun.eq.2)then
!           halo exchange on zsf width 2
#include "HALO_FIRE_ZSF.inc"

        elseif(fire_ifun.eq.3)then
!           halo exchange on atm winds and geopotential, width 1 for interpolation
#include "HALO_FIRE_WIND_A.inc"
#include "HALO_FIRE_PH.inc"

        elseif(fire_ifun.eq.4)then
!           halo exchange on fire winds width 2 for a 2-step RK method
#include "HALO_FIRE_WIND_F.inc"

            if(run_fuel_moisture)then
            ! have interpolated to the fire grid
#include "HALO_FIRE_MFG.inc"
            endif

        elseif(fire_ifun.eq.6)then
!           computing fuel_left needs ignition time from neighbors
#include "HALO_FIRE_TIGN.inc"

        endif
       endif
#endif

        
        ! need domain by 1 smaller, in last row.col winds are not set properly
        call fire_driver_phys ( &
            fire_ifun,need_lfn_update,                  &
            ids,ide-1, kds,kde, jds,jde-1,                          &
            ims,ime, kms,kme, jms,jme,                          &
            ips,min(ipe,ide-1), kps,kpe, jps,min(jpe,jde-1),                          & 
            ifds,ifde-ir, jfds,jfde-jr,                    &
            ifms,ifme, jfms,jfme,                    &
            ifps,min(ifpe,ifde-ir), jfps,min(jfpe,jfde-jr),      &
            ir,jr,                                      & ! atm/fire grid ratio
            grid%num_tiles,                             & ! atm grid tiling
            grid%i_start,min(grid%i_end,ide-1),                    &
            grid%j_start,min(grid%j_end,jde-1),                    &
            time_start,                                 &
            itimestep,restart,config_flags%fire_fuel_read,config_flags%fire_fuel_cat, &  ! in scalars
            grid%dt,grid%dx,grid%dy,                    &
            grid%u_frame,grid%v_frame,                  &
            grid%unit_fxlong,grid%unit_fxlat,                           & ! coordinates of grid center
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt, &
            config_flags%fire_wind_height,              &  ! height of wind input to fire spread formula
            fire_num_ignitions,                                & 
            grid%fire_ignition_longlat,      &
            ignition_line,              &
            grid%u_2,grid%v_2,           &          ! atm arrays in
            grid%ph_2,grid%phb,               & ! geopotential
            grid%z0,                        & ! roughness height
            grid%ht,                        &                         ! terrain height
            grid%xlong,grid%xlat,                         & ! coordinates of atm grid centers, for ignition location
            grid%lfn, &
            grid%lfn_hist, &  ! PAJ: to initialize fire perimeter from obs
            config_flags%fire_is_real_perim,  & ! PAJ: use obs fire perimeter. 
            grid%lfn_0,grid%lfn_1,grid%lfn_2,grid%lfn_s0,grid%lfn_s1,grid%lfn_s2,grid%lfn_s3,grid%flame_length,grid%ros_front, & 
            grid%tign_g,grid%fuel_frac,          & ! state arrays, fire grid
            grid%fire_area,                               & ! redundant, for display, fire grid
            grid%burnt_area_dt,                               & ! redundant, for display
            lfn_out,                                      & ! work - one timestep    
            grid%avg_fuel_frac,                           & ! out redundant arrays, atm grid
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx, & ! out redundant arrays, atm grid
            grid%grnhfx_fu,grid%grnqfx_fu, & ! out redundant arrays, atm grid
            grid%uah,grid%vah,                          &
            grid%fgrnhfx,grid%fgrnqfx,grid%fcanhfx,grid%fcanqfx, & ! out redundant arrays, atm grid
            grid%ros,                                   & ! rate of spread
            grid%fxlong,grid%fxlat,grid%fz0,                           &       
            grid%nfuel_cat,                               & ! input, or internal for safekeeping
            grid%fuel_time,                      & 
            config_flags%nfmc,         & ! moisture model variables start
            run_advance_moisture,run_fuel_moisture,dt_moisture,     &    ! moisture model control
            config_flags%fmep_decay_tlag,                               & ! moisture extended model assim. diffs decay time lag
            grid%rainc, grid%rainnc,                       & ! accumulated rain from different sources
            grid%t2, grid%q2, grid%psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
            grid%rain_old,                   & ! previous value of accumulated rain
            grid%t2_old, grid%q2_old, grid%psfc_old,   & ! previous values of the atmospheric state at surface
            grid%rh_fire,                    & ! relative humidity, diagnostics
            grid%fmc_gc,                      & ! fuel moisture fields updated, by class, assumed set to something reasonable
            grid%fmep,                      & ! fuel moisture extended model parameters
            grid%fmc_equi,                      & ! fuel moisture fields updated, by class, equilibrium diagnostic 
            grid%fmc_lag,                      & ! fuel moisture fields updated, by class, tendency diagnostic
            fp%fmc_g , &                             !  write-only alias. need to exit before using fp again
            fp, &
            grid, & ! DME added for halo update
            ids,ide,jds,jde,kds,kde, & 
            ims,ime,jms,jme,kms,kme, & 
            ips,ipe,jps,jpe,kps,kpe, &
            config_flags & 
        )

   
#ifdef DM_PARALLEL
          if(fire_run)then
            if(fire_ifun.eq.2)then
!               halo exchange on all fuel data width 2
#include "HALO_FIRE_FUEL.inc"
!               fire state was initialized
                call message('halo exchange on lfn width 2')
#include "HALO_FIRE_LFN.inc"
          endif
          if(run_fuel_moisture)then
            if(fire_ifun.eq.3)then
                     ! prepare for interpolation to the fire grid
#include "HALO_FIRE_MAG.inc"
                endif
            endif
            endif
#endif

! DME update tracer emissions
     if(fire_ifun.eq.6 .AND. config_flags%tracer_opt.eq.3)then
       call add_fire_tracer_emissions(config_flags%tracer_opt,grid%dt,grid%dx,grid%dy, &
                 ifms,ifme,jfms,jfme,    &
                 ifps,ifpe,jfps,jfpe,    &
                 ids,ide,kds,kde,jds,jde,          &
                 ims,ime,kms,kme,jms,jme,          &
                 ips,ipe,kps,kpe,jps,jpe,          &
                 rho,dz8w,                         &
                 grid%burnt_area_dt,grid%fgip, &
                 grid%tracer,config_flags%fire_tracer_smoke)
     endif
! DME
      enddo
    enddo
    if(tsteps>0)call crash('fire_driver_em: test run of uncoupled fire model completed')

end subroutine fire_driver_em

!
!*******************
!

! module_fr_fire_driver%%fire_driver
subroutine fire_driver_phys (ifun,need_lfn_update,    &
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe, kps,kpe, jps,jpe,                    &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifps, ifpe, jfps, jfpe,                       & ! fire patch in - will use smaller
    ir,jr,                                        & ! atm/fire grid ratio
    num_tiles,i_start,i_end,j_start,j_end,        & ! atm grid tiling
    time_start,                                   & ! start of the timestep
    itimestep,restart,ifuelread,nfuel_cat0,dt,dx,dy,      & ! in scalars
    u_frame,v_frame,                              &
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt,    &
    fire_wind_height,                             & ! for vertical wind interpolation
    num_ignitions,                                & 
    ignition_longlat,                             &
    ignition_line,                                &
    u,v,                                       & ! in arrays, atm grid
    ph,phb,                                       &
    z0,zs,                                        & 
    xlong,xlat,                                   &
    lfn,                                      &
    lfn_hist,                                      & ! PAJ to init perimeter from obs
    fire_is_real_perim,                            & ! PAJ to init perimeter from obs
    lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front, & 
    tign,fuel_frac,                           & ! state arrays, fire grid
    fire_area,                                    & ! redundant state, fire grid
    burnt_area_dt,                                    & ! redundant state, fire grid
    lfn_out,                                      & ! out level set function    
    avg_fuel_frac,                                &
    grnhfx,grnqfx,canhfx,canqfx,                  & ! out redundant arrays, atm grid  
    grnhfx_fu,grnqfx_fu,                 & ! out redundant arrays, atm grid
    uah,vah,                                      & ! out atm grid
    fgrnhfx,fgrnqfx,fcanhfx,fcanqfx,              & ! out redundant arrays, fire grid
    ros,                                          &
    fxlong,fxlat,fz0,                                 & 
    nfuel_cat,                                    & ! in array, data, fire grid, or constant internal
    fuel_time,                                & ! save constant internal data, fire grid
    nfmc,                                     & ! number of fuel moisture classes
    run_advance_moisture,run_fuel_moisture,dt_moisture,& ! moisture model control
    fmep_decay_tlag,                              & ! moist. extended model assim. diffs time lag
    rainc,rainnc,              & ! accumulated rain from different sources
    t2, q2, psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
    rain_old,                   & ! previous value of accumulated rain
    t2_old, q2_old, psfc_old,   & ! previous values of the atmospheric state at surface
    rh_fire,                    & ! relative humidity, diagnostics
    fmc_gc,                    &  ! fuel moisture fields updated, by class, assumed set to something reasonable
    fmep,                       &  ! fuel moisture extended model parameters
    fmc_equi,                    &  ! fuel moisture fields updated, by class equilibrium diagnostic
    fmc_lag,                    &  ! fuel moisture fields updated, by class tendency diagnostic
    fmc_g, &                          ! fuel moisture, alias of fp%fmc_g
    fp,                                           & ! fire params
    grid,                                         & 
    ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, &
    ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, &
    ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu, &
    config_flags & 
    )

#ifdef DM_PARALLEL
USE module_dm, only:wrf_dm_maxval
#endif
USE state_mod, only: domain

implicit none

    type(domain) , target :: grid                             ! state 

!*** arguments

integer, intent(in)::ifun,                        &
    ids,ide, kds,kde, jds,jde,                    & ! atm domain bounds
    ims,ime, kms,kme, jms,jme,                    & ! atm memory bounds 
    ips,ipe, kps,kpe, jps,jpe,                    & ! atm patch bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
    ir,jr,                                        & ! atm/fire grid refinement ratio
    nfmc,                                     & ! number of fuel moisture classes
    itimestep,                                    & ! number of this timestep
    ifuelread,                                    & ! how to initialize nfuel_cat:
                                                       ! -1=not at all, done outside 
                                                       ! 0=from nfuel_cat0
                                                       ! 1=from altitude
                                                       ! 2=from file
    nfuel_cat0,                                   & ! fuel category to initialize everything to
    num_tiles                                       ! number of tiles

integer, intent(in)::ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, & 
                     ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, & 
                     ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu   

type (namelist_t), intent(in)          :: config_flags

logical, intent(in)::restart
    

logical, intent(out)::need_lfn_update

integer,dimension(num_tiles),intent(in) :: i_start,i_end,j_start,j_end  ! atm grid tiling

real, intent(in):: &
    time_start,                                   & ! start of this time step from the beginning of the simulation
    dt,                                           & ! time step
    dx,dy,                                        & ! atm grid step
    u_frame,v_frame,                              & ! velocity offset
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_crwn_hgt,                                & ! lowest height crown fire heat is released (m)
    fire_ext_grnd,                                & ! extinction depth of surface fire heat flux (m)
    fire_ext_crwn,                                & ! wind height for vertical interploation to fire spread
    fire_wind_height 


integer, intent(in):: num_ignitions                 ! number of ignitions, can be 0
integer, intent(in):: ignition_longlat       ! if 1 ignition give as long/lat, otherwise as m from lower left corner
TYPE (ignition_line_type), DIMENSION(num_ignitions), intent(out):: ignition_line

real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::u,v, & ! wind velocity (m/s) (staggered atm grid) 
                              ph, phb                      ! geopotential (w-points atm grid)
real,intent(in),dimension(ims:ime, jms:jme)::   z0, &    ! roughness height
                                                zs       ! terrain height  
real,intent(out),dimension(ims:ime,jms:jme)::&
    uah,                                           & ! atm wind at fire_wind_height, diagnostics
    vah                                              ! atm wind at fire_wind_height, diagnostics

real, dimension(ims:ime, jms:jme), intent(in)::xlong, xlat ! in because computed in driver_wrf
    
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: &
    nfuel_cat                                       ! fuel data; can be also set inside (cell based, fire grid)

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn,tign,fuel_frac,                        &     ! state: level function, ign time, fuel left
    lfn_hist,                                  &     ! PAJ: to init obs. fira perimeter
    lfn_out                                    ! fire wind velocities

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front  ! level set function and reinitialization steps

real, intent(out), dimension(ifms:ifme, jfms:jfme)::  &
    fire_area                                        ! fraction of each cell burning

real, intent(out), dimension(ifms:ifme, jfms:jfme)::  &
    burnt_area_dt                                        
real, intent(out), dimension(ims:ime, jms:jme)::  &
   grnhfx_fu,grnqfx_fu                                        

real, intent(out), dimension(ims:ime, jms:jme):: &  ! redundant arrays, for display purposes only (atm grid)
    avg_fuel_frac,                               &  ! average fuel fraction
    grnhfx,                                      &  ! heat flux from surface fire (W/m^2) 
    grnqfx,                                      &  ! moisture flux from surface fire (W/m^2) 
    canhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    canqfx                                         ! moisture flux from crown fire (W/m^2) 

real, intent(out), dimension(ifms:ifme, jfms:jfme):: &  ! redundant arrays, for display only, fire grid
    fgrnhfx,                                      &  ! heat flux from surface fire (W/m^2) 
    fgrnqfx,                                      &  ! moisture flux from surface fire (W/m^2) 
    fcanhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    fcanqfx,                                      &   ! moisture flux from crown fire (W/m^2) 
    ros                                             ! fire rate of spread (m/s)
    
! moisture model arguments
logical, intent(in)::run_advance_moisture,run_fuel_moisture
real, intent(in)::dt_moisture
real, intent(in)::fmep_decay_tlag
real, intent(in), dimension(ims:ime,jms:jme):: t2, q2, psfc, rainc, rainnc
real, intent(inout), dimension(ims:ime,jms:jme):: t2_old, q2_old, psfc_old, rain_old 
real, intent(out),dimension(ims:ime,jms:jme):: rh_fire
real, intent(inout), dimension(ims:ime,nfmc,jms:jme):: fmc_gc
real, intent(inout), dimension(ims:ime,2,jms:jme):: fmep
real, intent(out), dimension(ims:ime,nfmc,jms:jme):: fmc_equi,fmc_lag
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: fmc_g



!  ***** data (constant in time) *****

real, dimension(ifms:ifme, jfms:jfme), intent(in) :: fxlong, fxlat, fz0 ! fire mesh coordinates and z0
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   ! fire params arrays

type(fire_params),intent(inout)::fp
    
!*** local
real :: dxf,dyf,latm, s
integer :: its,ite,jts,jte,kts,kte, &            ! tile
    ij,i,j,k,id,pid,ipe1,jpe1,ite1,jte1, &
    ifts,ifte,jfts,jfte                          ! fire tile
character(len=128)::msg
character(len=3)::kk
integer::ignitions_done_tile(num_tiles),ignited_tile(num_ignitions,num_tiles)
integer::ignitions_done,ignited_patch(num_ignitions),idex,jdex

    ! PAJ:
  logical :: fire_is_real_perim

!*** executable

! fire mesh step
dxf=dx/ir
dyf=dy/jr



!$OMP CRITICAL(FIRE_DRIVER_CRIT)
write(msg,'(a,2f15.6)')'atmosphere mesh step:',dx,dy
call message(msg)
write(msg,'(a,2f15.6)')'fire mesh step:      ',dxf,dyf
call message(msg)
write(msg,7001)'atm domain      ','ids',ids,ide,jds,jde
call message(msg)                    
write(msg,7001)'atm memory      ','ims',ims,ime,jms,jme
call message(msg)                    
write(msg,7001)'atm patch       ','ips',ips,ipe,jps,jpe
call message(msg)                    
write(msg,7001)'fire domain     ','ifds',ifds,ifde,jfds,jfde
call message(msg)                    
write(msg,7001)'fire memory     ','ifms',ifms,ifme,jfms,jfme
call message(msg)                    
write(msg,7001)'fire patch      ','ifps',ifps,ifpe,jfps,jfpe
call message(msg)                    
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)

! check mesh dimensions
call check_fmesh(ids,ide,ifds,ifde,ir,'id')           ! check if atm and fire grids line up
call check_fmesh(jds,jde,jfds,jfde,jr,'jd')
call check_fmesh(ips,ipe,ifps,ifpe,ir,'ip')
call check_fmesh(jps,jpe,jfps,jfpe,jr,'jp')
call check_mesh_2dim(ips,ipe,jps,jpe,ims,ime,jms,jme)        ! check if atm patch fits in atm array
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme) ! check if fire patch fits in fire array
call check_mesh_2dim(ips,ipe,jps,jpe,ids,ide,jds,jde)        ! check if atm patch fits in atm domain
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifds,ifde,jfds,jfde) ! check if fire patch fits in fire domain

!$OMP SINGLE
! init rest of fuel tables with derived constants
if(ifun.eq.1) then
   ! properties of fuel categories and moisture classes from namelist.fire
   call init_fuel_cats(fmoist_run .or. fmoist_interp) ! common for all threads
endif
!$OMP END SINGLE


pid=0
if(fire_print_file.gt.0)then
    if(itimestep.le.fire_print_file.or.mod(itimestep,fire_print_file).eq.0)pid=itimestep ! print 1-fire_print_file then every fire_print_file-th
endif

if(ifun.eq.3)then
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,1,0,0,u,'u')
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,0,0,1,v,'v')
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,0,1,0,ph,'ph')
endif

! fake atm tile bounds
kts=kps
kte=kpe

! staggered atm patch bounds
ipe1=ifval(ipe.eq.ide,ipe+1,ipe)
jpe1=ifval(jpe.eq.jde,jpe+1,jpe)

! set up fire tiles & interpolate to fire grid
!$OMP PARALLEL DO PRIVATE(ij,its,ite,jts,jte,ite1,jte1,ifts,ifte,jfts,jfte,msg,id) &
!$OMP SCHEDULE(STATIC)
do ij=1,num_tiles

    id = ifval(pid.ne.0,pid+ij*10000,0) ! for print

    ignitions_done_tile(ij)=0

    ! set up tile bounds    
    its = i_start(ij)  ! start atmospheric tile in i
    ite = i_end(ij)    ! end atmospheric tile in i
    jts = j_start(ij)  ! start atmospheric tile in j
    jte = j_end(ij)    ! end atmospheric tile in j
    ifts= (its-ids)*ir+ifds       ! start fire tile in i
    ifte= (ite-ids+1)*ir+ifds-1   ! end fire tile in i
    jfts= (jts-jds)*jr+jfds       ! start fire tile in j
    jfte= (jte-jds+1)*jr+jfds-1   ! end fire tile in j
        
! staggered atm tile bounds
    ite1=ifval(ite.eq.ide,ite+1,ite)
    jte1=ifval(jte.eq.jde,jte+1,jte)

!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,'(a,i3,1x,a,i7,1x,a,i3)')'tile=',ij,' id=',id,' ifun=',ifun
    call message(msg)
    write(msg,7001)'atm tile   ','its',its,ite,jts,jte
    call message(msg)                   
    write(msg,7001)'fire tile  ','ifts',ifts,ifte,jfts,jfte
    call message(msg)                    
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)

    ! check the tiles
    call check_mesh_2dim(its,ite,jts,jte,ips,ipe,jps,jpe)                 ! check if atm tile fits in atm patch
    call check_mesh_2dim(ifts,ifte,jfts,jfte,ifps,ifpe,jfps,jfpe)         ! check if fire tile fits in fire patch
    call check_mesh_2dim(ifts-2,ifte+2,jfts-2,jfte+2,ifms,ifme,jfms,jfme)! check if fire node tile fits in memory


!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,'(a,i6,a,2(f15.6,a))')'time step',itimestep,' at',time_start,' duration',dt,'s'
    call message(msg)
    7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)
    write(msg,'(a,2i9)')'refinement ratio:',ir,jr
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)

    if(run_advance_moisture)then
      if(ifun.eq.3)then 
      
      ! one timestep of the moisture model
          call message('advance_moisture start')
          call advance_moisture(    &
              itimestep.eq.1,             & ! initialize?
              ims,ime,  jms,jme,          & ! memory dimensions
              its,ite,  jts,jte,          & ! tile dimensions
              nfmc,                       & ! number of moisture fields
              dt_moisture,                & ! moisture model time step
              fmep_decay_tlag,            & ! moisture extended model assim. diffs decay tlag
              rainc, rainnc,              & ! accumulated rain 
              t2, q2, psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
              rain_old,                   & ! previous value of accumulated rain
              t2_old, q2_old, psfc_old,   & ! previous values of the atmospheric state at surface
              rh_fire,                    & ! relative humidity, diagnostics
              fmc_gc,                     & ! fuel moisture fields updated, by class, assumed set to something reasonable
              fmep,                       & ! fuel moisture extended model parameters
              fmc_equi,                   &  ! fuel moisture fields updated, by class equilibrium diagnostic
              fmc_lag                    &  ! fuel moisture fields updated, by class tendency diagnostic
          )
          call message('advance_moisture end')
      endif
    endif
        
    if(fire_run)then

    if(ifun.eq.4)then

      ! interpolate and compute weighted average to get the fuel moisture
      !! print *,'ifun=4, run_fuel_moisture=',run_fuel_moisture
      if(run_fuel_moisture)then
        call message('fuel_moisture start')
        call fuel_moisture(                &
        id,                                     & ! for prints and maybe file names
        nfmc,                                &
        ids,ide, jds,jde,               & ! atm grid dimensions
        ims,ime, jms,jme,           &
        ips,ipe,jps,jpe,                &
        its,ite,jts,jte,                     &
        ifds, ifde, jfds, jfde,         & ! fire grid dimensions
        ifms, ifme, jfms, jfme,     &
        ifts,ifte,jfts,jfte,                 &
        ir,jr,                                   & ! atm/fire grid ratio
        nfuel_cat,                         & ! fuel data
        fmc_gc,                             & ! moisture contents by class on atmospheric grid
        fmc_g                                & ! weighted fuel moisture contents on fire grid
        )
        call message('fuel_moisture end')
      endif ! run_fuel_moisture`

     endif ! ifun

    endif ! fire_run

!   the following executes in any case

    if(fire_run)then
      call fire_model (id,ifun,restart,need_lfn_update,  &
        run_fuel_moisture,                      & ! if fuel moisture needs to be updated
        num_ignitions,                          & ! switches
        ifuelread,nfuel_cat0,                   & ! initialize fuel categories
        ifds,ifde,jfds,jfde,                    & ! fire domain dims
        ifms,ifme,jfms,jfme,                    & ! fire memory dims
        ifps,ifpe,jfps,jfpe,                    &
        ifts,ifte,jfts,jfte,                    & ! fire patch dims
        time_start,dt,                          & ! time and increment
        dxf,dyf,                                & ! fire mesh spacing
        ignition_line,                          & ! description of ignition lines
        ignitions_done_tile(ij),ignited_tile(1,ij),  &
        fxlong,fxlat,unit_fxlong,unit_fxlat,      & ! fire mesh coordinates
        lfn,                                     & ! state: level function
        lfn_hist,                                & ! PAJ: to init obs fire perimeter
        fire_is_real_perim,                      & ! PAJ: to init obs fire perimeter
        lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front,  & ! state
        lfn_out,tign,fuel_frac,                     & ! state: ign time, fuel left
        fire_area,                              & ! output: fraction of cell burning
        burnt_area_dt,                              & 
        fgrnhfx,fgrnqfx,                        & ! output: heat fluxes
        ros,                                    & ! output: rate of spread for display
        nfuel_cat,                              & ! fuel data per point 
        fuel_time,                              & ! save derived internal data
        fp,                                      & ! fire coefficients
        grid,            &  ! DME added for halo update
        ids_hu,ide_hu,jds_hu,jde_hu,kds_hu,kde_hu, & 
        ims_hu,ime_hu,jms_hu,jme_hu,kms_hu,kme_hu, & 
        ips_hu,ipe_hu,jps_hu,jpe_hu,kps_hu,kpe_hu  &
    )

  endif ! fire_run
enddo ! tiles
!$OMP END PARALLEL DO

#ifdef DEBUG_OUT
if(ifun.eq.1)then
    if(pid.ne.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'zs',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%zsf,'zsf',pid)
    endif
endif
#endif

if (ifun.eq.3)then
    ignitions_done=ignitions_done_tile(1) ! all should be the same
    do i=1,ignitions_done
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
        write(msg,'(2(a,i4,1x))')'fire_driver_phys: checking ignition ',i,' of ',ignitions_done
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
        call message(msg)
        ignited_patch(i)=0
        do ij=1,num_tiles
            ignited_patch(i)=ignited_patch(i)+ignited_tile(i,ij)
        enddo
#ifdef DM_PARALLEL
        call message('fire_driver_phys: checking ignitions, collect counts')
        call wrf_dm_maxval(ignited_patch(i),idex,jdex)
        call message('fire_driver_phys: collected')
#endif
        if(ignited_patch(i).eq.0)then
            call crash('fire_driver_phys: Ignition failed, no nodes ignited. Bad coordinates?')
        endif
    enddo

 call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,1,0,0,uah,'uah')
 call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,1,vah,'vah')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fp%vx,'uf')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fp%vy,'vf')
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ips,ipe1,jps,jpe,ims,ime,jms,jme,uah,'uah',pid)
        call write_array_m(ips,ipe,jps,jpe1,ims,ime,jms,jme,vah,'vah',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m3(ips,ipe1,kds,kde+1,jps,jpe,ims,ime,kms,kme,jms,jme,u,'u',pid)
        call write_array_m3(ips,ipe,kds,kde+1,jps,jpe1,ims,ime,kms,kme,jms,jme,v,'v',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vx,'uf',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vy,'vf',pid)
    endif
#endif
endif

if(ifun.eq.5)then
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,lfn,'lfn',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,tign,'tign',pid)
    endif
#endif
endif

if(ifun.eq.6)then
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fgrnhfx,'fgrnhfx')
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fgrnqfx,'fgrnqfx')
    call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grnhfx,'grnhfx')
    call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grnqfx,'grnqfx')
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fuel_frac,'fuel_frac',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnhfx,'fgrnhfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnqfx,'fgrnqfx',pid)
    endif
#endif
endif

end subroutine fire_driver_phys
!
!*******************
!

subroutine fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
    ignition_line,fire_num_ignitions)
    USE namelist_mod, only : namelist_t
    implicit none
! create ignition arrays from scalar flags
!*** arguments
    TYPE (namelist_t) , INTENT(IN)          :: config_flags
    integer, intent(in)::fire_max_ignitions
    TYPE (ignition_line_type), DIMENSION(fire_max_ignitions), intent(out):: ignition_line ! any values from input discarded 
    integer, intent(out)::fire_num_ignitions
    integer, intent(in)::fire_ignition_longlat
!*** local
    integer::i
!*** executable
    ! this is only until I figure out how to input arrays through the namelist...
    if(fire_max_ignitions.lt.5)call crash('fire_max_ignitions too small')

    if(fire_ignition_longlat .eq. 0)then
       ! use values from _x and _y variables
       ignition_line(1)%start_x=config_flags%fire_ignition_start_x1
       ignition_line(1)%start_y=config_flags%fire_ignition_start_y1
       ignition_line(1)%end_x=config_flags%fire_ignition_end_x1
       ignition_line(1)%end_y=config_flags%fire_ignition_end_y1
       ignition_line(2)%start_x=config_flags%fire_ignition_start_x2
       ignition_line(2)%start_y=config_flags%fire_ignition_start_y2
       ignition_line(2)%end_x=config_flags%fire_ignition_end_x2
       ignition_line(2)%end_y=config_flags%fire_ignition_end_y2
       ignition_line(3)%start_x=config_flags%fire_ignition_start_x3
       ignition_line(3)%start_y=config_flags%fire_ignition_start_y3
       ignition_line(3)%end_x=config_flags%fire_ignition_end_x3
       ignition_line(3)%end_y=config_flags%fire_ignition_end_y3
       ignition_line(4)%start_x=config_flags%fire_ignition_start_x4
       ignition_line(4)%start_y=config_flags%fire_ignition_start_y4
       ignition_line(4)%end_x=config_flags%fire_ignition_end_x4
       ignition_line(4)%end_y=config_flags%fire_ignition_end_y4
       ignition_line(5)%start_x=config_flags%fire_ignition_start_x5
       ignition_line(5)%start_y=config_flags%fire_ignition_start_y5
       ignition_line(5)%end_x=config_flags%fire_ignition_end_x5
       ignition_line(5)%end_y=config_flags%fire_ignition_end_y5
    endif
    if(fire_ignition_longlat .eq. 1)then
        ! use values from _long and _lat
       ignition_line(1)%start_x=config_flags%fire_ignition_start_lon1
       ignition_line(1)%start_y=config_flags%fire_ignition_start_lat1
       ignition_line(1)%end_x=config_flags%fire_ignition_end_lon1
       ignition_line(1)%end_y=config_flags%fire_ignition_end_lat1
       ignition_line(2)%start_x=config_flags%fire_ignition_start_lon2
       ignition_line(2)%start_y=config_flags%fire_ignition_start_lat2
       ignition_line(2)%end_x=config_flags%fire_ignition_end_lon2
       ignition_line(2)%end_y=config_flags%fire_ignition_end_lat2
       ignition_line(3)%start_x=config_flags%fire_ignition_start_lon3
       ignition_line(3)%start_y=config_flags%fire_ignition_start_lat3
       ignition_line(3)%end_x=config_flags%fire_ignition_end_lon3
       ignition_line(3)%end_y=config_flags%fire_ignition_end_lat3
       ignition_line(4)%start_x=config_flags%fire_ignition_start_lon4
       ignition_line(4)%start_y=config_flags%fire_ignition_start_lat4
       ignition_line(4)%end_x=config_flags%fire_ignition_end_lon4
       ignition_line(4)%end_y=config_flags%fire_ignition_end_lat4
       ignition_line(5)%start_x=config_flags%fire_ignition_start_lon5
       ignition_line(5)%start_y=config_flags%fire_ignition_start_lat5
       ignition_line(5)%end_x=config_flags%fire_ignition_end_lon5
       ignition_line(5)%end_y=config_flags%fire_ignition_end_lat5
    endif
    ! common to both cases
       ignition_line(1)%ros=config_flags%fire_ignition_ros1 
       ignition_line(1)%radius=config_flags%fire_ignition_radius1 
       ignition_line(1)%start_time=config_flags%fire_ignition_start_time1 
       ignition_line(1)%end_time=config_flags%fire_ignition_end_time1 
       ignition_line(2)%ros=config_flags%fire_ignition_ros2 
       ignition_line(2)%radius=config_flags%fire_ignition_radius2 
       ignition_line(2)%start_time=config_flags%fire_ignition_start_time2 
       ignition_line(2)%end_time=config_flags%fire_ignition_end_time2 
       ignition_line(3)%ros=config_flags%fire_ignition_ros3 
       ignition_line(3)%radius=config_flags%fire_ignition_radius3 
       ignition_line(3)%start_time=config_flags%fire_ignition_start_time3 
       ignition_line(3)%end_time=config_flags%fire_ignition_end_time3 
       ignition_line(4)%ros=config_flags%fire_ignition_ros4 
       ignition_line(4)%radius=config_flags%fire_ignition_radius4 
       ignition_line(4)%start_time=config_flags%fire_ignition_start_time4 
       ignition_line(4)%end_time=config_flags%fire_ignition_end_time4 
       ignition_line(5)%ros=config_flags%fire_ignition_ros5 
       ignition_line(5)%radius=config_flags%fire_ignition_radius5 
       ignition_line(5)%start_time=config_flags%fire_ignition_start_time5
       ignition_line(5)%end_time=config_flags%fire_ignition_end_time5

    ! 
        fire_num_ignitions=0      
        do i=1,min(5,config_flags%fire_num_ignitions)
            ! count the ignitions 
            if(ignition_line(i)%radius.gt.0.)fire_num_ignitions=i
            ! expand ignition data given as zero
            if(ignition_line(i)%end_x.eq.0.)ignition_line(i)%end_x=ignition_line(i)%start_x
            if(ignition_line(i)%end_y.eq.0.)ignition_line(i)%end_y=ignition_line(i)%start_y
            if(ignition_line(i)%end_time.eq.0.)ignition_line(i)%end_time=ignition_line(i)%start_time
        enddo

end subroutine fire_ignition_convert

!
!***
!

subroutine check_fmesh(ids,ide,ifds,ifde,ir,s)
!*** purpose: check if fire and atm meshes line up
implicit none
!*** arguments
integer, intent(in)::ids,ide,ifds,ifde,ir
character(len=*),intent(in)::s
!*** local
character(len=128)msg
!*** executable
if ((ide-ids+1)*ir.ne.(ifde-ifds+1))then
!$OMP CRITICAL(FIRE_DRIVER_CRIT)
    write(msg,1)s,ids,ide,ifds,ifde,ir
1   format('module_fr_fire_driver: incompatible bounds ',a,' atm ',i5,':',i5,' fire ',i5,':',i5,' ratio ',i3)    
!$OMP END CRITICAL(FIRE_DRIVER_CRIT)
    call crash(msg)
endif
end subroutine check_fmesh

!
!*****************************
!
subroutine set_flags(config_flags)
implicit none
TYPE (namelist_t) , INTENT(IN)          :: config_flags
! copy flags from wrf to module_fr_fire_util
! for instructions how to add a flag see the top of module_fr_fire_util.F


fire_print_msg          = config_flags%fire_print_msg
fire_print_file         = config_flags%fire_print_file
fuel_left_method        = config_flags%fire_fuel_left_method
fuel_left_irl           = config_flags%fire_fuel_left_irl
fuel_left_jrl           = config_flags%fire_fuel_left_jrl
fire_const_time         = config_flags%fire_const_time
fire_const_grnhfx       = config_flags%fire_const_grnhfx
fire_const_grnqfx       = config_flags%fire_const_grnqfx
fire_atm_feedback       = config_flags%fire_atm_feedback
boundary_guard          = config_flags%fire_boundary_guard
fire_grows_only         = config_flags%fire_grows_only
fire_upwinding          = config_flags%fire_upwinding
fire_upwind_split       = config_flags%fire_upwind_split 
fire_viscosity          = config_flags%fire_viscosity 
fire_lfn_ext_up         = config_flags%fire_lfn_ext_up 
fire_test_steps         = config_flags%fire_test_steps 
fire_advection          = config_flags%fire_advection
fire_lsm_reinit         = config_flags%fire_lsm_reinit
fire_lsm_reinit_iter    = config_flags%fire_lsm_reinit_iter 
fire_upwinding_reinit   = config_flags%fire_upwinding_reinit 
fire_lsm_band_ngp       = config_flags%fire_lsm_band_ngp
fire_lsm_zcoupling      = config_flags%fire_lsm_zcoupling
fire_lsm_zcoupling_ref  = config_flags%fire_lsm_zcoupling_ref 
fire_viscosity_bg       = config_flags%fire_viscosity_bg 
fire_viscosity_band     = config_flags%fire_viscosity_band 
fire_viscosity_ngp      = config_flags%fire_viscosity_ngp
fire_slope_factor       = config_flags%fire_slope_factor 
fire_fmc_read           = config_flags%fire_fmc_read




end subroutine set_flags

end module module_fr_fire_driver
