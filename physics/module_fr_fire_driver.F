
module module_fr_fire_driver

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

use module_fr_fire_model, only: Fire_model
use module_fr_fire_phys, only : fire_params , init_fuel_cats, &
                                Advance_moisture, Fuel_moisture, Set_fire_params, Set_nfuel_cat
use module_fr_fire_util, only: fire_atm_feedback, fire_lsm_zcoupling, fire_lsm_zcoupling_ref, &
                               ifval, fire_advection, fire_fmc_read, &
                               fuel_left_method, fuel_left_jrl, fuel_left_irl, fire_viscosity_ngp, &
                               fire_viscosity_bg, fire_viscosity_band, fire_viscosity, fire_upwinding_reinit, &
                               fire_upwinding, fire_upwind_split, fire_slope_factor, fire_lsm_reinit_iter, &
                               fire_lsm_band_ngp, fire_lfn_ext_up, fire_grows_only, boundary_guard, &
                               fire_lsm_reinit, fire_print_msg, &
                               Crash, Check_mesh_2dim, Message
use module_fr_fire_core, only: ignition_line_type, Init_no_fire

USE state_mod, only: state_fire_t
USE namelist_mod, only: namelist_t

implicit none

private

public fire_driver_em, init_fire_driver

logical:: fmoist_run
real:: dt

integer :: fire_num_ignitions
integer, parameter :: FIRE_MAX_IGNITIONS = 5
type (ignition_line_type), dimension (FIRE_MAX_IGNITIONS):: ignition_line

contains

subroutine init_fire_driver (grid, config_flags)

    implicit none

    TYPE(state_fire_t) , TARGET :: grid             ! state
    TYPE (namelist_t) , INTENT(IN)  :: config_flags ! namelist

    integer::itimestep
    type(fire_params)::fp
    real:: time_start
    real:: moisture_time


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
    call fire_ignition_convert (config_flags,ignition_line,fire_num_ignitions)

    ! copy configuration flags to fire internal structures
    call set_flags(config_flags)

    ! initialize moisture model
    itimestep=grid%itimestep

    ! time - assume dt does not change
    dt = grid%dt
    time_start = itimestep * dt

    ! decide what to run - moisture, interpolation, or fire model itself
    fmoist_run    = config_flags%fmoist_run
    moisture_time = time_start

    if(fmoist_run)then
        grid%fmoist_lasttime=moisture_time ! initialize the last time the model has run to start of run
        grid%fmoist_nexttime=moisture_time
        call message('moisture initialization')
    endif

!$OMP SINGLE
! init rest of fuel tables with derived constants
! properties of fuel categories and moisture classes from namelist.fire
call init_fuel_cats(fmoist_run) ! common for all threads
!$OMP END SINGLE

        ! initialize all arrays that the model will not change later

        call set_nfuel_cat( &
            grid%ifms,grid%ifme,grid%jfms,grid%jfme, &
            grid%ifts,grid%ifte,grid%jfts,grid%jfte, &
            config_flags%fire_fuel_read,config_flags%fire_fuel_cat, &
            fp%zsf,grid%nfuel_cat)            ! better not use the extrapolated zsf!!

        ! uses nfuel_cat to set the other fuel data arrays
        ! needs zsf on halo width 1 to compute the terrain gradient
        call set_fire_params(   &
            grid%ifds,grid%ifde,grid%jfds,grid%jfde, &
            grid%ifms,grid%ifme,grid%jfms,grid%jfme, &
            grid%ifts,grid%ifte,grid%jfts,grid%jfte, &
            grid%dx,grid%dy,config_flags%fire_fuel_cat,    &
            grid%nfuel_cat,grid%fuel_time, &
            fp)

        ! initialize model state to no fire
        call init_no_fire  ( &
            grid%ifds,grid%ifde,grid%jfds,grid%jfde, &
            grid%ifms,grid%ifme,grid%jfms,grid%jfme, &
            grid%ifts,grid%ifte,grid%jfts,grid%jfte, &
            grid%dx,grid%dy,time_start,    &
            grid%fuel_frac,grid%fire_area,grid%lfn,grid%tign_g)

end subroutine init_fire_driver

subroutine fire_driver_em ( grid , config_flags                    & 
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifts,ifte, jfts,jfte                                   &
            )

    implicit none

    type (state_fire_t), target :: grid
    type (namelist_t), intent (in) :: config_flags
    integer, intent(in) :: ifds, ifde, jfds, jfde, ifms, ifme, jfms, jfme, &
        ifts, ifte, jfts, jfte

    integer :: itimestep
    real, dimension(ifms:ifme, jfms:jfme)::lfn_out  
    character(len=128)msg
    type(fire_params)::fp
    real:: time_start
    real:: moisture_time
    logical:: run_advance_moisture
    real::    dt_moisture


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

    itimestep=grid%itimestep

    ! time - assume dt does not change
    time_start = itimestep * dt

    ! decide what to run
    if(fire_fmc_read.ne.0.and.fmoist_run)call crash('fmoist_run=T requires fire_fmc_read=0')

    moisture_time = time_start
    run_advance_moisture = .false. ! default
    
    if(fmoist_run)then
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
        endif
    endif

        ! need domain by 1 smaller, in last row.col winds are not set properly
        call fire_driver_phys ( &
            ifds,ifde, jfds,jfde,                                                         &
            ifms,ifme, jfms,jfme,                                                               &
            ifts,ifte, jfts,jfte,                                                         &
            time_start,                                                                         &
            itimestep, config_flags%fire_fuel_read, config_flags%fire_fuel_cat,        &  ! in scalars
            grid%dt, grid%dx, grid%dy,                                                          &
            grid%unit_fxlong, grid%unit_fxlat,                                                  & ! coordinates of grid center
            config_flags%fire_ext_grnd, config_flags%fire_ext_crwn, config_flags%fire_crwn_hgt, &
            config_flags%fire_wind_height,                                                      &  ! height of wind input to fire spread formula
            fire_num_ignitions,                                                                 & 
            ignition_line,                                                                      &
            grid%lfn,                                                                           &
            grid%lfn_hist,                                                                      &  ! PAJ: to initialize fire perimeter from obs
            config_flags%fire_is_real_perim,                                                    & ! PAJ: use obs fire perimeter. 
            grid%lfn_0, grid%lfn_1, grid%lfn_2,                                                 &
            grid%lfn_s0, grid%lfn_s1, grid%lfn_s2, grid%lfn_s3,                                 &
            grid%flame_length, grid%ros_front,                                                  & 
            grid%tign_g, grid%fuel_frac,                                                        & ! state arrays, fire grid
            grid%fire_area,                                                                     & ! redundant, for display, fire grid
            grid%burnt_area_dt,                                                                 & ! redundant, for display
            lfn_out,                                                                            & ! work - one timestep    
            grid%fgrnhfx, grid%fgrnqfx, grid%fcanhfx, grid%fcanqfx,                             & ! out redundant arrays, atm grid
            grid%ros,                                                                           & ! rate of spread
            grid%fxlong, grid%fxlat, grid%fz0,                                                  &       
            grid%nfuel_cat,                                                                     & ! input, or internal for safekeeping
            grid%fuel_time,                                                                     & 
            config_flags%nfmc,                                                                  & ! moisture model variables start
            run_advance_moisture, dt_moisture,                                                  &    ! moisture model control
            config_flags%fmep_decay_tlag,                                                       & ! moisture extended model assim. diffs decay time lag
            grid%fire_rain,                                                                     & ! accumulated rain from different sources
            grid%fire_t2, grid%fire_q2, grid%fire_psfc,                                         & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
            grid%fire_rain_old,                                                                 & ! previous value of accumulated rain
            grid%fire_t2_old, grid%fire_q2_old, grid%fire_psfc_old,                             & ! previous values of the atmospheric state at surface
            grid%fire_rh_fire,                                                                  & ! relative humidity, diagnostics
            grid%fmc_gc,                                                                        & ! fuel moisture fields updated, by class, assumed set to something reasonable
            grid%fmep,                                                                          & ! fuel moisture extended model parameters
            grid%fmc_equi,                                                                      & ! fuel moisture fields updated, by class, equilibrium diagnostic 
            grid%fmc_lag,                                                                       & ! fuel moisture fields updated, by class, tendency diagnostic
            fp%fmc_g ,                                                                          &                             !  write-only alias. need to exit before using fp again
            fp                                                                                  &
        )

end subroutine fire_driver_em

subroutine fire_driver_phys ( &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifts, ifte, jfts, jfte,                       &
    time_start,                                   & ! start of the timestep
    itimestep,ifuelread,nfuel_cat0,               &
    dt,dx,dy,                                     & ! in scalars
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt,    &
    fire_wind_height,                             & ! for vertical wind interpolation
    num_ignitions,                                & 
    ignition_line,                                &
    lfn,                                          &
    lfn_hist,                                     & ! PAJ to init perimeter from obs
    fire_is_real_perim,                           & ! PAJ to init perimeter from obs
    lfn_0,lfn_1,lfn_2,                            &
    lfn_s0,lfn_s1,lfn_s2,lfn_s3,                  &
    flame_length,ros_front,                       & 
    tign,fuel_frac,                               & ! state arrays, fire grid
    fire_area,                                    & ! redundant state, fire grid
    burnt_area_dt,                                & ! redundant state, fire grid
    lfn_out,                                      & ! out level set function    
    fgrnhfx,fgrnqfx,fcanhfx,fcanqfx,              & ! out redundant arrays, fire grid
    ros,                                          &
    fxlong,fxlat,fz0,                             & 
    nfuel_cat,                                    & ! in array, data, fire grid, or constant internal
    fuel_time,                                    & ! save constant internal data, fire grid
    nfmc,                                         & ! number of fuel moisture classes
    run_advance_moisture,dt_moisture,             & ! moisture model control
    fmep_decay_tlag,                              & ! moist. extended model assim. diffs time lag
    fire_rain,                                    & ! accumulated rain from different sources
    fire_t2, fire_q2, fire_psfc,                  & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
    fire_rain_old,                                & ! previous value of accumulated rain
    fire_t2_old, fire_q2_old, fire_psfc_old,      & ! previous values of the atmospheric state at surface
    fire_rh_fire,                                 & ! relative humidity, diagnostics
    fmc_gc,                                       &  ! fuel moisture fields updated, by class, assumed set to something reasonable
    fmep,                                         &  ! fuel moisture extended model parameters
    fmc_equi,                                     &  ! fuel moisture fields updated, by class equilibrium diagnostic
    fmc_lag,                                      &  ! fuel moisture fields updated, by class tendency diagnostic
    fmc_g,                                        &                          ! fuel moisture, alias of fp%fmc_g
    fp                                            & ! fire params
    )

implicit none

integer, intent(in)::                             &
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifts, ifte, jfts, jfte,                       & ! fire tile bounds
    nfmc,                                         & ! number of fuel moisture classes
    itimestep,                                    & ! number of this timestep
    ifuelread,                                    & ! how to initialize nfuel_cat:
                                                    ! -1=not at all, done outside 
                                                    ! 0=from nfuel_cat0
                                                    ! 1=from altitude
                                                    ! 2=from file
    nfuel_cat0                                      ! fuel category to initialize everything to

real, intent(in):: &
    time_start,                                   & ! start of this time step from the beginning of the simulation
    dt,                                           & ! time step
    dx, dy,                                       & ! fire grid step (spacing)
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_crwn_hgt,                                & ! lowest height crown fire heat is released (m)
    fire_ext_grnd,                                & ! extinction depth of surface fire heat flux (m)
    fire_ext_crwn,                                & ! wind height for vertical interploation to fire spread
    fire_wind_height 

integer, intent(in):: num_ignitions          ! number of ignitions, can be 0
logical, intent(in) :: fire_is_real_perim
real, dimension(ifms:ifme, jfms:jfme), intent(in) :: fxlong, fxlat, fz0 ! fire mesh coordinates and z0

TYPE (ignition_line_type), DIMENSION(num_ignitions), intent(out):: ignition_line
 
real, intent(inout), dimension(ifms:ifme, jfms:jfme):: &
    lfn,tign,fuel_frac,                        &     ! state: level function, ign time, fuel left
    lfn_hist,                                  &     ! PAJ: to init obs. fira perimeter
    lfn_out,                                   &
    nfuel_cat,                                 &     ! fuel data; can be also set inside (cell based, fire grid)
    lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front  ! level set function and reinitialization steps

real, intent(out), dimension(ifms:ifme, jfms:jfme):: &
    fire_area,                                       &  ! fraction of each cell burning
    burnt_area_dt,                                   &
    fgrnhfx,                                         &  ! heat flux from surface fire (W/m^2) 
    fgrnqfx,                                         &  ! moisture flux from surface fire (W/m^2) 
    fcanhfx,                                         &  ! heat flux from crown fire (W/m^2) 
    fcanqfx,                                         &  ! moisture flux from crown fire (W/m^2) 
    ros,                                             &  ! fire rate of spread (m/s)
    fuel_time

type(fire_params),intent(inout)::fp
  ! moisture model arguments
logical, intent(in)::run_advance_moisture
real, intent(in)::dt_moisture
real, intent(in)::fmep_decay_tlag
real, intent(in), dimension(ifms:ifme,jfms:jfme):: fire_t2, fire_q2, fire_psfc, fire_rain
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: fire_t2_old, fire_q2_old, fire_psfc_old, fire_rain_old 
real, intent(out),dimension(ifms:ifme,jfms:jfme):: fire_rh_fire
real, intent(inout), dimension(ifms:ifme,nfmc,jfms:jfme):: fmc_gc
real, intent(inout), dimension(ifms:ifme,2,jfms:jfme):: fmep
real, intent(out), dimension(ifms:ifme,nfmc,jfms:jfme):: fmc_equi,fmc_lag
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: fmc_g
    
integer :: ignited_tile(num_ignitions) ! number of gridpts ignited in a given ignition
integer :: ignitions_done              ! number of isolated ignitions


ignitions_done = 0

  ! check the tiles
call check_mesh_2dim(ifts-2,ifte+2,jfts-2,jfte+2,ifms,ifme,jfms,jfme)! check if fire node tile fits in memory

if(run_advance_moisture)then
      ! one timestep of the moisture model
      call message('advance_moisture start')
      call advance_moisture(    &
              itimestep.eq.1,             & ! initialize?
              ifms,ifme,  jfms,jfme,      & ! fire memory dimensions
              ifts,ifte,  jfts,jfte,      & ! fire tile dimensions
              nfmc,                       & ! number of moisture fields
              dt_moisture,                & ! moisture model time step
              fmep_decay_tlag,            & ! moisture extended model assim. diffs decay tlag
              fire_rain,                  & ! accumulated rain 
              fire_t2, fire_q2, fire_psfc,& ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
              fire_rain_old,              & ! previous value of accumulated rain
              fire_t2_old, fire_q2_old, fire_psfc_old,   & ! previous values of the atmospheric state at surface
              fire_rh_fire,               & ! relative humidity, diagnostics
              fmc_gc,                     & ! fuel moisture fields updated, by class, assumed set to something reasonable
              fmep,                       & ! fuel moisture extended model parameters
              fmc_equi,                   &  ! fuel moisture fields updated, by class equilibrium diagnostic
              fmc_lag                     &  ! fuel moisture fields updated, by class tendency diagnostic
              )
      call message('advance_moisture end')

      call message('fuel_moisture start')
      call fuel_moisture(         &
          nfmc,                   &
          ifds, ifde, jfds, jfde, & ! fire grid dimensions
          ifms, ifme, jfms, jfme, &
          ifts,ifte,jfts,jfte,    &
          nfuel_cat,              & ! fuel data
          fmc_gc,                 & ! moisture contents by class on atmospheric grid
          fmc_g                   & ! weighted fuel moisture contents on fire grid
          )
      call message('fuel_moisture end')

      ! fuel moisture may have changed, reset the precomputed ros parameters
      ! uses nfuel_cat to set the other fuel data arrays
      ! needs zsf on halo width 1 to compute the terrain gradient
      call set_fire_params(        &       ! also on restart
              ifds,ifde,jfds,jfde, &
              ifms,ifme,jfms,jfme, &
              ifts,ifte,jfts,jfte, &
              dx,dy,nfuel_cat0,    &
              nfuel_cat,fuel_time, &
              fp)

endif

   call fire_model (                            &
        num_ignitions,                          & ! switches
        ifds,ifde,jfds,jfde,                    & ! fire domain dims
        ifms,ifme,jfms,jfme,                    & ! fire memory dims
        ifts,ifte,jfts,jfte,                    & ! fire patch dims
        time_start,dt,                          & ! time and increment
        dx,dy,                                  & ! fire mesh spacing
        ignition_line,                          & ! description of ignition lines
        ignitions_done,ignited_tile,            &
        fxlong,fxlat,unit_fxlong,unit_fxlat,    & ! fire mesh coordinates
        lfn,                                    & ! state: level function
        lfn_hist,                               & ! PAJ: to init obs fire perimeter
        fire_is_real_perim,                     & ! PAJ: to init obs fire perimeter
        lfn_0,lfn_1,lfn_2,lfn_s0,lfn_s1,lfn_s2,lfn_s3,flame_length,ros_front,  & ! state
        lfn_out,tign,fuel_frac,                 & ! state: ign time, fuel left
        fire_area,                              & ! output: fraction of cell burning
        burnt_area_dt,                          & 
        fgrnhfx,fgrnqfx,                        & ! output: heat fluxes
        ros,                                    & ! output: rate of spread for display
        fuel_time,                              & ! save derived internal data
        fp                                      & ! fire coefficients
        )

end subroutine fire_driver_phys

subroutine fire_ignition_convert (config_flags,ignition_line,fire_num_ignitions)

! create ignition arrays from scalar flags

    USE namelist_mod, only : namelist_t

    implicit none

    TYPE (namelist_t) , INTENT(IN)          :: config_flags
    TYPE (ignition_line_type), DIMENSION(FIRE_MAX_IGNITIONS), intent(out):: ignition_line ! any values from input discarded 
    integer, intent(out)::fire_num_ignitions

    integer::i


    ! this is only until I figure out how to input arrays through the namelist...
    if(FIRE_MAX_IGNITIONS.lt.5)call crash('FIRE_MAX_IGNITIONS too small')

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

subroutine set_flags(config_flags)

implicit none

TYPE (namelist_t) , INTENT(IN)          :: config_flags

! copy flags from wrf to module_fr_fire_util
! for instructions how to add a flag see the top of module_fr_fire_util.F
fire_print_msg          = config_flags%fire_print_msg
fuel_left_method        = config_flags%fire_fuel_left_method
fuel_left_irl           = config_flags%fire_fuel_left_irl
fuel_left_jrl           = config_flags%fire_fuel_left_jrl
fire_atm_feedback       = config_flags%fire_atm_feedback
boundary_guard          = config_flags%fire_boundary_guard
fire_grows_only         = config_flags%fire_grows_only
fire_upwinding          = config_flags%fire_upwinding
fire_upwind_split       = config_flags%fire_upwind_split 
fire_viscosity          = config_flags%fire_viscosity 
fire_lfn_ext_up         = config_flags%fire_lfn_ext_up 
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
