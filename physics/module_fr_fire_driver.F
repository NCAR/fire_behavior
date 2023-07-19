
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
use module_fr_fire_phys, only : Init_fuel_cats, Set_fire_params
use module_fr_fire_fuel_moisture_model, only: Advance_moisture, Fuel_moisture, Init_fuel_moisture, Check_for_moisture
use module_fr_fire_util, only: Crash, Check_mesh_2dim, Message
use module_fr_fire_core, only: ignition_line_type, Init_no_fire

use state_mod, only: state_fire_t
use namelist_mod, only: namelist_t

  implicit none

  private

  public Fire_driver_em, Init_fire_driver

  integer :: fire_num_ignitions
  integer, parameter :: FIRE_MAX_IGNITIONS = 5
  type (ignition_line_type), dimension (FIRE_MAX_IGNITIONS) :: ignition_line

contains

  subroutine init_fire_driver (grid, config_flags)

    implicit none

    type (state_fire_t), target :: grid
    type (namelist_t), intent(in) :: config_flags

    real :: time_start

      ! get ignition data
    call fire_ignition_convert (config_flags, ignition_line, fire_num_ignitions)

      ! initialize moisture model
    if (config_flags%fmoist_run) then
        call init_fuel_moisture ( grid, config_flags )
    endif

      ! init rest of fuel tables with derived constants
      ! properties of fuel categories and moisture classes from namelist.fire
    call init_fuel_cats ()

      ! uses nfuel_cat to set the other fuel data arrays
      ! needs zsf on halo width 1 to compute the terrain gradient
    call set_fire_params (   &
        grid%ifds,grid%ifde,grid%jfds,grid%jfde, &
        grid%ifms,grid%ifme,grid%jfms,grid%jfme, &
        grid%ifts,grid%ifte,grid%jfts,grid%jfte, &
        grid%dx,grid%dy,                         &
        grid%nfuel_cat,grid%fuel_time, &
        grid,config_flags%fire_fmc_read,config_flags%fire_print_msg)

      ! initialize model state to no fire
    time_start = grid%itimestep * grid%dt
    call init_no_fire  ( &
        grid%ifds,grid%ifde,grid%jfds,grid%jfde, &
        grid%ifms,grid%ifme,grid%jfms,grid%jfme, &
        grid%ifts,grid%ifte,grid%jfts,grid%jfte, &
        grid%dx,grid%dy,time_start,    &
        config_flags%fire_print_msg,   &
        grid%fuel_frac,grid%fire_area,grid%lfn,grid%tign_g)

  end subroutine init_fire_driver

subroutine fire_driver_em ( grid , config_flags )

    implicit none

    type (state_fire_t), target :: grid
    type (namelist_t), intent (in) :: config_flags

    integer :: ignitions_done              ! number of isolated ignitions
    integer :: ignited_tile(fire_num_ignitions) ! number of gridpts ignited in a given ignition
    real, dimension(grid%ifms:grid%ifme, grid%jfms:grid%jfme)::lfn_out  
    real:: time_start

    ! time - assume dt does not change
    time_start = grid%itimestep * grid%dt

    call check_for_moisture( grid, config_flags )

    ignitions_done = 0

      ! check the tiles
    call check_mesh_2dim(grid%ifts-2,grid%ifte+2,grid%jfts-2,grid%jfte+2,grid%ifms,grid%ifme,grid%jfms,grid%jfme,&
        config_flags%fire_print_msg)! check if fire node tile fits in memory

    if(grid%run_advance_moisture)then
          ! one timestep of the moisture model
          call message('advance_moisture start',config_flags%fire_print_msg)
          call advance_moisture(    &
                  grid%itimestep.eq.1,             & ! initialize?
                  grid%ifms,grid%ifme,  grid%jfms,grid%jfme,      & ! fire memory dimensions
                  grid%ifts,grid%ifte,  grid%jfts,grid%jfte,      & ! fire tile dimensions
                  config_flags%fire_print_msg,             &
                  config_flags%nfmc,                       & ! number of moisture fields
                  grid%dt_moisture,                & ! moisture model time step
                  config_flags%fmep_decay_tlag,            & ! moisture extended model assim. diffs decay tlag
                  grid%fire_rain,                  & ! accumulated rain
                  grid%fire_t2, grid%fire_q2, grid%fire_psfc,& ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
                  grid%fire_rain_old,              & ! previous value of accumulated rain
                  grid%fire_t2_old, grid%fire_q2_old, grid%fire_psfc_old,   & ! previous values of the atmospheric state at surface
                  grid%fire_rh_fire,               & ! relative humidity, diagnostics
                  grid%fmc_gc,                     & ! fuel moisture fields updated, by class, assumed set to something reasonable
                  grid%fmep,                       & ! fuel moisture extended model parameters
                  grid%fmc_equi,                   &  ! fuel moisture fields updated, by class equilibrium diagnostic
                  grid%fmc_lag                     &  ! fuel moisture fields updated, by class tendency diagnostic
                  )
          call message('advance_moisture end',config_flags%fire_print_msg)

          call message('fuel_moisture start',config_flags%fire_print_msg)
          call fuel_moisture(         &
              config_flags%nfmc,                   &
              grid%ifds, grid%ifde, grid%jfds, grid%jfde, & ! fire grid dimensions
              grid%ifms, grid%ifme, grid%jfms, grid%jfme, &
              grid%ifts,grid%ifte,grid%jfts,grid%jfte,    &
              config_flags%fire_print_msg,         &
              grid%nfuel_cat,              & ! fuel data
              grid%fmc_gc,                 & ! moisture contents by class on atmospheric grid
              grid%fmc_g                   & ! weighted fuel moisture contents on fire grid
              )
          call message('fuel_moisture end',config_flags%fire_print_msg)

          ! fuel moisture may have changed, reset the precomputed ros parameters
          ! uses nfuel_cat to set the other fuel data arrays
          ! needs zsf on halo width 1 to compute the terrain gradient
          call set_fire_params(        &       ! also on restart
                  grid%ifds,grid%ifde,grid%jfds,grid%jfde, &
                  grid%ifms,grid%ifme,grid%jfms,grid%jfme, &
                  grid%ifts,grid%ifte,grid%jfts,grid%jfte, &
                  grid%dx,grid%dy,                         &
                  grid%nfuel_cat,grid%fuel_time, &
                  grid,config_flags%fire_fmc_read,config_flags%fire_print_msg)

    endif

    call fire_model (                            &
        fire_num_ignitions,                          & ! switches
        grid%ifds,grid%ifde,grid%jfds,grid%jfde,                    & ! fire domain dims
        grid%ifms,grid%ifme,grid%jfms,grid%jfme,                    & ! fire memory dims
        grid%ifts,grid%ifte,grid%jfts,grid%jfte,                    & ! fire patch dims
        time_start,grid%dt,                          & ! time and increment
        grid%dx,grid%dy,                                  & ! fire mesh spacing
        ignition_line,                          & ! description of ignition lines
        ignitions_done,ignited_tile,            &
        grid%lons,grid%lats,grid%unit_fxlong,grid%unit_fxlat,    & ! fire mesh coordinates
        grid%lfn,                                    & ! state: level function
        grid%lfn_hist,                               & ! PAJ: to init obs fire perimeter
        config_flags%fire_is_real_perim,                     & ! PAJ: to init obs fire perimeter
        grid%lfn_0,grid%lfn_1,grid%lfn_2,grid%lfn_s0,grid%lfn_s1,grid%lfn_s2,grid%lfn_s3,grid%flame_length,grid%ros_front,  & ! state
        lfn_out,grid%tign_g,grid%fuel_frac,                 & ! state: ign time, fuel left
        grid%fire_area,                              & ! output: fraction of cell burning
        grid%burnt_area_dt,                          &
        grid%fgrnhfx,grid%fgrnqfx,                        & ! output: heat fluxes
        grid%ros,                                    & ! output: rate of spread for display
        grid%fuel_time,                              & ! save derived internal data
        grid,                                        & ! fire coefficients
        config_flags%fire_fuel_left_method, config_flags%fire_fuel_left_irl, config_flags%fire_fuel_left_jrl, &
        config_flags%fire_boundary_guard, config_flags%fire_grows_only, config_flags%fire_upwinding, &
        config_flags%fire_upwind_split, config_flags%fire_upwinding_reinit, &
        config_flags%fire_lsm_reinit, config_flags%fire_lsm_reinit_iter, &
        config_flags%fire_viscosity, config_flags%fire_viscosity_bg, &
        config_flags%fire_viscosity_band, config_flags%fire_viscosity_ngp, &
        config_flags%fire_advection, config_flags%fire_slope_factor, &
        config_flags%fire_lsm_band_ngp, config_flags%fire_print_msg )

end subroutine fire_driver_em

  subroutine Fire_ignition_convert (config_flags, ignition_line, fire_num_ignitions)

    use namelist_mod, only : namelist_t

    implicit none

    type (namelist_t), intent (in) :: config_flags
    type (ignition_line_type), dimension (FIRE_MAX_IGNITIONS), intent (out):: ignition_line
    integer, intent(out) :: fire_num_ignitions

    integer, parameter :: FIRE_MAX_IGNITIONS_IN_NAMELIST = 5
    integer :: i


    if (FIRE_MAX_IGNITIONS < FIRE_MAX_IGNITIONS_IN_NAMELIST) call crash ('FIRE_MAX_IGNITIONS too small')

    ignition_line(1)%start_x = config_flags%fire_ignition_start_lon1
    ignition_line(1)%start_y = config_flags%fire_ignition_start_lat1
    ignition_line(1)%end_x = config_flags%fire_ignition_end_lon1
    ignition_line(1)%end_y = config_flags%fire_ignition_end_lat1
    ignition_line(1)%ros = config_flags%fire_ignition_ros1
    ignition_line(1)%radius = config_flags%fire_ignition_radius1
    ignition_line(1)%start_time = config_flags%fire_ignition_start_time1
    ignition_line(1)%end_time = config_flags%fire_ignition_end_time1

    ignition_line(2)%start_x = config_flags%fire_ignition_start_lon2
    ignition_line(2)%start_y = config_flags%fire_ignition_start_lat2
    ignition_line(2)%end_x = config_flags%fire_ignition_end_lon2
    ignition_line(2)%end_y = config_flags%fire_ignition_end_lat2
    ignition_line(2)%ros = config_flags%fire_ignition_ros2
    ignition_line(2)%radius = config_flags%fire_ignition_radius2
    ignition_line(2)%start_time = config_flags%fire_ignition_start_time2
    ignition_line(2)%end_time = config_flags%fire_ignition_end_time2

    ignition_line(3)%start_x = config_flags%fire_ignition_start_lon3
    ignition_line(3)%start_y = config_flags%fire_ignition_start_lat3
    ignition_line(3)%end_x = config_flags%fire_ignition_end_lon3
    ignition_line(3)%end_y = config_flags%fire_ignition_end_lat3
    ignition_line(3)%ros = config_flags%fire_ignition_ros3
    ignition_line(3)%radius = config_flags%fire_ignition_radius3
    ignition_line(3)%start_time = config_flags%fire_ignition_start_time3
    ignition_line(3)%end_time = config_flags%fire_ignition_end_time3

    ignition_line(4)%start_x = config_flags%fire_ignition_start_lon4
    ignition_line(4)%start_y = config_flags%fire_ignition_start_lat4
    ignition_line(4)%end_x = config_flags%fire_ignition_end_lon4
    ignition_line(4)%end_y = config_flags%fire_ignition_end_lat4
    ignition_line(4)%ros = config_flags%fire_ignition_ros4
    ignition_line(4)%radius = config_flags%fire_ignition_radius4
    ignition_line(4)%start_time = config_flags%fire_ignition_start_time4
    ignition_line(4)%end_time = config_flags%fire_ignition_end_time4

    ignition_line(5)%start_x = config_flags%fire_ignition_start_lon5
    ignition_line(5)%start_y = config_flags%fire_ignition_start_lat5
    ignition_line(5)%end_x = config_flags%fire_ignition_end_lon5
    ignition_line(5)%end_y = config_flags%fire_ignition_end_lat5
    ignition_line(5)%ros = config_flags%fire_ignition_ros5
    ignition_line(5)%radius = config_flags%fire_ignition_radius5
    ignition_line(5)%start_time = config_flags%fire_ignition_start_time5
    ignition_line(5)%end_time = config_flags%fire_ignition_end_time5

    fire_num_ignitions = 0
    do i = 1, min (FIRE_MAX_IGNITIONS_IN_NAMELIST, config_flags%fire_num_ignitions)
        ! Count the ignitions
      if (ignition_line(i)%radius > 0.0) fire_num_ignitions = i
        ! Expand ignition data given as zero
      if (ignition_line(i)%end_x == 0.0) ignition_line(i)%end_x = ignition_line(i)%start_x
      if (ignition_line(i)%end_y == 0.0) ignition_line(i)%end_y = ignition_line(i)%start_y
      if (ignition_line(i)%end_time == 0.0) ignition_line(i)%end_time = ignition_line(i)%start_time
    end do

  end subroutine Fire_ignition_convert

end module module_fr_fire_driver
