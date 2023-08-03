
  module module_fr_fire_driver

    use module_fr_fire_model, only: Fire_model
    use module_fr_fire_fuel_moisture_model, only: Advance_moisture, Fuel_moisture, Init_fuel_moisture, Check_for_moisture
    use module_fr_fire_util, only: Crash, Check_mesh_2dim, Message
    use ignition_line_mod, only: ignition_line_t, Initialize_ignitions

    use state_mod, only: state_fire_t
    use namelist_mod, only: namelist_t
    use module_fr_fire_fuel_anderson_mod, only: fuel_anderson_t
    use ros_wrffire_mod, only : ros_wrffire_t

    implicit none

    private

    public :: Fire_driver_em, Init_fire_driver

    type (ignition_line_t), dimension(:), allocatable :: ignition_lines
    type (fuel_anderson_t) :: fuel_model
    type (ros_wrffire_t) :: ros_model

  contains

    subroutine Init_fire_driver (grid, config_flags)

      implicit none

      type (state_fire_t), target :: grid
      type (namelist_t), intent(in) :: config_flags


        ! check if tile fits in memory
      call Check_mesh_2dim(grid%ifts-2,grid%ifte+2,grid%jfts-2,grid%jfte+2,grid%ifms,grid%ifme,grid%jfms,grid%jfme,&
          config_flags%fire_print_msg)

      call Initialize_ignitions (config_flags, ignition_lines)

      if (config_flags%fmoist_run) call Init_fuel_moisture (grid, config_flags, fuel_model)

      call fuel_model%Initialization (config_flags%fuelmc_c)

      call ros_model%Set_ros_parameters (grid%ifds, grid%ifde, grid%jfds, grid%jfde, &
          grid%ifms, grid%ifme, grid%jfms, grid%jfme, grid%ifts, grid%ifte, grid%jfts, grid%jfte, &
          grid%dx, grid%dy, grid%nfuel_cat,grid%fuel_time, grid, fuel_model,config_flags)

    end subroutine Init_fire_driver

    subroutine Fire_driver_em (grid, config_flags)

      implicit none

      type (state_fire_t), target :: grid
      type (namelist_t), intent (in) :: config_flags

        ! number of isolated ignitions
      integer :: ignitions_done
        ! number of gridpts ignited in a given ignition
      integer :: ignited_tile(config_flags%fire_num_ignitions)
      real, dimension(grid%ifms:grid%ifme, grid%jfms:grid%jfme) :: lfn_out
      real:: time_start


        ! time - assume dt does not change
      time_start = grid%itimestep * grid%dt
      ignitions_done = 0

      call Check_for_moisture (grid, config_flags)

      if (grid%run_advance_moisture) then
        call Message ('advance_moisture start', config_flags%fire_print_msg)
        call Advance_moisture (grid%itimestep.eq.1, & ! initialize?
            grid%ifms, grid%ifme, grid%jfms, grid%jfme, grid%ifts, grid%ifte, grid%jfts, grid%jfte, &
            config_flags%fire_print_msg, config_flags%nfmc, grid%dt_moisture, config_flags%fmep_decay_tlag, &
            grid%fire_rain, grid%fire_t2, grid%fire_q2, grid%fire_psfc, grid%fire_rain_old, grid%fire_t2_old, &
            grid%fire_q2_old, grid%fire_psfc_old, grid%fire_rh_fire, grid%fmc_gc,  grid%fmep,  grid%fmc_equi, &
            grid%fmc_lag, config_flags%fuelmc_g)
        call Message ('advance_moisture end',config_flags%fire_print_msg)

        call Message ('fuel_moisture start',config_flags%fire_print_msg)
        call Fuel_moisture (config_flags%nfmc, grid%ifds, grid%ifde, grid%jfds, grid%jfde, &
            grid%ifms, grid%ifme, grid%jfms, grid%jfme, grid%ifts,grid%ifte,grid%jfts,grid%jfte,    &
            config_flags%fire_print_msg, grid%nfuel_cat, grid%fmc_gc, grid%fmc_g)
        call Message ('fuel_moisture end',config_flags%fire_print_msg)

          ! fuel moisture may have changed, reset the precomputed ros parameters
        call ros_model%Set_ros_parameters (grid%ifds, grid%ifde, grid%jfds, grid%jfde, &
            grid%ifms, grid%ifme, grid%jfms, grid%jfme, grid%ifts, grid%ifte, grid%jfts, grid%jfte, &
            grid%dx,grid%dy, grid%nfuel_cat,grid%fuel_time,  grid, fuel_model, config_flags)
      end if

      call fire_model (                            &
          config_flags%fire_num_ignitions,                          & ! switches
          ros_model, &
          grid%ifds,grid%ifde,grid%jfds,grid%jfde,                    & ! fire domain dims
          grid%ifms,grid%ifme,grid%jfms,grid%jfme,                    & ! fire memory dims
          grid%ifts,grid%ifte,grid%jfts,grid%jfte,                    & ! fire patch dims
          time_start,grid%dt,                          & ! time and increment
          grid%dx,grid%dy,                                  & ! fire mesh spacing
          ignition_lines,                          & ! description of ignition lines
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

    end subroutine Fire_driver_em

  end module module_fr_fire_driver
