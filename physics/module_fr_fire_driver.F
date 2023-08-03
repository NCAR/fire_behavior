
  module module_fr_fire_driver

    use module_fr_fire_model, only: Fire_model
    use module_fr_fire_fuel_moisture_model, only: Init_fuel_moisture, Fuel_moisture_model
    use module_fr_fire_util, only: Check_mesh_2dim
    use ignition_line_mod, only: ignition_line_t, Initialize_ignitions
    use module_fr_fire_fuel_anderson_mod, only: fuel_anderson_t
    use ros_wrffire_mod, only : ros_wrffire_t
    use state_mod, only: state_fire_t
    use namelist_mod, only: namelist_t

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


      if (config_flags%fmoist_run) call Fuel_moisture_model (grid, config_flags, fuel_model, ros_model)

        ! time - assume dt does not change
      time_start = grid%itimestep * grid%dt
      ignitions_done = 0
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
