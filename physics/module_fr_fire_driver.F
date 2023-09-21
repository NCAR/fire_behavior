
  module module_fr_fire_driver

    use module_fr_fire_model, only: Fire_model
    use module_fr_fire_fuel_moisture_model, only: Init_fuel_moisture, Fuel_moisture_model
    use module_fr_fire_util, only: Check_dims
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
      call Check_dims (grid%ifts - 2, grid%ifte + 2, grid%jfts - 2, grid%jfte + 2, grid%ifms, &
          grid%ifme, grid%jfms, grid%jfme, config_flags%fire_print_msg)

      call Initialize_ignitions (config_flags, ignition_lines)

      if (config_flags%fmoist_run) call Init_fuel_moisture (grid, config_flags, fuel_model)

      call fuel_model%Initialization (config_flags%fuelmc_c)

      call ros_model%Set_ros_parameters (grid%ifds, grid%ifde, grid%jfds, grid%jfde, &
          grid%ifms, grid%ifme, grid%jfms, grid%jfme, grid%ifts, grid%ifte, grid%jfts, grid%jfte, &
          grid%dx, grid%dy, grid%nfuel_cat,grid%fuel_time, grid, fuel_model,config_flags)

    end subroutine Init_fire_driver

    subroutine Fire_driver_em (grid, config_flags)

      implicit none

      type (state_fire_t), intent (in out) :: grid
      type (namelist_t), intent (in) :: config_flags


      if (config_flags%fmoist_run) call Fuel_moisture_model (grid, config_flags, fuel_model, ros_model)

      call Fire_model (config_flags, ros_model, ignition_lines, grid)

    end subroutine Fire_driver_em

  end module module_fr_fire_driver
