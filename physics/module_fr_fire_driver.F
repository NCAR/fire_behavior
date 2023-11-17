
  module module_fr_fire_driver

    use module_fr_fire_model, only: Fire_model, Print_summary
    use module_fr_fire_fuel_moisture_model, only: Init_fuel_moisture, Fuel_moisture_model
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

      integer :: ij


      call Initialize_ignitions (config_flags, ignition_lines)

      if (config_flags%fmoist_run) call Init_fuel_moisture (grid, config_flags, fuel_model)

      call fuel_model%Initialization (config_flags%fuelmc_c)

      do ij=1,grid%num_tiles
        call ros_model%Set_ros_parameters (grid%ifds, grid%ifde, grid%jfds, grid%jfde, &
            grid%ifms, grid%ifme, grid%jfms, grid%jfme, grid%i_start(ij), grid%i_end(ij), &
            grid%j_start(ij), grid%j_end(ij), grid%dx, grid%dy, grid%nfuel_cat,grid%fuel_time, &
            grid, fuel_model,config_flags)
      enddo

    end subroutine Init_fire_driver

    subroutine Fire_driver_em (grid, config_flags)

      implicit none

      type (state_fire_t), intent (in out) :: grid
      type (namelist_t), intent (in) :: config_flags

      integer :: stat_lev = 1
      integer :: ij

      if (config_flags%fmoist_run) call Fuel_moisture_model (grid, config_flags, fuel_model, ros_model)

      do ij=1,grid%num_tiles
        call Fire_model (config_flags, ros_model, ignition_lines, grid, &
            grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij))
      enddo

      if (config_flags%fire_print_msg >= stat_lev) then
        call Print_summary (config_flags, grid)
      endif

    end subroutine Fire_driver_em

  end module module_fr_fire_driver
