  module namelist_mod

    use namelist_wrf_mod, only : grid_config_rec_type, NUM_FMC

    implicit none

    private

    public :: namelist_t

    type :: namelist_t
        ! WRF namelist
      type (grid_config_rec_type) :: wrf
        ! Fire
      integer :: fire_print_msg = 0           ! "write fire statistics, 0 no writes, 1+ for more"  ""
      integer :: fire_print_file = 0          ! "write fire output text files, 0 no writes, 1+ for more" ""
      integer :: fire_fuel_left_method = 1    ! "submesh to compute fuel lwft, even, at least 2" ""
      integer :: fire_fuel_left_irl = 2       ! "submesh to compute fuel lwft, even, at least 2" ""
      integer :: fire_fuel_left_jrl = 2       ! "submesh to compute fuel lwft, even, at least 2" ""
      real :: fire_const_time = -1.0          ! "time from ignition to freeze fire, <0 never" "s"
      real :: fire_const_grnhfx = 0.0         ! "if both >=0, the amount of constant heat flux" "1"
      real :: fire_const_grnqfx = 0.0         ! "if both >=0, the amount of constant heat flux" "1"
      real :: fire_atm_feedback = 1.0         ! "the heat fluxes to the atmosphere are multiplied by this" "1"
      integer :: fire_boundary_guard = 8      ! "cells to stop when fire close to domain boundary"
      integer :: fire_grows_only = 1          ! "if >0 level set function cannot increase = fire can only grow" "1"
      integer :: fire_upwinding = 9           ! "upwind normal spread: 1=standard, 2=godunov, 3=eno, 4=sethian, 5=2nd-order, 6=WENO3, 7=WENO5, 8=hybrid WENO3/ENO1, 9=hybrid WENO5/ENO1" "1"
      integer :: fire_upwind_split = 0        ! "1=upwind advection separately from normal direction spread" "1"
      real :: fire_viscosity = 0.4            ! "artificial viscosity in level set method" "1"
      real :: fire_lfn_ext_up = 1.0           ! "0.=extend level set function at boundary by reflection, 1.=always up" "1"
      integer :: fire_test_steps = 0          ! ">0 = on first call, do specified number of steps and terminate (testing only)" "1"
      integer :: fire_advection = 1           ! "0 = fire spread computed from normal wind speed/slope, 1 = fireline particle speed projected on normal" "0"
      logical :: fire_lsm_reinit = .true.     ! "flag to activate reinitialization of level set method"
      integer :: fire_lsm_reinit_iter = 1     ! "number of iterations for the reinitialization PDE"
      integer :: fire_upwinding_reinit = 4    ! "numerical scheme (space) for reinitialization PDE: 1=WENO3, 2=WENO5, 3=hybrid WENO3-ENO1, 4=hybrid WENO5-ENO1"
      integer :: fire_lsm_band_ngp = 4        ! "number of grid points around lfn=0 that WENO5/3 is used (ENO1 elsewhere), for fire_upwinding_reinit=4,5 and fire_upwinding=8,9 options"
      logical :: fire_lsm_zcoupling = .false. ! "flag to activate reference velocity at a different height from fire_wind_height"
      real :: fire_lsm_zcoupling_ref = 50.0   ! "reference height from wich u at fire_wind_hegiht is calculated using a logarithmic profile" "m"
      real :: fire_viscosity_bg = 0.4         ! "artificial viscosity in the near-front region" "1"
      real :: fire_viscosity_band = 0.5       ! "number of times the hybrid advection band to transition from fire_viscosity_bg to fire_viscosity" "1"
      integer :: fire_viscosity_ngp = 2       ! "number of grid points around lfn=0 where low artificial viscosity is used = fire_viscosity_bg"
      real :: fire_slope_factor = 1.0         ! "slope correction factor" "-"
      integer :: fire_fmc_read = 1            ! "ground fuel moisture is set by: if 0, in wrfinput; if 1, user-presc; if 2, read from file"
      logical :: fmoist_run = .false.         ! "run moisture model (on the atmospheric grid), output to fmc_gc"
      logical :: fmoist_interp = .false.      ! "interpolate moisture from the model or the input to fuels on the fire grid"
      logical :: fmoist_only = .false.        ! "only run moisture model, skip fire"
      integer :: fmoist_freq = 0              ! "frequency to run moisture model 0: use fmoist_dt, k>0: every k timesteps" "1"
      real :: fmoist_dt = 600                 ! "moisture model time step" "s"
      integer :: fire_fuel_read = -1          ! "fuel categories are set by: if 0, uniform; if 1, user-presc; if 2, read from file"   ""
      integer :: fire_fuel_cat = 1            ! "fuel category if ifuelread=0"              ""
      real :: fire_ext_grnd = 50.0            ! "extinction depth of sfc fire heat"   "m"
      real :: fire_ext_crwn = 50.0            ! "extinction depth of crown fire heat" "m"
      real :: fire_crwn_hgt = 15.0            ! "height that heat from crown fire is released" "m"
      real :: fire_wind_height = 6.096        ! "height of uah,vah wind in fire spread formula" "m"
      logical :: fire_is_real_perim = .false. ! .false. = point/line ignition, .true. = observed perimeter"
      integer :: nfmc = NUM_FMC               ! "number of fuel moisture classes" related to NUM_NFMC
      real :: fmep_decay_tlag = 999999        ! "time constant of assimilated adjustments of equilibria decay" "1"
      integer :: tracer_opt = 0               ! 3) Activates smoke tracer
      real :: fire_tracer_smoke = 0.02        ! "parts per unit of burned fuel becoming smoke (tracer_opt=3)" "g_smoke/kg_air"

        ! Ignitions
      integer :: fire_num_ignitions = 0 ! "number of ignition lines"

      real :: fire_ignition_start_x1 = 0.0    ! "x coord of start of ignition line" "m"
      real :: fire_ignition_start_y1 = 0.0    ! "y coord of start of ignition line" "m"
      real :: fire_ignition_start_lon1 = 0.0  ! "long coord of start of ignition line" "deg"
      real :: fire_ignition_start_lat1 = 0.0  ! "lat coord of start of ignition line" "deg"
      real :: fire_ignition_end_x1 = 0.0      ! "x coord of end of ignition line" "m"
      real :: fire_ignition_end_y1 = 0.0      ! "y coord of end of ignition line" "m"
      real :: fire_ignition_end_lon1 = 0.0    ! "long coord of end of ignition line" "deg"
      real :: fire_ignition_end_lat1 = 0.0    ! "lat coord of end of ignition line" "deg"
      real :: fire_ignition_ros1 = 0.01       ! "rate of spread during ignition" "m/s"
      real :: fire_ignition_start_time1 = 0.0 ! "ignition line start time" "s"
      real :: fire_ignition_end_time1 = 0.0   ! "ignition line end time" "s"
      real :: fire_ignition_radius1 = 0.0     ! "ignite all within the radius" "m"

      real :: fire_ignition_start_x2 = 0.0
      real :: fire_ignition_start_y2 = 0.0
      real :: fire_ignition_start_lon2 = 0.0
      real :: fire_ignition_start_lat2 = 0.0
      real :: fire_ignition_end_x2 = 0.0
      real :: fire_ignition_end_y2 = 0.0
      real :: fire_ignition_end_lon2 = 0.0
      real :: fire_ignition_end_lat2 = 0.0
      real :: fire_ignition_ros2 = 0.01
      real :: fire_ignition_start_time2 = 0.0
      real :: fire_ignition_end_time2 = 0.0
      real :: fire_ignition_radius2 = 0.0

      real :: fire_ignition_start_x3 = 0.0
      real :: fire_ignition_start_y3 = 0.0
      real :: fire_ignition_start_lon3 = 0.0
      real :: fire_ignition_start_lat3 = 0.0
      real :: fire_ignition_end_x3 = 0.0
      real :: fire_ignition_end_y3 = 0.0
      real :: fire_ignition_end_lon3 = 0.0
      real :: fire_ignition_end_lat3 = 0.0
      real :: fire_ignition_ros3 = 0.01
      real :: fire_ignition_start_time3 = 0.0
      real :: fire_ignition_end_time3 = 0.0
      real :: fire_ignition_radius3 = 0.0

      real :: fire_ignition_start_x4 = 0.0
      real :: fire_ignition_start_y4 = 0.0
      real :: fire_ignition_start_lon4 = 0.0
      real :: fire_ignition_start_lat4 = 0.0
      real :: fire_ignition_end_x4 = 0.0
      real :: fire_ignition_end_y4 = 0.0
      real :: fire_ignition_end_lon4 = 0.0
      real :: fire_ignition_end_lat4 = 0.0
      real :: fire_ignition_ros4 = 0.01
      real :: fire_ignition_start_time4 = 0.0
      real :: fire_ignition_end_time4 = 0.0
      real :: fire_ignition_radius4 = 0.0

      real :: fire_ignition_start_x5 = 0.0
      real :: fire_ignition_start_y5 = 0.0
      real :: fire_ignition_start_lon5 = 0.0
      real :: fire_ignition_start_lat5 = 0.0
      real :: fire_ignition_end_x5 = 0.0
      real :: fire_ignition_end_y5 = 0.0
      real :: fire_ignition_end_lon5 = 0.0
      real :: fire_ignition_end_lat5 = 0.0
      real :: fire_ignition_ros5 = 0.01
      real :: fire_ignition_start_time5 = 0.0
      real :: fire_ignition_end_time5 = 0.0
      real :: fire_ignition_radius5 = 0.0
    end type namelist_t

    interface namelist_t
      module procedure Namelist_t_const
    end interface namelist_t

  contains

    function Namelist_t_const (file_name) result (return_value)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      character (len = *), intent (in) :: file_name
      type (namelist_t) :: return_value

      logical, parameter :: DEBUG_LOCAL = .true.

      integer :: fire_print_msg = 0           ! "write fire statistics, 0 no writes, 1+ for more"  ""
      integer :: fire_print_file = 0          ! "write fire output text files, 0 no writes, 1+ for more" ""
      integer :: fire_fuel_left_method = 1    ! "submesh to compute fuel lwft, even, at least 2" ""
      integer :: fire_fuel_left_irl = 2       ! "submesh to compute fuel lwft, even, at least 2" ""
      integer :: fire_fuel_left_jrl = 2       ! "submesh to compute fuel lwft, even, at least 2" ""
      real :: fire_const_time = -1.0          ! "time from ignition to freeze fire, <0 never" "s"
      real :: fire_const_grnhfx = 0.0         ! "if both >=0, the amount of constant heat flux" "1"
      real :: fire_const_grnqfx = 0.0         ! "if both >=0, the amount of constant heat flux" "1"
      real :: fire_atm_feedback = 1.0         ! "the heat fluxes to the atmosphere are multiplied by this" "1"
      integer :: fire_boundary_guard = 8      ! "cells to stop when fire close to domain boundary"
      integer :: fire_grows_only = 1          ! "if >0 level set function cannot increase = fire can only grow" "1"
      integer :: fire_upwinding = 9           ! "upwind normal spread: 1=standard, 2=godunov, 3=eno, 4=sethian, 5=2nd-order, 6=WENO3, 7=WENO5, 8=hybrid WENO3/ENO1, 9=hybrid WENO5/ENO1" "1"
      integer :: fire_upwind_split = 0        ! "1=upwind advection separately from normal direction spread" "1"
      real :: fire_viscosity = 0.4            ! "artificial viscosity in level set method" "1"
      real :: fire_lfn_ext_up = 1.0           ! "0.=extend level set function at boundary by reflection, 1.=always up" "1"
      integer :: fire_test_steps = 0          ! ">0 = on first call, do specified number of steps and terminate (testing only)" "1"
      integer :: fire_advection = 1           ! "0 = fire spread computed from normal wind speed/slope, 1 = fireline particle speed projected on normal" "0"
      logical :: fire_lsm_reinit = .true.     ! "flag to activate reinitialization of level set method"
      integer :: fire_lsm_reinit_iter = 1     ! "number of iterations for the reinitialization PDE"
      integer :: fire_upwinding_reinit = 4    ! "numerical scheme (space) for reinitialization PDE: 1=WENO3, 2=WENO5, 3=hybrid WENO3-ENO1, 4=hybrid WENO5-ENO1"
      integer :: fire_lsm_band_ngp = 4        ! "number of grid points around lfn=0 that WENO5/3 is used (ENO1 elsewhere), for fire_upwinding_reinit=4,5 and fire_upwinding=8,9 options"
      logical :: fire_lsm_zcoupling = .false. ! "flag to activate reference velocity at a different height from fire_wind_height"
      real :: fire_lsm_zcoupling_ref = 50.0   ! "reference height from wich u at fire_wind_hegiht is calculated using a logarithmic profile" "m"
      real :: fire_viscosity_bg = 0.4         ! "artificial viscosity in the near-front region" "1"
      real :: fire_viscosity_band = 0.5       ! "number of times the hybrid advection band to transition from fire_viscosity_bg to fire_viscosity" "1"
      integer :: fire_viscosity_ngp = 2       ! "number of grid points around lfn=0 where low artificial viscosity is used = fire_viscosity_bg"
      real :: fire_slope_factor = 1.0         ! "slope correction factor" "-"
      integer :: fire_fmc_read = 1            ! "ground fuel moisture is set by: if 0, in wrfinput; if 1, user-presc; if 2, read from file" 
      logical :: fmoist_run = .false.         ! "run moisture model (on the atmospheric grid), output to fmc_gc"
      logical :: fmoist_interp = .false.      ! "interpolate moisture from the model or the input to fuels on the fire grid"
      logical :: fmoist_only = .false.        ! "only run moisture model, skip fire"
      integer :: fmoist_freq = 0              ! "frequency to run moisture model 0: use fmoist_dt, k>0: every k timesteps" "1"
      real :: fmoist_dt = 600                 ! "moisture model time step" "s"
      integer :: fire_fuel_read = -1          ! "fuel categories are set by: if 0, uniform; if 1, user-presc; if 2, read from file"   ""
      integer :: fire_fuel_cat = 1            ! "fuel category if ifuelread=0"              ""
      real :: fire_ext_grnd = 50.0            ! "extinction depth of sfc fire heat"   "m"
      real :: fire_ext_crwn = 50.0            ! "extinction depth of crown fire heat" "m"
      real :: fire_crwn_hgt = 15.0            ! "height that heat from crown fire is released" "m"
      real :: fire_wind_height = 6.096        ! "height of uah,vah wind in fire spread formula" "m"
      logical :: fire_is_real_perim = .false. ! .false. = point/line ignition, .true. = observed perimeter"
      integer :: nfmc = NUM_FMC               ! "number of fuel moisture classes" related to NUM_NFMC
      real :: fmep_decay_tlag = 999999        ! "time constant of assimilated adjustments of equilibria decay" "1"
      integer :: tracer_opt = 0               ! 3) Activates smoke tracer
      real :: fire_tracer_smoke = 0.02        ! "parts per unit of burned fuel becoming smoke (tracer_opt=3)" "g_smoke/kg_air"

        ! ignitions
      integer :: fire_num_ignitions = 0

      real :: fire_ignition_start_x1 = 0.0    ! "x coord of start of ignition line" "m"
      real :: fire_ignition_start_y1 = 0.0    ! "y coord of start of ignition line" "m"
      real :: fire_ignition_start_lon1 = 0.0  ! "long coord of start of ignition line" "deg"
      real :: fire_ignition_start_lat1 = 0.0  ! "lat coord of start of ignition line" "deg"
      real :: fire_ignition_end_x1 = 0.0      ! "x coord of end of ignition line" "m"
      real :: fire_ignition_end_y1 = 0.0      ! "y coord of end of ignition line" "m"
      real :: fire_ignition_end_lon1 = 0.0    ! "long coord of end of ignition line" "deg"
      real :: fire_ignition_end_lat1 = 0.0    ! "lat coord of end of ignition line" "deg"
      real :: fire_ignition_ros1 = 0.01       ! "rate of spread during ignition" "m/s"
      real :: fire_ignition_start_time1 = 0.0 ! "ignition line start time" "s"
      real :: fire_ignition_end_time1 = 0.0   ! "ignition line end time" "s"
      real :: fire_ignition_radius1 = 0.0     ! "ignite all within the radius" "m"

      real :: fire_ignition_start_x2 = 0.0, fire_ignition_start_y2 = 0.0, fire_ignition_start_lon2 = 0.0, &
          fire_ignition_start_lat2 = 0.0, fire_ignition_end_x2 = 0.0, fire_ignition_end_y2 = 0.0, fire_ignition_end_lon2 = 0.0, &
          fire_ignition_end_lat2 = 0.0, fire_ignition_ros2 = 0.01, fire_ignition_start_time2 = 0.0, fire_ignition_end_time2 = 0.0, &
          fire_ignition_radius2 = 0.0

      real :: fire_ignition_start_x3 = 0.0, fire_ignition_start_y3 = 0.0, fire_ignition_start_lon3 = 0.0, &
          fire_ignition_start_lat3 = 0.0, fire_ignition_end_x3 = 0.0, fire_ignition_end_y3 = 0.0, fire_ignition_end_lon3 = 0.0, &
          fire_ignition_end_lat3 = 0.0, fire_ignition_ros3 = 0.01, fire_ignition_start_time3 = 0.0, fire_ignition_end_time3 = 0.0, &
          fire_ignition_radius3 = 0.0

      real :: fire_ignition_start_x4 = 0.0, fire_ignition_start_y4 = 0.0, fire_ignition_start_lon4 = 0.0, &
          fire_ignition_start_lat4 = 0.0, fire_ignition_end_x4 = 0.0, fire_ignition_end_y4 = 0.0, fire_ignition_end_lon4 = 0.0, &
          fire_ignition_end_lat4 = 0.0, fire_ignition_ros4 = 0.01, fire_ignition_start_time4 = 0.0, fire_ignition_end_time4 = 0.0, &
          fire_ignition_radius4 = 0.0

      real :: fire_ignition_start_x5 = 0.0, fire_ignition_start_y5 = 0.0, fire_ignition_start_lon5 = 0.0, &
          fire_ignition_start_lat5 = 0.0, fire_ignition_end_x5 = 0.0, fire_ignition_end_y5 = 0.0, fire_ignition_end_lon5 = 0.0, &
          fire_ignition_end_lat5 = 0.0, fire_ignition_ros5 = 0.01, fire_ignition_start_time5 = 0.0, fire_ignition_end_time5 = 0.0, &
          fire_ignition_radius5 = 0.0

      namelist /fire/  fire_print_msg, fire_print_file, fire_fuel_left_method, fire_fuel_left_irl, fire_fuel_left_jrl, &
          fire_const_time, fire_const_grnhfx, fire_const_grnqfx, fire_atm_feedback, fire_boundary_guard, fire_grows_only, &
          fire_upwinding, fire_upwind_split, fire_viscosity, fire_lfn_ext_up, fire_test_steps, fire_advection, fire_lsm_reinit, &
          fire_lsm_reinit_iter, fire_upwinding_reinit, fire_lsm_band_ngp, fire_lsm_zcoupling, fire_lsm_zcoupling_ref, &
          fire_viscosity_bg, fire_viscosity_band, fire_viscosity_ngp, fire_slope_factor, fire_fmc_read, fmoist_run, fmoist_interp, &
          fmoist_only, fmoist_freq, fmoist_dt, fire_fuel_read, fire_fuel_cat, fire_ext_grnd, fire_ext_crwn, fire_crwn_hgt, &
          fire_wind_height, fire_is_real_perim, nfmc, fmep_decay_tlag, tracer_opt, fire_tracer_smoke, &
            ! Ignitions
          fire_num_ignitions, &
            ! Ignition 1
          fire_ignition_start_x1, fire_ignition_start_y1, fire_ignition_start_lon1, &
          fire_ignition_start_lat1, fire_ignition_end_x1, fire_ignition_end_y1, fire_ignition_end_lon1, &
          fire_ignition_end_lat1, fire_ignition_ros1, fire_ignition_start_time1, fire_ignition_end_time1, &
          fire_ignition_radius1, &
            ! Ignition 2
          fire_ignition_start_x2, fire_ignition_start_y2, fire_ignition_start_lon2, &
          fire_ignition_start_lat2, fire_ignition_end_x2, fire_ignition_end_y2, fire_ignition_end_lon2, &
          fire_ignition_end_lat2, fire_ignition_ros2, fire_ignition_start_time2, fire_ignition_end_time2, &
          fire_ignition_radius2, &
            ! Ignition 3
          fire_ignition_start_x3, fire_ignition_start_y3, fire_ignition_start_lon3, &
          fire_ignition_start_lat3, fire_ignition_end_x3, fire_ignition_end_y3, fire_ignition_end_lon3, &
          fire_ignition_end_lat3, fire_ignition_ros3, fire_ignition_start_time3, fire_ignition_end_time3, &
          fire_ignition_radius3, &
            ! Ignition 4
          fire_ignition_start_x4, fire_ignition_start_y4, fire_ignition_start_lon4, &
          fire_ignition_start_lat4, fire_ignition_end_x4, fire_ignition_end_y4, fire_ignition_end_lon4, &
          fire_ignition_end_lat4, fire_ignition_ros4, fire_ignition_start_time4, fire_ignition_end_time4, &
          fire_ignition_radius4, &
            ! Ignition 5
          fire_ignition_start_x5, fire_ignition_start_y5, fire_ignition_start_lon5, &
          fire_ignition_start_lat5, fire_ignition_end_x5, fire_ignition_end_y5, fire_ignition_end_lon5, &
          fire_ignition_end_lat5, fire_ignition_ros5, fire_ignition_start_time5, fire_ignition_end_time5, &
          fire_ignition_radius5

      integer :: unit_nml

      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Entering subroutine Read_namelist'

      open (newunit = unit_nml, file = trim (file_name), action = 'read')
      read (unit_nml, nml = fire)

      return_value%fire_print_msg = fire_print_msg
      return_value%fire_print_file = fire_print_file
      return_value%fire_fuel_left_method = fire_fuel_left_method
      return_value%fire_fuel_left_irl = fire_fuel_left_irl
      return_value%fire_fuel_left_jrl = fire_fuel_left_jrl
      return_value%fire_const_time = fire_const_time
      return_value%fire_const_grnhfx = fire_const_grnhfx
      return_value%fire_const_grnqfx = fire_const_grnqfx
      return_value%fire_atm_feedback = fire_atm_feedback
      return_value%fire_boundary_guard = fire_boundary_guard
      return_value%fire_grows_only = fire_grows_only
      return_value%fire_upwinding = fire_upwinding
      return_value%fire_upwind_split = fire_upwind_split
      return_value%fire_viscosity = fire_viscosity
      return_value%fire_lfn_ext_up = fire_lfn_ext_up
      return_value%fire_test_steps = fire_test_steps
      return_value%fire_advection = fire_advection
      return_value%fire_lsm_reinit = fire_lsm_reinit
      return_value%fire_lsm_reinit_iter = fire_lsm_reinit_iter
      return_value%fire_upwinding_reinit = fire_upwinding_reinit
      return_value%fire_lsm_band_ngp = fire_lsm_band_ngp
      return_value%fire_lsm_zcoupling = fire_lsm_zcoupling
      return_value%fire_lsm_zcoupling_ref = fire_lsm_zcoupling_ref
      return_value%fire_viscosity_bg = fire_viscosity_bg
      return_value%fire_viscosity_band = fire_viscosity_band
      return_value%fire_viscosity_ngp = fire_viscosity_ngp
      return_value%fire_slope_factor = fire_slope_factor
      return_value%fire_fmc_read = fire_fmc_read
      return_value%fmoist_run = fmoist_run
      return_value%fmoist_interp = fmoist_interp
      return_value%fmoist_only = fmoist_only
      return_value%fmoist_freq = fmoist_freq
      return_value%fmoist_dt = fmoist_dt
      return_value%fire_fuel_read = fire_fuel_read
      return_value%fire_fuel_cat = fire_fuel_cat
      return_value%fire_ext_grnd = fire_ext_grnd
      return_value%fire_ext_crwn = fire_ext_crwn
      return_value%fire_crwn_hgt = fire_crwn_hgt
      return_value%fire_wind_height = fire_wind_height
      return_value%fire_is_real_perim = fire_is_real_perim
      return_value%nfmc = nfmc
      return_value%fmep_decay_tlag = fmep_decay_tlag
      return_value%tracer_opt = tracer_opt
      return_value%fire_tracer_smoke = fire_tracer_smoke

      return_value%fire_num_ignitions = fire_num_ignitions

      return_value%fire_ignition_start_x1 = fire_ignition_start_x1
      return_value%fire_ignition_start_y1 = fire_ignition_start_y1
      return_value%fire_ignition_start_lon1 = fire_ignition_start_lon1
      return_value%fire_ignition_start_lat1 = fire_ignition_start_lat1
      return_value%fire_ignition_end_x1 = fire_ignition_end_x1
      return_value%fire_ignition_end_y1 = fire_ignition_end_y1
      return_value%fire_ignition_end_lon1 = fire_ignition_end_lon1
      return_value%fire_ignition_end_lat1 = fire_ignition_end_lat1
      return_value%fire_ignition_ros1 = fire_ignition_ros1
      return_value%fire_ignition_start_time1 = fire_ignition_start_time1
      return_value%fire_ignition_end_time1 = fire_ignition_end_time1
      return_value%fire_ignition_radius1 = fire_ignition_radius1

      return_value%fire_ignition_start_x2 = fire_ignition_start_x2
      return_value%fire_ignition_start_y2 = fire_ignition_start_y2
      return_value%fire_ignition_start_lon2 = fire_ignition_start_lon2
      return_value%fire_ignition_start_lat2 = fire_ignition_start_lat2
      return_value%fire_ignition_end_x2 = fire_ignition_end_x2
      return_value%fire_ignition_end_y2 = fire_ignition_end_y2
      return_value%fire_ignition_end_lon2 = fire_ignition_end_lon2
      return_value%fire_ignition_end_lat2 = fire_ignition_end_lat2
      return_value%fire_ignition_ros2 = fire_ignition_ros2
      return_value%fire_ignition_start_time2 = fire_ignition_start_time2
      return_value%fire_ignition_end_time2 = fire_ignition_end_time2
      return_value%fire_ignition_radius2 = fire_ignition_radius2

      return_value%fire_ignition_start_x3 = fire_ignition_start_x3
      return_value%fire_ignition_start_y3 = fire_ignition_start_y3
      return_value%fire_ignition_start_lon3 = fire_ignition_start_lon3
      return_value%fire_ignition_start_lat3 = fire_ignition_start_lat3
      return_value%fire_ignition_end_x3 = fire_ignition_end_x3
      return_value%fire_ignition_end_y3 = fire_ignition_end_y3
      return_value%fire_ignition_end_lon3 = fire_ignition_end_lon3
      return_value%fire_ignition_end_lat3 = fire_ignition_end_lat3
      return_value%fire_ignition_ros3 = fire_ignition_ros3
      return_value%fire_ignition_start_time3 = fire_ignition_start_time3
      return_value%fire_ignition_end_time3 = fire_ignition_end_time3
      return_value%fire_ignition_radius3 = fire_ignition_radius3

      return_value%fire_ignition_start_x4 = fire_ignition_start_x4
      return_value%fire_ignition_start_y4 = fire_ignition_start_y4
      return_value%fire_ignition_start_lon4 = fire_ignition_start_lon4
      return_value%fire_ignition_start_lat4 = fire_ignition_start_lat4
      return_value%fire_ignition_end_x4 = fire_ignition_end_x4
      return_value%fire_ignition_end_y4 = fire_ignition_end_y4
      return_value%fire_ignition_end_lon4 = fire_ignition_end_lon4
      return_value%fire_ignition_end_lat4 = fire_ignition_end_lat4
      return_value%fire_ignition_ros4 = fire_ignition_ros4
      return_value%fire_ignition_start_time4 = fire_ignition_start_time4
      return_value%fire_ignition_end_time4 = fire_ignition_end_time4
      return_value%fire_ignition_radius4 = fire_ignition_radius4

      return_value%fire_ignition_start_x5 = fire_ignition_start_x5
      return_value%fire_ignition_start_y5 = fire_ignition_start_y5
      return_value%fire_ignition_start_lon5 = fire_ignition_start_lon5
      return_value%fire_ignition_start_lat5 = fire_ignition_start_lat5
      return_value%fire_ignition_end_x5 = fire_ignition_end_x5
      return_value%fire_ignition_end_y5 = fire_ignition_end_y5
      return_value%fire_ignition_end_lon5 = fire_ignition_end_lon5
      return_value%fire_ignition_end_lat5 = fire_ignition_end_lat5
      return_value%fire_ignition_ros5 = fire_ignition_ros5
      return_value%fire_ignition_start_time5 = fire_ignition_start_time5
      return_value%fire_ignition_end_time5 = fire_ignition_end_time5
      return_value%fire_ignition_radius5 = fire_ignition_radius5

      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Leaving subroutine Read_namelist'

    end function Namelist_t_const

  end module namelist_mod
