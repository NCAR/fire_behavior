  module namelist_mod

    implicit none

    private

    public :: namelist_t, NUM_FMC

    integer, parameter :: NUM_FMC = 5

    type :: namelist_fire_t
      real :: dx = 200.0, dy = 200.0, dt = 2.0
      integer ::  n_steps = 1
      logical :: read_wrf_input = .false.
      logical ::  check_tends = .false.
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
      integer :: fire_upwinding = 9           ! "upwind normal spread: 1=standard, 2=godunov, 3=eno, 4=sethian, 5=2nd-order,
                                              ! 6=WENO3, 7=WENO5, 8=hybrid WENO3/ENO1, 9=hybrid WENO5/ENO1" "1"
      integer :: fire_upwind_split = 0        ! "1=upwind advection separately from normal direction spread" "1"
      real :: fire_viscosity = 0.4            ! "artificial viscosity in level set method" "1"
      real :: fire_lfn_ext_up = 1.0           ! "0.=extend level set function at boundary by reflection, 1.=always up" "1"
      integer :: fire_test_steps = 0          ! ">0 = on first call, do specified number of steps and terminate (testing only)" "1"
      integer :: fire_advection = 1           ! "0 = fire spread computed from normal wind speed/slope, 1 = fireline particle speed projected on normal" "0"
      logical :: fire_lsm_reinit = .true.     ! "flag to activate reinitialization of level set method"
      integer :: fire_lsm_reinit_iter = 1     ! "number of iterations for the reinitialization PDE"
      integer :: fire_upwinding_reinit = 4    ! "numerical scheme (space) for reinitialization PDE: 1=WENO3, 2=WENO5, 3=hybrid WENO3-ENO1, 4=hybrid WENO5-ENO1"
      integer :: fire_lsm_band_ngp = 4        ! "number of grid points around lfn=0 that WENO5/3 is used (ENO1 elsewhere),
                                              ! for fire_upwinding_reinit=4,5 and fire_upwinding=8,9 options"
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

    contains
      procedure, public :: Initialization => Init_namelist
    end type namelist_fire_t

    type, extends (namelist_fire_t) :: namelist_t
        ! Atmosphere
      integer :: ids = 1, ide = 1, jds = 1, jde = 1, kds = 1, kde = 1, sr_x = 1, sr_y = 1
      logical :: restart = .false.
      real :: cen_lat = 0.0 ! "center latitude"      "degrees, negative is south"
      real :: cen_lon = 0.0 ! "central longitude"      "degrees, negative is west"
    end type namelist_t

  contains

    subroutine Init_namelist (this, file_name)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT

      implicit none

      class (namelist_fire_t), intent (in out) :: this
      character (len = *), intent (in) :: file_name

      logical, parameter :: DEBUG_LOCAL = .true.

      real :: dx = 200.0, dy = 200.0, dt = 2.0
      integer :: ids = 1, ide = 1, jds = 1, jde = 1, kds = 1, kde = 1, sr_x = 1, sr_y = 1
      integer ::  n_steps = 1
      logical :: read_wrf_input = .false.
      logical ::  check_tends = .false.
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

      logical :: restart = .false.
      real :: cen_lat = 0.0, cen_lon = 0.0

      namelist /control/ restart, cen_lat, cen_lon, dx, dy, dt, ids, ide, jds, jde, kds, kde, sr_x, sr_y, &
          ids, ide, jds, jde, kds, kde, sr_x, sr_y, n_steps, read_wrf_input, check_tends
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

      read (unit_nml, nml = control)
      read (unit_nml, nml = fire)

      this%dx = dx
      this%dy = dy
      this%dt = dt

      this%n_steps = n_steps
      this%read_wrf_input = read_wrf_input
      this%check_tends = check_tends

      this%fire_print_msg = fire_print_msg
      this%fire_print_file = fire_print_file
      this%fire_fuel_left_method = fire_fuel_left_method
      this%fire_fuel_left_irl = fire_fuel_left_irl
      this%fire_fuel_left_jrl = fire_fuel_left_jrl
      this%fire_const_time = fire_const_time
      this%fire_const_grnhfx = fire_const_grnhfx
      this%fire_const_grnqfx = fire_const_grnqfx
      this%fire_atm_feedback = fire_atm_feedback
      this%fire_boundary_guard = fire_boundary_guard
      this%fire_grows_only = fire_grows_only
      this%fire_upwinding = fire_upwinding
      this%fire_upwind_split = fire_upwind_split
      this%fire_viscosity = fire_viscosity
      this%fire_lfn_ext_up = fire_lfn_ext_up
      this%fire_test_steps = fire_test_steps
      this%fire_advection = fire_advection
      this%fire_lsm_reinit = fire_lsm_reinit
      this%fire_lsm_reinit_iter = fire_lsm_reinit_iter
      this%fire_upwinding_reinit = fire_upwinding_reinit
      this%fire_lsm_band_ngp = fire_lsm_band_ngp
      this%fire_lsm_zcoupling = fire_lsm_zcoupling
      this%fire_lsm_zcoupling_ref = fire_lsm_zcoupling_ref
      this%fire_viscosity_bg = fire_viscosity_bg
      this%fire_viscosity_band = fire_viscosity_band
      this%fire_viscosity_ngp = fire_viscosity_ngp
      this%fire_slope_factor = fire_slope_factor
      this%fire_fmc_read = fire_fmc_read
      this%fmoist_run = fmoist_run
      this%fmoist_interp = fmoist_interp
      this%fmoist_only = fmoist_only
      this%fmoist_freq = fmoist_freq
      this%fmoist_dt = fmoist_dt
      this%fire_fuel_read = fire_fuel_read
      this%fire_fuel_cat = fire_fuel_cat
      this%fire_ext_grnd = fire_ext_grnd
      this%fire_ext_crwn = fire_ext_crwn
      this%fire_crwn_hgt = fire_crwn_hgt
      this%fire_wind_height = fire_wind_height
      this%fire_is_real_perim = fire_is_real_perim
      this%nfmc = nfmc
      this%fmep_decay_tlag = fmep_decay_tlag
      this%tracer_opt = tracer_opt
      this%fire_tracer_smoke = fire_tracer_smoke

      this%fire_num_ignitions = fire_num_ignitions

      this%fire_ignition_start_x1 = fire_ignition_start_x1
      this%fire_ignition_start_y1 = fire_ignition_start_y1
      this%fire_ignition_start_lon1 = fire_ignition_start_lon1
      this%fire_ignition_start_lat1 = fire_ignition_start_lat1
      this%fire_ignition_end_x1 = fire_ignition_end_x1
      this%fire_ignition_end_y1 = fire_ignition_end_y1
      this%fire_ignition_end_lon1 = fire_ignition_end_lon1
      this%fire_ignition_end_lat1 = fire_ignition_end_lat1
      this%fire_ignition_ros1 = fire_ignition_ros1
      this%fire_ignition_start_time1 = fire_ignition_start_time1
      this%fire_ignition_end_time1 = fire_ignition_end_time1
      this%fire_ignition_radius1 = fire_ignition_radius1

      this%fire_ignition_start_x2 = fire_ignition_start_x2
      this%fire_ignition_start_y2 = fire_ignition_start_y2
      this%fire_ignition_start_lon2 = fire_ignition_start_lon2
      this%fire_ignition_start_lat2 = fire_ignition_start_lat2
      this%fire_ignition_end_x2 = fire_ignition_end_x2
      this%fire_ignition_end_y2 = fire_ignition_end_y2
      this%fire_ignition_end_lon2 = fire_ignition_end_lon2
      this%fire_ignition_end_lat2 = fire_ignition_end_lat2
      this%fire_ignition_ros2 = fire_ignition_ros2
      this%fire_ignition_start_time2 = fire_ignition_start_time2
      this%fire_ignition_end_time2 = fire_ignition_end_time2
      this%fire_ignition_radius2 = fire_ignition_radius2

      this%fire_ignition_start_x3 = fire_ignition_start_x3
      this%fire_ignition_start_y3 = fire_ignition_start_y3
      this%fire_ignition_start_lon3 = fire_ignition_start_lon3
      this%fire_ignition_start_lat3 = fire_ignition_start_lat3
      this%fire_ignition_end_x3 = fire_ignition_end_x3
      this%fire_ignition_end_y3 = fire_ignition_end_y3
      this%fire_ignition_end_lon3 = fire_ignition_end_lon3
      this%fire_ignition_end_lat3 = fire_ignition_end_lat3
      this%fire_ignition_ros3 = fire_ignition_ros3
      this%fire_ignition_start_time3 = fire_ignition_start_time3
      this%fire_ignition_end_time3 = fire_ignition_end_time3
      this%fire_ignition_radius3 = fire_ignition_radius3

      this%fire_ignition_start_x4 = fire_ignition_start_x4
      this%fire_ignition_start_y4 = fire_ignition_start_y4
      this%fire_ignition_start_lon4 = fire_ignition_start_lon4
      this%fire_ignition_start_lat4 = fire_ignition_start_lat4
      this%fire_ignition_end_x4 = fire_ignition_end_x4
      this%fire_ignition_end_y4 = fire_ignition_end_y4
      this%fire_ignition_end_lon4 = fire_ignition_end_lon4
      this%fire_ignition_end_lat4 = fire_ignition_end_lat4
      this%fire_ignition_ros4 = fire_ignition_ros4
      this%fire_ignition_start_time4 = fire_ignition_start_time4
      this%fire_ignition_end_time4 = fire_ignition_end_time4
      this%fire_ignition_radius4 = fire_ignition_radius4

      this%fire_ignition_start_x5 = fire_ignition_start_x5
      this%fire_ignition_start_y5 = fire_ignition_start_y5
      this%fire_ignition_start_lon5 = fire_ignition_start_lon5
      this%fire_ignition_start_lat5 = fire_ignition_start_lat5
      this%fire_ignition_end_x5 = fire_ignition_end_x5
      this%fire_ignition_end_y5 = fire_ignition_end_y5
      this%fire_ignition_end_lon5 = fire_ignition_end_lon5
      this%fire_ignition_end_lat5 = fire_ignition_end_lat5
      this%fire_ignition_ros5 = fire_ignition_ros5
      this%fire_ignition_start_time5 = fire_ignition_start_time5
      this%fire_ignition_end_time5 = fire_ignition_end_time5
      this%fire_ignition_radius5 = fire_ignition_radius5

      select type (this)
        type is (namelist_fire_t)
          ! we are good 

        class is (namelist_t)
          this%restart = restart
          this%cen_lat = cen_lat
          this%cen_lon = cen_lon

          this%ids = ids
          this%jds = jds
          this%kds = kds
          this%ide = ide
          this%jde = jde
          this%kde = kde

          this%sr_x = sr_x
          this%sr_y = sr_y

        class default
          write (ERROR_UNIT, *) 'Unknown type for namelist_fire_t'
          stop
      end select

      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Leaving subroutine Read_namelist'

    end subroutine Init_namelist

  end module namelist_mod
