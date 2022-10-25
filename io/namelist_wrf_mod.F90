  module namelist_wrf_mod

    implicit none

    private

    public :: grid_config_rec_type, NUM_FMC

    integer, parameter :: NUM_FMC = 5

    type :: grid_config_rec_type
        ! Atmosphere
      logical :: restart = .false.
      real :: cen_lat = 0.0 ! "center latitude"      "degrees, negative is south"
      real :: cen_lon = 0.0 ! "central longitude"      "degrees, negative is west"

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
!    contains
!      procedure, public :: Print => Print_config_flags
    end type grid_config_rec_type

  end module namelist_wrf_mod
