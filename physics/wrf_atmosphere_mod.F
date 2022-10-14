  module wrf_atmosphere_mod

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

    implicit none

    private

    public :: CP, XLV, RERADIUS, G, PI2, Wrf_error_fatal, Wrf_message, domain, grid_config_rec_type, NUM_TRACER, P_FIRE_SMOKE, &
        Get_ijk_from_subgrid, Wrf_debug

    real, parameter :: R_D = 287.0                ! gas constant of dry air (J deg^-1 kg^-1)
    real, parameter :: CP = 7.0 * R_D / 2.0
    real, parameter :: RERADIUS = 1.0 / 6370.0e03 ! reciprocal of earth radius (m^-1)
    real, parameter :: XLV = 2.5E6                ! latent heat of vaporization of water at 0^oC (J kg^-1)
    real, parameter :: G = 9.81                   ! acceleration due to gravity (m {s}^-2)
    real, parameter :: PI2 = 2.0 * 3.1415926

    integer, parameter :: NUM_TRACER = 1, NUM_FMEP = 2, NUM_FMC = 5, P_FIRE_SMOKE = 1
    integer, parameter :: N_POINTS_IN_HALO = 5

    type :: domain
      integer :: ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe
      integer :: ifds, ifde, jfds, jfde, kfds, kfde, ifms, ifme, jfms, jfme, kfms, kfme, &
                 ifps, ifpe, jfps, jfpe, kfps, kfpe
        ! Atmosphere
        ! 4D
      real, dimension(:, :, :, :), allocatable :: tracer
        ! 3D
      real, dimension(:, :, :), allocatable :: ph_2   ! "perturbation geopotential"  "m2 s-2"
      real, dimension(:, :, :), allocatable :: phb    ! "base-state geopotential"  "m2 s-2"
      real, dimension(:, :, :), allocatable :: u_2    ! "x-wind component"   "m s-1"
      real, dimension(:, :, :), allocatable :: v_2    ! "x-wind component"   "m s-1"
      real, dimension(:, :, :), allocatable :: rho    ! "DENSITY"         "Kg m-3"
      real, dimension(:, :, :), allocatable :: z_at_w ! Height agl at walls "m"  ??
      real, dimension(:, :, :), allocatable :: dz8w   ! Distance between vertical layers "m"
        ! 2D
      real, dimension(:, :), allocatable :: z0       ! "Background ROUGHNESS LENGTH" "m"
      real, dimension(:, :), allocatable :: ht       ! "Terrain Height"   "m"
      real, dimension(:, :), allocatable :: xlat     ! "LATITUDE, SOUTH IS NEGATIVE"   "degree_north"
      real, dimension(:, :), allocatable :: xlong    ! "LONGITUDE, WEST IS NEGATIVE" "degree_east"
      real, dimension(:, :), allocatable :: rainc    ! "ACCUMULATED TOTAL CUMULUS PRECIPITATION" "mm"
      real, dimension(:, :), allocatable :: rainnc   ! "ACCUMULATED TOTAL GRID SCALE PRECIPITATION" "mm"
      real, dimension(:, :), allocatable :: t2       ! "TEMP at 2 M"       "K"
      real, dimension(:, :), allocatable :: q2       ! "QV at 2 M"         "kg kg-1"
      real, dimension(:, :), allocatable :: psfc     ! "SFC PRESSURE"      "Pa"
      real, dimension(:, :), allocatable :: mut
        ! 1D
      real, dimension(:), allocatable :: c1h ! "half levels, c1h = d bf / d eta, using znw"        "Dimensionless"
      real, dimension(:), allocatable :: c2h ! "half levels, c2h = (1-c1h)*(p0-pt)"                "Pa"

        ! Fire vars in the atm grid
        ! 1) Atm vars
      real, dimension(:, :), allocatable :: rain_old ! "previous value of accumulated rain" "mm"
      real, dimension(:, :), allocatable :: t2_old   ! "previous value of air temperature at 2m" "K"
      real, dimension(:, :), allocatable :: q2_old   ! "previous value of 2m specific humidity" "kg/kg"
      real, dimension(:, :), allocatable :: psfc_old ! "previous value of surface pressure" "Pa"
      real, dimension(:, :), allocatable :: rh_fire  ! "relative humidity at the surface" "1"
        ! 2) Fire diag vars
      real, dimension(:, :), allocatable :: avg_fuel_frac ! "fuel remaining averaged to atmospheric grid" "1"
      real, dimension(:, :), allocatable :: grnhfx ! "heat flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: grnqfx ! "moisture flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: canhfx ! "heat flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: canqfx ! "moisture flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: grnhfx_fu ! "heat flux from ground fire (feedback unsensitive)" "W/m^2"
      real, dimension(:, :), allocatable :: grnqfx_fu ! "moisture flux from ground fire (feedback unsensitive)" "W/m^2"
      real, dimension(:, :), allocatable :: uah, vah ! "wind at fire_wind_height" "m/s"
        ! 3) Output vars with feedback to the atmospheric model
      real, dimension (:, :, :), allocatable :: rthfrten ! "temperature tendency" "K/s"
      real, dimension (:, :, :), allocatable :: rqvfrten ! "RQVFRTEN" "humidity tendency" Stagger in z
        ! 4) 3D vars for FMC model
      real, dimension(:, :, :), allocatable :: fmc_gc ! "fuel moisture contents by class" "1"
      real, dimension(:, :, :), allocatable :: fmep  ! "fuel moisture extended model parameters" "1"
      real, dimension(:, :, :), allocatable :: fmc_equi ! "fuel moisture contents by class equilibrium (diagnostics only)" "1"
      real, dimension(:, :, :), allocatable :: fmc_lag ! "fuel moisture contents by class time lag (diagnostics only)" "h"

        ! Fire vars in fire grid
        ! 2D
      real, dimension(:, :), allocatable :: uf ! W-E winds used in fire module
      real, dimension(:, :), allocatable :: vf ! W-E winds used in fire module
      real, dimension(:, :), allocatable :: zsf    ! terrain height
      real, dimension(:, :), allocatable :: dzdxf  ! terrain grad
      real, dimension(:, :), allocatable :: dzdyf  ! terrain grad
      real, dimension(:, :), allocatable :: bbb    ! ta rate of spread formula coeff
      real, dimension(:, :), allocatable :: betafl ! a rate of spread formula variable 
      real, dimension(:, :), allocatable :: phiwc  ! a rate of spread formula coeff 
      real, dimension(:, :), allocatable :: r_0    ! a rate of spread formula variable
      real, dimension(:, :), allocatable :: fgip   ! a rate of spread formula coeff 
      real, dimension(:, :), allocatable :: ischap ! a rate of spread formula switch
      real, dimension(:, :), allocatable :: iboros ! Ib divided by ROS
      real, dimension(:, :), allocatable :: fmc_g  ! fuel moisture, ground
      real, dimension(:, :), allocatable :: lfn    ! "level function" "1"
      real, dimension(:, :), allocatable :: lfn_hist ! "level function history" "1"
      real, dimension(:, :), allocatable :: lfn_0 ! "level function for time integration, step 0" "1"
      real, dimension(:, :), allocatable :: lfn_1 ! "level function for time integration, step 1" "1"
      real, dimension(:, :), allocatable :: lfn_2 ! "level function for time integration, step 2" "1"
      real, dimension(:, :), allocatable :: lfn_s0 ! "level set sign function from LSM integration" "1"
      real, dimension(:, :), allocatable :: lfn_s1 ! "level set function for reinitialization integration" "1"
      real, dimension(:, :), allocatable :: lfn_s2 ! "level set function for reinitialization integration" "1"
      real, dimension(:, :), allocatable :: lfn_s3 ! "level set function for reinitialization integration" "1"
      real, dimension(:, :), allocatable :: flame_length ! "fire flame length" "m"
      real, dimension(:, :), allocatable :: ros_front ! "rate of spread at fire front" "m/s"
      real, dimension(:, :), allocatable :: tign_g ! "ignition time on ground" "s"
      real, dimension(:, :), allocatable :: fuel_frac ! "fuel remaining" "1"
      real, dimension(:, :), allocatable :: fire_area ! "fraction of cell area on fire" "1"
      real, dimension(:, :), allocatable :: burnt_area_dt ! "fraction of cell area burnt on current dt" "-"
      real, dimension(:, :), allocatable :: fgrnhfx ! "heat flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: fgrnqfx ! "moisture flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: fcanhfx ! "heat flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: fcanqfx ! "moisture flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: ros ! "rate of spread" "m/s"
      real, dimension(:, :), allocatable :: fxlong ! "longitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: fxlat ! "latitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: fz0 ! "roughness length of fire cells" "m"
      real, dimension(:, :), allocatable :: nfuel_cat ! "fuel data"
      real, dimension(:, :), allocatable :: fuel_time ! "fuel"
  
      integer :: itimestep
      integer :: num_tiles
      integer, dimension (:), allocatable :: i_start, i_end, j_start, j_end
      real :: dt = 2.0              ! "TEMPORAL RESOLUTION"      "SECONDS"
      real :: dx = 200.0            ! "X HORIZONTAL RESOLUTION"   "METERS"
      real :: dy = 200.0            ! "Y HORIZONTAL RESOLUTION"   "METERS"
      integer :: sr_x = 0, sr_y = 0 ! Refinement ratios
      real :: fmoist_lasttime       ! "last time the moisture model was run" "s"
      real :: fmoist_nexttime       ! "next time the moisture model will run" "s"
      real :: u_frame               ! "FRAME X WIND"         "m s-1"
      real :: v_frame               ! "FRAME Y WIND"         "m s-1"
    contains
      procedure, public :: Print => Print_domain
    end type domain

    interface domain
      module procedure Domain_const
    end interface domain

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
    contains
      procedure, public :: Print => Print_config_flags
    end type grid_config_rec_type

  contains

    function Domain_const (ids, ide, kds, kde, jds, jde, sr_x, sr_y) result (return_value)

      implicit none

      integer, intent (in) :: ids, ide, kds, kde, jds, jde, sr_x, sr_y
      type (domain) :: return_value

      call Domain_init (return_value, ids, ide, kds, kde, jds, jde, sr_x, sr_y)

    end function Domain_const

    subroutine Get_ijk_from_subgrid (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0    )

    ! return the values for subgrid whose refinement is in grid%sr
    ! note when using this routine, it does not affect K. For K 
    ! (vertical), it just returns what get_ijk_from_grid does

      type (domain), intent (in) :: grid
      integer, intent(out) ::                                 &
                       ids0, ide0, jds0, jde0, kds0, kde0,    &
                       ims0, ime0, jms0, jme0, kms0, kme0,    &
                       ips0, ipe0, jps0, jpe0, kps0, kpe0
        ! Local
      integer ::                              &
                ids, ide, jds, jde, kds, kde, &
                ims, ime, jms, jme, kms, kme, &
                ips, ipe, jps, jpe, kps, kpe


      ids0 = grid%ids
      ide0 = grid%ide * grid%sr_x
      ims0 = (grid%ims - 1) * grid%sr_x + 1
      ime0 = grid%ime * grid%sr_x
      ips0 = (grid%ips - 1) * grid%sr_x + 1
      ipe0 = grid%ipe * grid%sr_x

      jds0 = grid%jds
      jde0 = grid%jde * grid%sr_y
      jms0 = (grid%jms - 1) * grid%sr_y + 1
      jme0 = grid%jme * grid%sr_y
      jps0 = (grid%jps - 1) * grid%sr_y + 1
      jpe0 = grid%jpe * grid%sr_y

      kds0 = grid%kds
      kde0 = grid%kde
      kms0 = grid%kms
      kme0 = grid%kme
      kps0 = grid%kps
      kpe0 = grid%kpe

      return

    end subroutine Get_ijk_from_subgrid

    subroutine Print_config_flags (this)

      implicit none

      class (grid_config_rec_type), intent(in out) :: this


      write (OUTPUT_UNIT, *) 'restart = ', this%restart

    end subroutine Print_config_flags

    subroutine Print_domain (this)

      implicit none

      class (domain), intent(in out) :: this


      write (OUTPUT_UNIT, *) ''
      write (OUTPUT_UNIT, *) 'ids = ', this%ids, 'ide = ', this%ide
      write (OUTPUT_UNIT, *) 'jds = ', this%jds, 'jde = ', this%jde
      write (OUTPUT_UNIT, *) 'kds = ', this%kds, 'kde = ', this%kde

      write (OUTPUT_UNIT, *) 'ims = ', this%ims, 'ime = ', this%ime
      write (OUTPUT_UNIT, *) 'jms = ', this%jms, 'jme = ', this%jme
      write (OUTPUT_UNIT, *) 'kms = ', this%kms, 'kme = ', this%kme

      write (OUTPUT_UNIT, *) 'ips = ', this%ips, 'ipe = ', this%ipe
      write (OUTPUT_UNIT, *) 'jps = ', this%jps, 'jpe = ', this%jpe
      write (OUTPUT_UNIT, *) 'kps = ', this%kps, 'kpe = ', this%kpe


      write (OUTPUT_UNIT, *) ''
      write (OUTPUT_UNIT, *) 'sr_x = ', this%sr_x
      write (OUTPUT_UNIT, *) 'sr_y = ', this%sr_y

      write (OUTPUT_UNIT, *) ''
      write (OUTPUT_UNIT, *) 'ifds = ', this%ifds, 'ifde = ', this%ifde
      write (OUTPUT_UNIT, *) 'jfds = ', this%jfds, 'jfde = ', this%jfde
      write (OUTPUT_UNIT, *) 'kfds = ', this%kfds, 'kfde = ', this%kfde

      write (OUTPUT_UNIT, *) 'ifms = ', this%ifms, 'ifme = ', this%ifme
      write (OUTPUT_UNIT, *) 'jfms = ', this%jfms, 'jfme = ', this%jfme
      write (OUTPUT_UNIT, *) 'kfms = ', this%kfms, 'kfme = ', this%kfme

      write (OUTPUT_UNIT, *) 'ifps = ', this%ifps, 'ifpe = ', this%ifpe
      write (OUTPUT_UNIT, *) 'jfps = ', this%jfps, 'jfpe = ', this%jfpe
      write (OUTPUT_UNIT, *) 'kfps = ', this%kfps, 'kfpe = ', this%kfpe

      write (OUTPUT_UNIT, *) ''
      write (OUTPUT_UNIT, *) 'shape ph_2 = ', shape (this%ph_2)
      write (OUTPUT_UNIT, *) 'shape phb = ', shape (this%phb)
      write (OUTPUT_UNIT, *) 'shape u_2 = ', shape (this%u_2)
      write (OUTPUT_UNIT, *) 'shape v_2 = ', shape (this%v_2)
      write (OUTPUT_UNIT, *) 'shape i_start = ', shape (this%i_start)

    end subroutine Print_domain

    subroutine Domain_init (this, ids, ide, kds, kde, jds, jde, sr_x, sr_y)

      implicit none

      class (domain), intent(in out) :: this
      integer, intent (in) :: ids, ide, kds, kde, jds, jde, sr_x, sr_y


        ! Fill in atm dims including
        ! domain decomposition
      this%ids = ids
      this%ide = ide
      this%kds = kds
      this%kde = kde
      this%jds = jds
      this%jde = jde

      this%ims = ids - N_POINTS_IN_HALO
      this%ime = ide + N_POINTS_IN_HALO
      this%kms = kds
      this%kme = kde
      this%jms = jds - N_POINTS_IN_HALO
      this%jme = jde + N_POINTS_IN_HALO

      this%ips = ids
      this%ipe = ide
      this%kps = kds
      this%kpe = kde
      this%jps = jds
      this%jpe = jde

      this%num_tiles = 1
      allocate (this%i_start(this%num_tiles))
      this%i_start = this%ids 
      allocate (this%i_end(this%num_tiles))
      this%i_end = this%ide
      allocate (this%j_start(this%num_tiles))
      this%j_start = this%jds 
      allocate (this%j_end(this%num_tiles))
      this%j_end = this%jde

        ! Atmosphere vars
      allocate (this%tracer(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme, NUM_TRACER))

      allocate (this%ph_2(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))
      allocate (this%phb(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))
      allocate (this%u_2(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))
      allocate (this%v_2(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))
      allocate (this%rho(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))
      allocate (this%z_at_w(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))
      allocate (this%dz8w(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))

      allocate (this%z0(this%ims:this%ime, this%jms:this%jme))
      allocate (this%ht(this%ims:this%ime, this%jms:this%jme))
      allocate (this%xlat(this%ims:this%ime, this%jms:this%jme))
      allocate (this%xlong(this%ims:this%ime, this%jms:this%jme))
      allocate (this%rainc(this%ims:this%ime, this%jms:this%jme))
      allocate (this%rainnc(this%ims:this%ime, this%jms:this%jme))
      allocate (this%t2(this%ims:this%ime, this%jms:this%jme))
      allocate (this%q2(this%ims:this%ime, this%jms:this%jme))
      allocate (this%psfc(this%ims:this%ime, this%jms:this%jme))
      allocate (this%mut(this%ims:this%ime, this%jms:this%jme))

      allocate (this%c1h(this%kms:this%kme))
      allocate (this%c2h(this%kms:this%kme))

        ! Fire vars
      allocate (this%rain_old(this%ims:this%ime, this%jms:this%jme))
      allocate (this%t2_old(this%ims:this%ime, this%jms:this%jme))
      allocate (this%q2_old(this%ims:this%ime, this%jms:this%jme))
      allocate (this%psfc_old(this%ims:this%ime, this%jms:this%jme))
      allocate (this%rh_fire(this%ims:this%ime, this%jms:this%jme))

      allocate (this%avg_fuel_frac(this%ims:this%ime, this%jms:this%jme))
      allocate (this%grnhfx(this%ims:this%ime, this%jms:this%jme))
      allocate (this%grnqfx(this%ims:this%ime, this%jms:this%jme))
      allocate (this%canhfx(this%ims:this%ime, this%jms:this%jme))
      allocate (this%canqfx(this%ims:this%ime, this%jms:this%jme))
      allocate (this%grnhfx_fu(this%ims:this%ime, this%jms:this%jme))
      allocate (this%grnqfx_fu(this%ims:this%ime, this%jms:this%jme))
      allocate (this%uah(this%ims:this%ime, this%jms:this%jme))
      allocate (this%vah(this%ims:this%ime, this%jms:this%jme))

      allocate (this%rthfrten(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))
      allocate (this%rqvfrten(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))

      allocate (this%fmc_gc(this%ims:this%ime, NUM_FMC, this%jms:this%jme))
      allocate (this%fmc_equi(this%ims:this%ime, NUM_FMC, this%jms:this%jme))
      allocate (this%fmc_lag(this%ims:this%ime, NUM_FMC, this%jms:this%jme))
      allocate (this%fmep(this%ims:this%ime, NUM_FMEP, this%jms:this%jme))

        ! Fire vars in the fire grid
      this%sr_x = sr_x
      this%sr_y = sr_y

      call Get_ijk_from_subgrid (this, this%ifds, this%ifde, this%jfds, this%jfde, this%kfds, this%kfde, &
          this%ifms, this%ifme, this%jfms, this%jfme, this%kfms, this%kfme, this%ifps, this%ifpe, this%jfps, &
          this%jfpe, this%kfps, this%kfpe)

      allocate (this%uf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%vf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%zsf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%dzdxf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%dzdyf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%bbb(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%betafl(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%phiwc(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%r_0(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fgip(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%ischap(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%iboros(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fmc_g(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_hist(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_0(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_1(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_2(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_s0(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_s1(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_s2(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_s3(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%flame_length(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%ros_front(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%tign_g(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fuel_frac(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fire_area(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%burnt_area_dt(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fgrnhfx(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fgrnqfx(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fcanhfx(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fcanqfx(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%ros(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fxlong(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fxlat(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fz0(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%nfuel_cat(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fuel_time(this%ifms:this%ifme, this%jfms:this%jfme))

    end subroutine Domain_init

    subroutine Wrf_debug (level , str)

      implicit none

      character (len = *) :: str
      integer, intent (in) :: level


      call wrf_message (str)

      return

    end subroutine wrf_debug

    subroutine wrf_error_fatal( str )

      implicit none

      character (len = *) :: str


      call Wrf_error_fatal3 (' ', 0, str)

    end subroutine wrf_error_fatal

    subroutine Wrf_error_fatal3 (file_str, line, str)

      implicit none

      character (len = *) :: file_str
      integer, intent (in) :: line  ! only print file and line if line > 0
      character (len = *) :: str

      character (len = 256) :: line_str


      write (line_str,'(i6)') line

        ! Fatal errors are printed to stdout and stderr regardless of
        ! any &logging namelist settings.
      call Wrf_message ('-------------- FATAL CALLED ---------------')
        ! only print file and line if line is positive
      if ( line > 0 ) then
        call wrf_message ('FATAL CALLED FROM FILE:  ' // file_str // '  LINE:  ' // trim (line_str))
      end if
      call Wrf_message (str)
      call Wrf_message ('-------------------------------------------')

      stop

    end subroutine wrf_error_fatal3

    subroutine Wrf_message (str)

      implicit none

      character (len = *) :: str

 300  format (A)
      print 300, trim(str)

    end subroutine wrf_message

  end module wrf_atmosphere_mod
