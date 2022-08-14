  module wrf_atmosphere_mod

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

    implicit none

    private

    public :: CP, XLV, RERADIUS, G, PI2, wrf_error_fatal, wrf_message, domain, grid_config_rec_type, NUM_TRACER, P_FIRE_SMOKE, &
        get_ijk_from_subgrid

    real, parameter :: R_D = 287.0                ! gas constant of dry air (J deg^-1 kg^-1)
    real, parameter :: CP = 7.0 * R_D / 2.0
    real, parameter :: RERADIUS = 1.0 / 6370.0e03 ! reciprocal of earth radius (m^-1)
    real, parameter :: XLV = 2.5E6                ! latent heat of vaporization of water at 0^oC (J kg^-1)
    real, parameter :: G = 9.81                   ! acceleration due to gravity (m {s}^-2)
    real, parameter :: PI2=2.*3.1415926


    integer, parameter :: NUM_TRACER = 1, P_FIRE_SMOKE = 1, DEFAULT_DOMAIN_SIZE = 50, DEFAULT_DOMAIN_SIZE_FIRE = 50
    integer, parameter :: ifms = 1, ifme = DEFAULT_DOMAIN_SIZE_FIRE, jfms = 1, jfme = DEFAULT_DOMAIN_SIZE_FIRE
    integer, parameter :: ims = 1, ime = DEFAULT_DOMAIN_SIZE, kms = 1, kme = DEFAULT_DOMAIN_SIZE, &
        jms = 1, jme = DEFAULT_DOMAIN_SIZE
    integer, parameter :: NUM_FMC = 2 ! hardcoded here but it seems it is cotrolled by namelist setting
    integer, parameter :: NUM_FMEP = 2 ! constant

    integer, parameter :: NUM_TILES_PAJ = 1

    type :: domain
       ! Atmosphere
      real, dimension(ims:ime, kms:kme, jms:jme, NUM_TRACER) :: tracer
      real, dimension(ims:ime, kms:kme, jms:jme) :: ph_2  ! "perturbation geopotential"  "m2 s-2"
      real, dimension(ims:ime, kms:kme, jms:jme) :: phb   ! "base-state geopotential"  "m2 s-2"
      real, dimension(ims:ime, kms:kme, jms:jme) :: u_2   ! "x-wind component"   "m s-1"
      real, dimension(ims:ime, kms:kme, jms:jme) :: v_2   ! "y-wind component"   "m s-1"
      real, dimension(ims:ime, jms:jme) :: z0             ! "Background ROUGHNESS LENGTH" "m"
      real, dimension(ims:ime, jms:jme) :: ht             ! "Terrain Height"   "m"
      real, dimension(ims:ime, jms:jme) :: xlat           ! "LATITUDE, SOUTH IS NEGATIVE"   "degree_north"
      real, dimension(ims:ime, jms:jme) :: xlong          ! "LONGITUDE, WEST IS NEGATIVE" "degree_east"
      real, dimension(ims:ime, jms:jme) :: rainc          ! "ACCUMULATED TOTAL CUMULUS PRECIPITATION" "mm"
      real, dimension(ims:ime, jms:jme) :: rainnc         ! "ACCUMULATED TOTAL GRID SCALE PRECIPITATION" "mm"
      real, dimension(ims:ime, jms:jme) :: t2             ! "TEMP at 2 M"       "K"
      real, dimension(ims:ime, jms:jme) :: q2             ! "QV at 2 M"         "kg kg-1"
      real, dimension(ims:ime, jms:jme) :: psfc           ! "SFC PRESSURE"      "Pa"
      real, dimension(ims:ime, jms:jme) :: mut

      real, dimension(kms:kme) :: c1h ! "half levels, c1h = d bf / d eta, using znw"        "Dimensionless"
      real, dimension(kms:kme) :: c2h ! "half levels, c2h = (1-c1h)*(p0-pt)"                "Pa"

      integer :: num_tiles

        ! Fire var in atm grid
      real, dimension(ims:ime, jms:jme) :: rain_old       ! "previous value of accumulated rain" "mm"
      real, dimension(ims:ime, jms:jme) :: t2_old         ! "previous value of air temperature at 2m" "K"
      real, dimension(ims:ime, jms:jme) :: q2_old         ! "previous value of 2m specific humidity" "kg/kg"
      real, dimension(ims:ime, jms:jme) :: psfc_old       ! "previous value of surface pressure" "Pa"
      real, dimension(ims:ime, jms:jme) :: rh_fire        ! "relative humidity at the surface" "1"

        ! Fire fmc model
      real, dimension(ims:ime, NUM_FMC, jms:jme) :: fmc_gc ! "fuel moisture contents by class" "1"
      real, dimension(ims:ime, NUM_FMEP, jms:jme) :: fmep  ! "fuel moisture extended model parameters" "1"
      real, dimension(ims:ime, NUM_FMC, jms:jme) :: fmc_equi ! "fuel moisture contents by class equilibrium (diagnostics only)" "1"
      real, dimension(ims:ime, NUM_FMC, jms:jme) :: fmc_lag ! "fuel moisture contents by class time lag (diagnostics only)" "h"
  
        ! Fire var in fire grid
      real, dimension(ifms:ifme, jfms:jfme) :: uf     ! W-E winds used in fire module
      real, dimension(ifms:ifme, jfms:jfme) :: vf     ! W-E winds used in fire module
      real, dimension(ifms:ifme, jfms:jfme) :: zsf    ! terrain height
      real, dimension(ifms:ifme, jfms:jfme) :: dzdxf  ! terrain grad
      real, dimension(ifms:ifme, jfms:jfme) :: dzdyf  ! terrain grad
      real, dimension(ifms:ifme, jfms:jfme) :: bbb    ! ta rate of spread formula coeff
      real, dimension(ifms:ifme, jfms:jfme) :: betafl ! a rate of spread formula variable 
      real, dimension(ifms:ifme, jfms:jfme) :: phiwc  ! a rate of spread formula coeff 
      real, dimension(ifms:ifme, jfms:jfme) :: r_0    ! a rate of spread formula variable
      real, dimension(ifms:ifme, jfms:jfme) :: fgip   ! a rate of spread formula coeff 
      real, dimension(ifms:ifme, jfms:jfme) :: ischap ! a rate of spread formula switch
      real, dimension(ifms:ifme, jfms:jfme) :: iboros ! Ib divided by ROS
      real, dimension(ifms:ifme, jfms:jfme) :: fmc_g  ! fuel moisture, ground
      real, dimension(ifms:ifme, jfms:jfme) :: lfn    ! "level function" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: lfn_hist ! "level function history" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: lfn_0 ! "level function for time integration, step 0" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: lfn_1 ! "level function for time integration, step 1" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: lfn_2 ! "level function for time integration, step 2" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: lfn_s0 ! "level set sign function from LSM integration" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: lfn_s1 ! "level set function for reinitialization integration" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: lfn_s2 ! "level set function for reinitialization integration" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: lfn_s3 ! "level set function for reinitialization integration" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: flame_length ! "fire flame length" "m"
      real, dimension(ifms:ifme, jfms:jfme) :: ros_front ! "rate of spread at fire front" "m/s"
      real, dimension(ifms:ifme, jfms:jfme) :: tign_g ! "ignition time on ground" "s"
      real, dimension(ifms:ifme, jfms:jfme) :: fuel_frac ! "fuel remaining" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: fire_area ! "fraction of cell area on fire" "1"
      real, dimension(ifms:ifme, jfms:jfme) :: burnt_area_dt ! "fraction of cell area burnt on current dt" "-"
      real, dimension(ifms:ifme, jfms:jfme) :: fgrnhfx ! "heat flux from ground fire" "W/m^2"
      real, dimension(ifms:ifme, jfms:jfme) :: fgrnqfx ! "moisture flux from ground fire" "W/m^2"
      real, dimension(ifms:ifme, jfms:jfme) :: fcanhfx ! "heat flux from crown fire" "W/m^2"
      real, dimension(ifms:ifme, jfms:jfme) :: fcanqfx ! "moisture flux from crown fire" "W/m^2"
      real, dimension(ifms:ifme, jfms:jfme) :: ros ! "rate of spread" "m/s"
      real, dimension(ifms:ifme, jfms:jfme) :: fxlong ! "longitude of midpoints of fire cells" "degrees"
      real, dimension(ifms:ifme, jfms:jfme) :: fxlat ! "latitude of midpoints of fire cells" "degrees"
      real, dimension(ifms:ifme, jfms:jfme) :: fz0 ! "roughness length of fire cells" "m"
      real, dimension(ifms:ifme, jfms:jfme) :: nfuel_cat ! "fuel data"
      real, dimension(ifms:ifme, jfms:jfme) :: fuel_time ! "fuel"

        ! Diagnostic only
      real, dimension(ims:ime, jms:jme) :: avg_fuel_frac ! "fuel remaining averaged to atmospheric grid" "1"
      real, dimension(ims:ime, jms:jme) :: grnhfx ! "heat flux from ground fire" "W/m^2"
      real, dimension(ims:ime, jms:jme) :: grnqfx ! "moisture flux from ground fire" "W/m^2"
      real, dimension(ims:ime, jms:jme) :: canhfx ! "heat flux from crown fire" "W/m^2"
      real, dimension(ims:ime, jms:jme) :: canqfx ! "moisture flux from crown fire" "W/m^2"
      real, dimension(ims:ime, jms:jme) :: grnhfx_fu ! "heat flux from ground fire (feedback unsensitive)" "W/m^2"
      real, dimension(ims:ime, jms:jme) :: grnqfx_fu ! "moisture flux from ground fire (feedback unsensitive)" "W/m^2"
      real, dimension(ims:ime, jms:jme) :: uah, vah ! "wind at fire_wind_height" "m/s"

        ! Output to atmospheric model
      real, dimension (ims:ime, kms:kme, jms:jme) :: rthfrten ! "temperature tendency" "K/s"
      real, dimension (ims:ime, kms:kme, jms:jme) :: rqvfrten ! "RQVFRTEN" "humidity tendency" Stagger in z

      integer :: sr_x, sr_y ! Refinement ratios
      integer :: itimestep
      integer, dimension (NUM_TILES_PAJ) :: i_start, i_end, j_start, j_end

      real :: dt ! "TEMPORAL RESOLUTION"      "SECONDS"
      real :: dx ! "X HORIZONTAL RESOLUTION"   "METERS"
      real :: dy ! "Y HORIZONTAL RESOLUTION"   "METERS"
      real :: fmoist_lasttime ! "last time the moisture model was run" "s"
      real :: fmoist_nexttime ! "next time the moisture model will run" "s"
      real :: u_frame          ! "FRAME X WIND"         "m s-1"
      real :: v_frame          ! "FRAME Y WIND"         "m s-1"
    contains
      procedure, public :: Set_default => Set_default_domain
      procedure, public :: Print => Print_domain
    end type domain

    type :: grid_config_rec_type
        ! Atmosphere
      logical :: restart
      real :: cen_lat ! "center latitude"      "degrees, negative is south"
      real :: cen_lon ! "central longitude"      "degrees, negative is west"

        ! Fire
      integer :: fire_print_msg ! "write fire statistics, 0 no writes, 1+ for more"  ""
      integer :: fire_print_file ! "write fire output text files, 0 no writes, 1+ for more" ""
      integer :: fire_fuel_left_method ! "submesh to compute fuel lwft, even, at least 2" ""
      integer :: fire_fuel_left_irl ! "submesh to compute fuel lwft, even, at least 2" ""
      integer :: fire_fuel_left_jrl ! "submesh to compute fuel lwft, even, at least 2" ""
      real :: fire_const_time ! "time from ignition to freeze fire, <0 never" "s"
      real :: fire_const_grnhfx ! "if both >=0, the amount of constant heat flux" "1"
      real :: fire_const_grnqfx ! "if both >=0, the amount of constant heat flux" "1"
      real :: fire_atm_feedback ! "the heat fluxes to the atmosphere are multiplied by this" "1"
      integer :: fire_boundary_guard ! "cells to stop when fire close to domain boundary"
      integer :: fire_grows_only ! "if >0 level set function cannot increase = fire can only grow" "1"
      integer :: fire_upwinding ! "upwind normal spread: 1=standard, 2=godunov, 3=eno, 4=sethian, 5=2nd-order, 6=WENO3, 7=WENO5, 8=hybrid WENO3/ENO1, 9=hybrid WENO5/ENO1" "1"
      integer :: fire_upwind_split ! "1=upwind advection separately from normal direction spread" "1"
      real :: fire_viscosity ! "artificial viscosity in level set method" "1"
      real :: fire_lfn_ext_up ! "0.=extend level set function at boundary by reflection, 1.=always up" "1"
      integer :: fire_test_steps ! ">0 = on first call, do specified number of steps and terminate (testing only)" "1"
      integer :: fire_advection ! "0 = fire spread computed from normal wind speed/slope, 1 = fireline particle speed projected on normal" "0"
      logical :: fire_lsm_reinit ! "flag to activate reinitialization of level set method"
      integer :: fire_lsm_reinit_iter ! "number of iterations for the reinitialization PDE"
      integer :: fire_upwinding_reinit ! "numerical scheme (space) for reinitialization PDE: 1=WENO3, 2=WENO5, 3=hybrid WENO3-ENO1, 4=hybrid WENO5-ENO1"
      integer :: fire_lsm_band_ngp ! "number of grid points around lfn=0 that WENO5/3 is used (ENO1 elsewhere), for fire_upwinding_reinit=4,5 and fire_upwinding=8,9 options"
      logical :: fire_lsm_zcoupling ! "flag to activate reference velocity at a different height from fire_wind_height"
      real :: fire_lsm_zcoupling_ref ! "reference height from wich u at fire_wind_hegiht is calculated using a logarithmic profile" "m"
      real :: fire_viscosity_bg ! "artificial viscosity in the near-front region" "1"
      real :: fire_viscosity_band ! "number of times the hybrid advection band to transition from fire_viscosity_bg to fire_viscosity" "1"
      integer :: fire_viscosity_ngp ! "number of grid points around lfn=0 where low artificial viscosity is used = fire_viscosity_bg"
      real :: fire_slope_factor ! "slope correction factor" "-"

      integer :: fire_fmc_read ! "ground fuel moisture is set by: if 0, in wrfinput; if 1, user-presc; if 2, read from file" 
      logical :: fmoist_run     ! "run moisture model (on the atmospheric grid), output to fmc_gc"
      logical :: fmoist_interp  ! "interpolate moisture from the model or the input to fuels on the fire grid"
      logical :: fmoist_only    ! "only run moisture model, skip fire"
      integer :: fmoist_freq    ! "frequency to run moisture model 0: use fmoist_dt, k>0: every k timesteps" "1"
      real :: fmoist_dt         ! "moisture model time step" "s"
      integer :: fire_fuel_read ! "fuel categories are set by: if 0, uniform; if 1, user-presc; if 2, read from file"   ""
      integer :: fire_fuel_cat  ! "fuel category if ifuelread=0"              ""
      real :: fire_ext_grnd     ! "extinction depth of sfc fire heat"   "m"
      real :: fire_ext_crwn     ! "extinction depth of crown fire heat" "m"
      real :: fire_crwn_hgt     ! "height that heat from crown fire is released" "m"
      real :: fire_wind_height  ! "height of uah,vah wind in fire spread formula" "m"
      logical :: fire_is_real_perim ! .false. = point/line ignition, .true. = observed perimeter"
      integer :: nfmc ! "number of fuel moisture classes" related to NUM_NFMC
      real :: fmep_decay_tlag ! "time constant of assimilated adjustments of equilibria decay" "1"
      integer :: tracer_opt
      real :: fire_tracer_smoke ! "parts per unit of burned fuel becoming smoke (tracer_opt=3)" "g_smoke/kg_air"

        ! Ignitions
      integer :: fire_num_ignitions ! "number of ignition lines"

      real :: fire_ignition_start_x1 ! "x coord of start of ignition line" "m"
      real :: fire_ignition_start_y1 ! "y coord of start of ignition line" "m"
      real :: fire_ignition_start_lon1 ! "long coord of start of ignition line" "deg"
      real :: fire_ignition_start_lat1 ! "lat coord of start of ignition line" "deg"
      real :: fire_ignition_end_x1 ! "x coord of end of ignition line" "m"
      real :: fire_ignition_end_y1 ! "y coord of end of ignition line" "m"
      real :: fire_ignition_end_lon1 ! "long coord of end of ignition line" "deg"
      real :: fire_ignition_end_lat1 ! "lat coord of end of ignition line" "deg"
      real :: fire_ignition_ros1 ! "rate of spread during ignition" "m/s"
      real :: fire_ignition_start_time1 ! "ignition line start time" "s"
      real :: fire_ignition_end_time1 ! "ignition line end time" "s"
      real :: fire_ignition_radius1 ! "ignite all within the radius" "m"

      real :: fire_ignition_start_x2
      real :: fire_ignition_start_y2 
      real :: fire_ignition_start_lon2
      real :: fire_ignition_start_lat2 
      real :: fire_ignition_end_x2
      real :: fire_ignition_end_y2 
      real :: fire_ignition_end_lon2
      real :: fire_ignition_end_lat2
      real :: fire_ignition_ros2
      real :: fire_ignition_start_time2
      real :: fire_ignition_end_time2
      real :: fire_ignition_radius2

      real :: fire_ignition_start_x3
      real :: fire_ignition_start_y3
      real :: fire_ignition_start_lon3
      real :: fire_ignition_start_lat3
      real :: fire_ignition_end_x3
      real :: fire_ignition_end_y3
      real :: fire_ignition_end_lon3
      real :: fire_ignition_end_lat3
      real :: fire_ignition_ros3
      real :: fire_ignition_start_time3
      real :: fire_ignition_end_time3
      real :: fire_ignition_radius3

      real :: fire_ignition_start_x4
      real :: fire_ignition_start_y4
      real :: fire_ignition_start_lon4
      real :: fire_ignition_start_lat4
      real :: fire_ignition_end_x4
      real :: fire_ignition_end_y4
      real :: fire_ignition_end_lon4
      real :: fire_ignition_end_lat4
      real :: fire_ignition_ros4
      real :: fire_ignition_start_time4
      real :: fire_ignition_end_time4
      real :: fire_ignition_radius4

      real :: fire_ignition_start_x5
      real :: fire_ignition_start_y5
      real :: fire_ignition_start_lon5
      real :: fire_ignition_start_lat5
      real :: fire_ignition_end_x5
      real :: fire_ignition_end_y5
      real :: fire_ignition_end_lon5
      real :: fire_ignition_end_lat5
      real :: fire_ignition_ros5
      real :: fire_ignition_start_time5
      real :: fire_ignition_end_time5
      real :: fire_ignition_radius5
    contains
      procedure, public :: Set_default => Set_default_config_flags
      procedure, public :: Print => Print_config_flags
    end type grid_config_rec_type

  contains

    subroutine Get_ijk_from_grid (grid ,                   &
                          ids, ide, jds, jde, kds, kde,    &
                          ims, ime, jms, jme, kms, kme,    &
                          ips, ipe, jps, jpe, kps, kpe )

      implicit none

      type (domain), intent (in) :: grid
      integer, intent(out) ::                                 &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe


      print *, 'Subroutine get_ijk_from_grid underconstruction'
      stop

!      data_ordering : SELECT CASE ( model_data_order )
!         CASE  ( DATA_ORDER_XYZ )
!             ids = grid%sd31 ; ide = grid%ed31 ; jds = grid%sd32 ; jde = grid%ed32 ; kds = grid%sd33 ; kde = grid%ed33 ;
!             ims = grid%sm31 ; ime = grid%em31 ; jms = grid%sm32 ; jme = grid%em32 ; kms = grid%sm33 ; kme = grid%em33 ;
!             ips = grid%sp31 ; ipe = grid%ep31 ; jps = grid%sp32 ; jpe = grid%ep32 ; kps = grid%sp33 ; kpe = grid%ep33 ;
!         CASE  ( DATA_ORDER_YXZ )
!             ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd33  ; kde = grid%ed33  ;
!             ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm33  ; kme = grid%em33  ;
!             ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp33  ; kpe = grid%ep33  ;
!         CASE  ( DATA_ORDER_ZXY )
!             ids = grid%sd32  ; ide = grid%ed32  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd31  ; kde = grid%ed31  ;
!             ims = grid%sm32  ; ime = grid%em32  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm31  ; kme = grid%em31  ;
!             ips = grid%sp32  ; ipe = grid%ep32  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp31  ; kpe = grid%ep31  ;
!         CASE  ( DATA_ORDER_ZYX )
!             ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd32  ; jde = grid%ed32  ; kds = grid%sd31  ; kde = grid%ed31  ;
!             ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm32  ; jme = grid%em32  ; kms = grid%sm31  ; kme = grid%em31  ;
!             ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp32  ; jpe = grid%ep32  ; kps = grid%sp31  ; kpe = grid%ep31  ;
!         CASE  ( DATA_ORDER_XZY )
!             ids = grid%sd31  ; ide = grid%ed31  ; jds = grid%sd33  ; jde = grid%ed33  ; kds = grid%sd32  ; kde = grid%ed32  ;
!             ims = grid%sm31  ; ime = grid%em31  ; jms = grid%sm33  ; jme = grid%em33  ; kms = grid%sm32  ; kme = grid%em32  ;
!             ips = grid%sp31  ; ipe = grid%ep31  ; jps = grid%sp33  ; jpe = grid%ep33  ; kps = grid%sp32  ; kpe = grid%ep32  ;
!         CASE  ( DATA_ORDER_YZX )
!             ids = grid%sd33  ; ide = grid%ed33  ; jds = grid%sd31  ; jde = grid%ed31  ; kds = grid%sd32  ; kde = grid%ed32  ;
!             ims = grid%sm33  ; ime = grid%em33  ; jms = grid%sm31  ; jme = grid%em31  ; kms = grid%sm32  ; kme = grid%em32  ;
!             ips = grid%sp33  ; ipe = grid%ep33  ; jps = grid%sp31  ; jpe = grid%ep31  ; kps = grid%sp32  ; kpe = grid%ep32  ;
!      END SELECT data_ordering

    end subroutine Get_ijk_from_grid

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


      call Get_ijk_from_grid (grid ,                       &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, kpe)
      ids0 = ids
      ide0 = ide * grid%sr_x
      ims0 = (ims - 1)*grid%sr_x + 1
      ime0 = ime * grid%sr_x
      ips0 = (ips - 1)*grid%sr_x + 1
      ipe0 = ipe * grid%sr_x

      jds0 = jds
      jde0 = jde * grid%sr_y
      jms0 = (jms - 1) * grid%sr_y + 1
      jme0 = jme * grid%sr_y
      jps0 = (jps - 1) * grid%sr_y + 1
      jpe0 = jpe * grid%sr_y

      kds0 = kds
      kde0 = kde
      kms0 = kms
      kme0 = kme
      kps0 = kps
      kpe0 = kpe

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


      write (OUTPUT_UNIT, *) 'sr_x = ', this%sr_x
      write (OUTPUT_UNIT, *) 'sr_y = ', this%sr_y

    end subroutine Print_domain

    subroutine Set_default_config_flags (this)

      implicit none

      class (grid_config_rec_type), intent(in out) :: this


      this%restart = .false.

    end subroutine Set_default_config_flags

    subroutine Set_default_domain (this)

      implicit none

      class (domain), intent(in out) :: this


      this%sr_x = 2
      this%sr_y = 2

    end subroutine Set_default_domain

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
