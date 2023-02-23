  module state_mod

    use namelist_mod, only : namelist_t, NUM_FMC
    use geogrid_mod, only : geogrid_t
    use proj_lc_mod, only : proj_lc_t
    use datetime_mod, only : datetime_t
    use netcdf_mod, only : Create_netcdf_file, Add_netcdf_dim, Add_netcdf_var
    use wrf_mod, only : wrf_t, G, RERADIUS
    use constants_mod, only : PI

    implicit none

    private

    public :: domain, P_FIRE_SMOKE, NUM_TRACER

    integer, parameter :: NUM_TRACER = 1, NUM_FMEP = 2, P_FIRE_SMOKE = 1
    integer, parameter :: N_POINTS_IN_HALO = 5

    type :: state_fire_t
!      real, dimension (:, :), allocatable :: lats, lons, elevations, dz_dxs, dz_dys, fuel_cats
      real :: dx = 200.0 , dy = 200.0
      real :: dt = 2.0              ! "TEMPORAL RESOLUTION"      "SECONDS"
      integer :: itimestep = 0
      type (datetime_t) :: datetime_start, datetime_end, datetime_now, datetime_next_output, datetime_next_atm_update

      real, dimension(:, :), allocatable :: uf ! W-E winds used in fire module
      real, dimension(:, :), allocatable :: vf ! S-N winds used in fire module
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
      real, dimension(:, :), allocatable :: lats   ! "latitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lons   ! "longitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lats_c ! "latitude of corners of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lons_c ! "longitude of corners of fire cells" "degrees"
      real, dimension(:, :), allocatable :: fxlong ! "longitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: fxlat ! "latitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: fz0 ! "roughness length of fire cells" "m"
      real, dimension(:, :), allocatable :: nfuel_cat ! "fuel data"
      real, dimension(:, :), allocatable :: fuel_time ! "fuel"
      real, dimension(:, :), allocatable :: emis_smoke

        ! New vars defined on fire grid for NUOPC coupling
      real, dimension(:, :), allocatable :: fire_psfc       ! "Surface Pressure"  "Pa"
      real, dimension(:, :), allocatable :: fire_rain       ! "Accumulated total rain"  "mm"
      real, dimension(:, :), allocatable :: fire_t2         ! "TEMP at 2 M"       "K"
      real, dimension(:, :), allocatable :: fire_q2         ! "Value of 2m specific humidity" "kg/kg"
      real, dimension(:, :), allocatable :: fire_rh_fire    ! "relative humidity, diagnostics" ""
      real, dimension(:, :), allocatable :: fire_psfc_old   ! "Surface Pressure, previous value"  "Pa"
      real, dimension(:, :), allocatable :: fire_rain_old   ! "Accumulated total rain, previous value"  "mm"
      real, dimension(:, :), allocatable :: fire_t2_old     ! "TEMP at 2 M, previous value"       "K"
      real, dimension(:, :), allocatable :: fire_q2_old     ! "Value of 2m specific humidity, previous value" "kg/kg"
      real, dimension(:, :, :), allocatable :: fire_u3d     ! W-E winds used in fire module 3D
      real, dimension(:, :, :), allocatable :: fire_v3d     ! S-N winds used in fire module 3D
      real, dimension(:, :, :), allocatable :: fire_ph      ! "geopotential levels"  "m2 s-2"
      real, dimension(:, :, :), allocatable :: fire_pres    ! "pressure levels"  "Pa"

        ! FMC model
      real, dimension(:, :, :), allocatable :: fmc_gc ! "fuel moisture contents by class" "1"
      real, dimension(:, :, :), allocatable :: fmep  ! "fuel moisture extended model parameters" "1"
      real, dimension(:, :, :), allocatable :: fmc_equi ! "fuel moisture contents by class equilibrium (diagnostics only)" "1"
      real, dimension(:, :, :), allocatable :: fmc_lag ! "fuel moisture contents by class time lag (diagnostics only)" "h"

      real :: fmoist_lasttime       ! "last time the moisture model was run" "s"
      real :: fmoist_nexttime       ! "next time the moisture model will run" "s"
      real :: u_frame               ! "FRAME X WIND"         "m s-1"
      real :: v_frame               ! "FRAME Y WIND"         "m s-1"
      real :: unit_fxlong, unit_fxlat
      integer :: fire_ignition_longlat
      integer :: nx ! "number of longitudinal grid points" "1"
      integer :: ny ! "number of latitudinal grid points" "1"
      real :: cen_lat, cen_lon
    contains
      procedure, public :: Handle_output => Handle_output
      procedure, public :: Save_state => Save_state
    end type state_fire_t

    type, extends (state_fire_t) :: domain
      integer :: ifds, ifde, jfds, jfde, kfds, kfde, ifms, ifme, jfms, jfme, kfms, kfme, &
                 ifps, ifpe, jfps, jfpe, kfps, kfpe, ifts, ifte, jfts, jfte, kfts, kfte
      integer :: sr_x = 0, sr_y = 0
      real :: dxf, dyf

    contains
      procedure, public :: Handle_wrfdata_update => Handle_wrfdata_update
      procedure, public :: Initialization => Init_domain
      procedure, public :: Init_latlons_fire => Init_latlons_fire
      procedure, public :: Interpolate_vars_atm_to_fire => Interpolate_vars_atm_to_fire
      procedure, public :: Print => Print_domain
      procedure, public :: Provide_atm_feedback => Provide_atm_feedback
      procedure :: sum_2d_fire_vars => Sum_2d_fire_vars
    end type domain

  contains

    subroutine calc_smoke_emissions(         &
           grid,config_flags,                &
           ifts,ifte,jfts,jfte)

      implicit none

      type (domain), intent(in out) :: grid   ! data
      type (namelist_t), intent(in) :: config_flags
      integer, intent(in) :: ifts,ifte,jfts,jfte

      integer::i_f,j_f


      do j_f=jfts,jfte
        do i_f=ifts,ifte
          grid%emis_smoke(i_f,j_f)=config_flags%fire_tracer_smoke*grid%burnt_area_dt(i_f,j_f)*grid%fgip(i_f,j_f)  ! kg/m^2
        enddo
      enddo

    end subroutine calc_smoke_emissions

    subroutine calc_unit_fxlat_fxlong (grid, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT

      implicit none

      type (domain), intent(in out) :: grid
      type (namelist_t), intent(in) :: config_flags

      logical :: real,ideal


      ideal=config_flags%fire_ignition_start_x1 .ne.0. .or. config_flags%fire_ignition_start_y1 .ne. 0.
      real=config_flags%fire_ignition_start_lon1 .ne. 0. .or. config_flags%fire_ignition_start_lat1 .ne. 0.
      if(ideal)grid%fire_ignition_longlat = 0

      if (ideal) write (OUTPUT_UNIT, *) 'Using ideal ignition coordinates, m from the lower left domain corner'
      if (real) grid%fire_ignition_longlat = 1
      if (real) write (OUTPUT_UNIT, *) 'Using real ignition coordinates, longitude and latitude'
      if (ideal.and.real) write (ERROR_UNIT, *) 'Only one of the ideal or real coordinates may be given'

      if(grid%fire_ignition_longlat .eq. 0)then
           ! ideal
           !  ignition is in m
        grid%unit_fxlong=1.
        grid%unit_fxlat=1.
           ! will set fire mesh coordinates to uniform mesh below
      else
           ! real
           ! 1 degree in m (approximate OK)
        grid%unit_fxlat = 2.0 * PI / (360.0 * RERADIUS)  ! earth circumference in m / 360 degrees
        grid%unit_fxlong = cos (grid%cen_lat * 2.0 * PI / 360.0) * grid%unit_fxlat  ! latitude
      endif

    end subroutine calc_unit_fxlat_fxlong

    subroutine Handle_output (this, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags

      logical, parameter :: DEBUG_LOCAL = .true.


      if (this%datetime_now == this%datetime_next_output) then
        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Writing output...'
        call this%Save_state ()
        call this%datetime_next_output%Add_seconds (config_flags%interval_output)
      end if

    end subroutine Handle_output

    subroutine Handle_wrfdata_update (this, wrf, config_flags, testcase)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (domain), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      type (wrf_t), intent (in out) :: wrf
      logical, intent (in), optional :: testcase

      logical, parameter :: DEBUG_LOCAL = .true.
      integer :: i, j, k


      If_update_atm: if (this%datetime_now == this%datetime_next_atm_update) then
        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Updating wrfdata...'
        if (DEBUG_LOCAL) call this%datetime_now%Print_datetime ()

        if (.not. present(testcase)) call wrf%Update_atm_state (this%datetime_now)

        call wrf%interpolate_wind2fire(config_flags,                            & ! flag for debug output
                config_flags%fire_wind_height,                                  & ! height to interpolate to
                wrf%ids,wrf%ide-1, wrf%kds,wrf%kde, wrf%jds,wrf%jde-1,          &
                wrf%ims,wrf%ime, wrf%kms,wrf%kme, wrf%jms,wrf%jme,              &
                wrf%ips,min(wrf%ipe,wrf%ide-1), wrf%jps,min(wrf%jpe,wrf%jde-1), &
                wrf%i_start(1),min(wrf%i_end(1),wrf%ide-1),                     &
                wrf%j_start(1),min(wrf%j_end(1),wrf%jde-1),                     &
                this%ifds,this%ifde-this%sr_x, this%jfds,this%jfde-this%sr_y,   &
                this%ifms, this%ifme, this%jfms, this%jfme,                     &
                this%ifts, this%ifte, this%jfts, this%jfte,   &
                this%sr_x,this%sr_y,                          & ! atm/fire this ratio
                wrf%u3d_stag,wrf%v3d_stag,                    & ! 3D atm this arrays in
                wrf%ph_stag,wrf%phb_stag,                     &
                wrf%z0_stag,                                  & ! 2D atm this arrays in
                this%uf,this%vf,this%fz0)                       ! fire this arrays out

        call this%interpolate_vars_atm_to_fire(wrf)

        call this%datetime_next_atm_update%Add_seconds (config_flags%interval_atm)

      end if If_update_atm

    end subroutine Handle_wrfdata_update

    subroutine Init_domain (this, config_flags, geogrid)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
      implicit none

      class (domain), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      type (geogrid_t), intent (in), optional :: geogrid

      integer :: ims0, ime0, jms0, jme0, ids0, ide0, jds0, jde0
      real, parameter :: DEFAULT_ZSF = 0.0, DEFAULT_DZDXF = 0.0, DEFAULT_DZDYF = 0.0
      logical :: use_geogrid


      if (present (geogrid)) then
        use_geogrid = .true.
      else
        use_geogrid = .false.
      end if

        ! Domain dimensions
      if (use_geogrid) then
        ids0 = geogrid%ifds
        ide0 = geogrid%ifde
        jds0 = geogrid%jfds
        jde0 = geogrid%jfde

        this%ifds = ids0
        this%ifde = ide0
        this%ifms = ids0 - N_POINTS_IN_HALO * config_flags%sr_x
        this%ifme = ide0 + N_POINTS_IN_HALO * config_flags%sr_x
        this%ifps = ids0
        this%ifpe = ide0
        this%ifts = ids0
        this%ifte = ide0

        this%jfds = jds0
        this%jfde = jde0
        this%jfms = jds0 - N_POINTS_IN_HALO * config_flags%sr_y
        this%jfme = jde0 + N_POINTS_IN_HALO * config_flags%sr_y
        this%jfps = jds0
        this%jfpe = jde0
        this%jfts = jds0
        this%jfte = jde0
      else
        ids0 = config_flags%ids
        ide0 = config_flags%ide
        jds0 = config_flags%jds
        jde0 = config_flags%jde

        ims0 = ids0 - N_POINTS_IN_HALO
        ime0 = ide0 + N_POINTS_IN_HALO
        jms0 = jds0 - N_POINTS_IN_HALO
        jme0 = jde0 + N_POINTS_IN_HALO

        this%ifds = config_flags%ids
        this%ifde = config_flags%ide * config_flags%sr_x
        this%ifms = (ims0 - 1) * config_flags%sr_x + 1
        this%ifme = ime0 * config_flags%sr_x
        this%ifps = (config_flags%ids - 1) * config_flags%sr_x + 1
        this%ifpe = config_flags%ide * config_flags%sr_x
        this%ifts = (config_flags%ids - 1) * config_flags%sr_x + 1
        this%ifte = (config_flags%ide - config_flags%ids + 1) * config_flags%sr_x + config_flags%ids - 1

        this%jfds = config_flags%jds
        this%jfde = config_flags%jde * config_flags%sr_y
        this%jfms = (jms0 - 1) * config_flags%sr_y + 1
        this%jfme = jme0 * config_flags%sr_y
        this%jfps = (config_flags%jds - 1) * config_flags%sr_y + 1
        this%jfpe = config_flags%jde * config_flags%sr_y
        this%jfts = (config_flags%jds - 1) * config_flags%sr_y + 1
        this%jfte = (config_flags%jde - config_flags%jds + 1) * config_flags%sr_y + config_flags%jds - 1
      end if

      this%kfds = config_flags%kds
      this%kfde = config_flags%kde
      this%kfms = config_flags%kds
      this%kfme = config_flags%kde
      this%kfps = config_flags%kds
      this%kfpe = config_flags%kde
      this%kfts = config_flags%kds
      this%kfte = config_flags%kde

        ! Datetimes
      this%datetime_start = datetime_t (config_flags%start_year, config_flags%start_month, config_flags%start_day, &
          config_flags%start_hour, config_flags%start_minute, config_flags%start_second)
      this%datetime_end = datetime_t (config_flags%end_year, config_flags%end_month, config_flags%end_day, &
          config_flags%end_hour, config_flags%end_minute, config_flags%end_second)
      this%datetime_now = this%datetime_start

      this%datetime_next_output = this%datetime_start
      call this%datetime_next_output%Add_seconds (config_flags%interval_output)

      this%datetime_next_atm_update = this%datetime_start

        ! Grid dimensions
      if_geogrid: if (use_geogrid) then
        this%cen_lat = geogrid%cen_lat
        this%cen_lon = geogrid%cen_lon
        if (geogrid%dx == config_flags%dx) then
          this%dx = geogrid%dx
        else
          write (ERROR_UNIT, *) 'dx in namelist and in geogrid differ'
          stop
        end if
        if (geogrid%dy == config_flags%dy) then
          this%dy = geogrid%dy
        else
          write (ERROR_UNIT, *) 'dy in namelist and in geogrid differ'
          stop
        end if
        if (geogrid%sr_x == config_flags%sr_x) then
          this%sr_x = geogrid%sr_x
          this%dxf = config_flags%dx / this%sr_x
        else
          write (ERROR_UNIT, *) 'sr_x in namelist and in geogrid differ'
          stop
        end if
        if (geogrid%sr_y == config_flags%sr_y) then
          this%sr_y = geogrid%sr_y
          this%dyf = config_flags%dy / this%sr_y
        else
          write (ERROR_UNIT, *) 'sr_y in namelist and in geogrid differ'
          stop
        end if
      else
        ! we need to initialize nx, ny here
        this%dx = config_flags%dx
        this%dy = config_flags%dy
        this%dt = config_flags%dt
        this%sr_x = config_flags%sr_x
        this%sr_y = config_flags%sr_y
        this%dxf = config_flags%dx / this%sr_x
        this%dyf = config_flags%dy / this%sr_y
      end if if_geogrid

      call this%Print()

      this%nx = this%ifde
      this%ny = this%jfde

      if (use_geogrid) then
        call Init_latlons_fire (this, geogrid%cen_lat, geogrid%cen_lon, geogrid%stand_lon, &
                           geogrid%true_lat_1, geogrid%true_lat_2)
        ! what if it is not geogrid
      end if

      allocate (this%uf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%vf(this%ifms:this%ifme, this%jfms:this%jfme))
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
      allocate (this%fuel_time(this%ifms:this%ifme, this%jfms:this%jfme))

      allocate (this%fire_psfc(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fire_rain(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fire_t2(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fire_q2(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fire_rh_fire(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fire_psfc_old(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fire_rain_old(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fire_t2_old(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fire_q2_old(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fire_u3d(this%ifms:this%ifme, this%jfms:this%jfme, this%kfms:this%kfme))
      allocate (this%fire_v3d(this%ifms:this%ifme, this%jfms:this%jfme, this%kfms:this%kfme))
      allocate (this%fire_ph(this%ifms:this%ifme, this%jfms:this%jfme, this%kfms:this%kfme))
      allocate (this%fire_pres(this%ifms:this%ifme, this%jfms:this%jfme, this%kfms:this%kfme))

      allocate (this%fmc_gc(this%ifms:this%ifme, NUM_FMC, this%jfms:this%jfme))
      allocate (this%fmc_equi(this%ifms:this%ifme, NUM_FMC, this%jfms:this%jfme))
      allocate (this%fmc_lag(this%ifms:this%ifme, NUM_FMC, this%jfms:this%jfme))
      allocate (this%fmep(this%ifms:this%ifme, NUM_FMEP, this%jfms:this%jfme))

      this%dt = config_flags%dt

      allocate (this%zsf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%dzdxf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%dzdyf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%nfuel_cat(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%emis_smoke(this%ifms:this%ifme, this%jfms:this%jfme))
      this%emis_smoke = 0.0

      if_geogrid2d: if (use_geogrid) then
        this%zsf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%elevations
        this%dzdxf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%dz_dxs
        this%dzdyf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%dz_dys
        this%nfuel_cat(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%fuel_cats
      else
        this%zsf = DEFAULT_ZSF
        this%dzdxf = DEFAULT_DZDXF
        this%dzdyf = DEFAULT_DZDYF
      end if if_geogrid2d

      call calc_unit_fxlat_fxlong (this, config_flags)

    end subroutine Init_domain

    subroutine Init_latlons_fire (this, cen_lat, cen_lon, standard_lon, &
                           true_lat_1, true_lat_2)

      implicit none

      class (domain), intent (in out) :: this
      real, intent(in) :: cen_lat, cen_lon, standard_lon, &
                          true_lat_1, true_lat_2
        ! Local
      type (proj_lc_t) :: proj
      integer :: i, j


      allocate (this%lons(this%nx, this%ny))
      allocate (this%lats(this%nx, this%ny))
      allocate (this%lons_c(this%nx + 1, this%ny + 1))
      allocate (this%lats_c(this%nx + 1, this%ny + 1))

      proj = proj_lc_t (cen_lat = cen_lat , cen_lon = cen_lon, dx = this%dxf, dy = this%dyf, &
          standard_lon = standard_lon , true_lat_1 = true_lat_1 , true_lat_2 = true_lat_2 , &
          nx = this%nx, ny = this%ny)

      do j = 1, this%ny
        do i = 1, this%nx
          call proj%Calc_latlon (i = real (i), j = real (j), lat = this%lats(i, j), lon = this%lons(i, j))
          call proj%Calc_latlon (i = real (i) - 0.5, j = real (j) - 0.5, lat = this%lats_c(i, j), lon = this%lons_c(i, j))
        end do
      end do

      do j = 1, this%ny
        call proj%Calc_latlon (i = real (this%nx) + 0.5, j = real (j) - 0.5, lat = this%lats_c(this%nx + 1, j), &
            lon = this%lons_c(this%nx + 1, j))
      end do

      do i = 1, this%nx
        call proj%Calc_latlon (i = real (i) - 0.5, j = real (this%ny) + 0.5, lat = this%lats_c(i, this%ny + 1), &
            lon = this%lons_c(i, this%ny + 1))
      end do

      call proj%Calc_latlon (i = real (this%nx) + 0.5, j = real (this%ny) + 0.5, lat = this%lats_c(this%nx + 1, this%ny + 1), &
          lon = this%lons_c(this%nx + 1, this%ny + 1))

    end subroutine Init_latlons_fire

    subroutine Interpolate_vars_atm_to_fire (this, wrf)

        implicit none

        class (domain), intent(in out) :: this          ! fire state
        type (wrf_t), intent(in) :: wrf                 ! atm state

        If_testcase: if (this%datetime_now == this%datetime_start) then

          if (this%fire_ignition_longlat == 0) then
              ! set ideal fire mesh coordinates - used for ignition only
              ! do not forget to set unit_fxlong, unit_fxlat outside of parallel loop

              ! DME: Next call added to fixe a bug when
              ! initializing a nested domain (before, fxlong & fxlat where not
              ! assigned, they were supposed to be set by a mod in WPS
              ! but here we use standard WPS therefore were zero and fire does not ignite)
            call set_ideal_coord( this%dxf,this%dyf, &
                  this%ifds,this%ifde,this%jfds,this%jfde,  &
                  this%ifms,this%ifme,this%jfms,this%jfme,  &
                  this%ifts,this%ifte,this%jfts,this%jfte,  &
                  this%fxlong,this%fxlat)
          else
              ! assume halo xlong xlat
              ! interpolate nodal coordinates
            call wrf%interpolate_z2fire(                    &
                this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
                this%ifms, this%ifme, this%jfms, this%jfme,  &
                this%ifts,this%ifte,this%jfts,this%jfte,     &
                this%sr_x,this%sr_y,                         & ! atm/fire this ratio
                wrf%xlat,                                    &
                this%fxlat,0)

            call wrf%interpolate_z2fire(                    &
                this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
                this%ifms, this%ifme, this%jfms, this%jfme,  &
                this%ifts,this%ifte,this%jfts,this%jfte,     &
                this%sr_x,this%sr_y,                         & ! atm/fire this ratio
                wrf%xlong,                                   &
                this%fxlong,0)
          end if

        call wrf%interpolate_z2fire(                    &
            this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
            this%ifms, this%ifme, this%jfms, this%jfme,  &
            this%ifts,this%ifte,this%jfts,this%jfte,     &
            this%sr_x,this%sr_y,                         & ! atm/fire this ratio
            wrf%z0_stag,                                 &
            this%fz0,1)

        endif If_testcase

        call wrf%interpolate_z2fire(                    &
            this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
            this%ifms, this%ifme, this%jfms, this%jfme,  &
            this%ifts,this%ifte,this%jfts,this%jfte,     &
            this%sr_x,this%sr_y,                         & ! atm/fire this ratio
            wrf%t2_stag,                                 &
            this%fire_t2,1)

         call wrf%interpolate_z2fire(                    &
            this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
            this%ifms, this%ifme, this%jfms, this%jfme,  &
            this%ifts,this%ifte,this%jfts,this%jfte,     &
            this%sr_x,this%sr_y,                         & ! atm/fire this ratio
            wrf%q2_stag,                                 &
            this%fire_q2,1)

         call wrf%interpolate_z2fire(                    &
            this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
            this%ifms, this%ifme, this%jfms, this%jfme,  &
            this%ifts,this%ifte,this%jfts,this%jfte,     &
            this%sr_x,this%sr_y,                         & ! atm/fire this ratio
            wrf%psfc_stag,                                 &
            this%fire_psfc,1)

         call wrf%interpolate_z2fire(                    &
            this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
            this%ifms, this%ifme, this%jfms, this%jfme,  &
            this%ifts,this%ifte,this%jfts,this%jfte,     &
            this%sr_x,this%sr_y,                         & ! atm/fire this ratio
            wrf%rainc_stag+wrf%rainnc_stag,                                 &
            this%fire_rain,1)

    end subroutine Interpolate_vars_atm_to_fire

    subroutine Print_domain (this)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (domain), intent(in out) :: this


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

      write (OUTPUT_UNIT, *) 'ifts = ', this%ifts, 'ifte = ', this%ifte
      write (OUTPUT_UNIT, *) 'jfts = ', this%jfts, 'jfte = ', this%jfte
      write (OUTPUT_UNIT, *) 'kfts = ', this%kfts, 'kfte = ', this%kfte

      write (OUTPUT_UNIT, *) ''

    end subroutine Print_domain

    subroutine Provide_atm_feedback (this, wrf, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (domain), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      type (wrf_t), intent (in out) :: wrf

      integer :: ij,its,ite,jts,jte,kts,kte            ! atm tile

      call this%sum_2d_fire_vars (wrf, config_flags)

        ! --- add heat and moisture fluxes to tendency variables by postulated decay
      do ij=1,wrf%num_tiles
        ! FIRE works on domain by 1 smaller, in last row&col winds are not set properly
        its = wrf%i_start(ij)             ! start atmospheric tile in i
        ite = min(wrf%i_end(ij),wrf%ide-1)    ! end atmospheric tile in i
        jts = wrf%j_start(ij)             ! start atmospheric tile in j
        jte = min(wrf%j_end(ij),wrf%jde-1)    ! end atmospheric tile in j
        kts = wrf%kds
        kte = wrf%kde

        call wrf%fire_tendency(                 &
             wrf%ids,wrf%ide-1, wrf%kds,wrf%kde, wrf%jds,wrf%jde-1, & ! domain dimensions
             wrf%ims,wrf%ime, wrf%kms,wrf%kme, wrf%jms,wrf%jme,     &
             wrf%its,wrf%ite, wrf%kts,wrf%kte, wrf%jts,wrf%jte,     & !
             wrf%grnhfx,wrf%grnqfx,wrf%canhfx,wrf%canqfx,           & ! fluxes on atm wrf
             config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt, &
             wrf%z_at_w_stag,wrf%dz8w_stag,wrf%mut_stag,wrf%c1h,wrf%c2h,wrf%rho_stag,  &
             wrf%rthfrten,wrf%rqvfrten)                ! out
      enddo

      if (config_flags%tracer_opt.eq.3) then
        call calc_smoke_emissions(this,config_flags, &
               this%ifts,this%ifte,this%jfts,this%jfte)

        call wrf%add_fire_tracer_emissions(                        &
                  this%ifms,this%ifme,this%jfms,this%jfme,         &
                  this%ifps,this%ifpe,this%jfps,this%jfpe,         &
                  wrf%ids,wrf%ide,wrf%kds,wrf%kde,wrf%jds,wrf%jde, &
                  wrf%ims,wrf%ime,wrf%kms,wrf%kme,wrf%jms,wrf%jme, &
                  wrf%ips,wrf%ipe,wrf%kps,wrf%kpe,wrf%jps,wrf%jpe, &
                  wrf%rho_stag,wrf%dz8w_stag,                      &
                  this%burnt_area_dt,this%fgip,                    &
                  wrf%tracer(:,:,:,p_fire_smoke),this%emis_smoke)
      end if

    end subroutine Provide_atm_feedback

    subroutine Save_state (this)

      implicit none

      class (state_fire_t), intent (in) :: this

      character (len = :), allocatable :: file_output
      integer :: nz


      file_output='fire_output_'//this%datetime_now%datetime//'.nc'

      call Create_netcdf_file (file_name = file_output)

      call Add_netcdf_dim (file_output, 'nx', this%nx)
      call Add_netcdf_dim (file_output, 'ny', this%ny)

      if (allocated (this%fire_u3d)) then
        nz = size (this%fire_u3d, dim = 3)
        call Add_netcdf_dim (file_output, 'nz', nz - 1)
      else
        nz = 0
      end if

      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fxlat', this%fxlat(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fxlong', this%fxlong(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fgrnhfx', this%fgrnhfx(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fire_area', this%fire_area(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'emis_smoke', this%emis_smoke(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fire_t2', this%fire_t2(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fire_q2', this%fire_q2(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fire_psfc', this%fire_psfc(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fire_rain', this%fire_rain(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fz0', this%fz0(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fmc_g', this%fmc_g(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'uf', this%uf(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'vf', this%vf(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'zsf', this%zsf(1:this%nx, 1:this%ny))
      if (nz > 0) then
        call Add_netcdf_var (file_output, ['nx', 'ny', 'nz'], 'fire_u3d', this%fire_u3d(1:this%nx, 1:this%ny, 1:nz - 1))
        call Add_netcdf_var (file_output, ['nx', 'ny', 'nz'], 'fire_v3d', this%fire_v3d(1:this%nx, 1:this%ny, 1:nz - 1))
        call Add_netcdf_var (file_output, ['nx', 'ny', 'nz'], 'fire_ph', this%fire_ph(1:this%nx, 1:this%ny, 1:nz - 1))
        call Add_netcdf_var (file_output, ['nx', 'ny', 'nz'], 'fire_pres', this%fire_pres(1:this%nx, 1:this%ny, 1:nz - 1))
      end if

    end subroutine Save_state

    subroutine set_ideal_coord(dxf,dyf,   &
                    ifds,ifde,jfds,jfde,  &
                    ifms,ifme,jfms,jfme,  &
                    ifts,ifte,jfts,jfte,  &
                    fxlong,fxlat)

    implicit none

    real, intent(in) :: dxf,dyf
    integer, intent(in) :: &
                    ifds,ifde,jfds,jfde,  &
                    ifms,ifme,jfms,jfme,  &
                    ifts,ifte,jfts,jfte
    real, intent(out), dimension(ifms:ifme,jfms:jfme) :: fxlong,fxlat

    integer::i,j


        ! could we not just as easily get something that
        ! that looks like a lat/lon
        ! set fake  coordinates, in m
      do j=jfts,jfte
          do i=ifts,ifte
              ! uniform mesh, lower left domain corner is (0,0)
              fxlong(i,j)=(i-ifds+0.5)*dxf
              fxlat (i,j)=(j-jfds+0.5)*dyf
          enddo
      enddo

    end subroutine set_ideal_coord

    subroutine sum_2d_cells(       &
           ims2,ime2,jms2,jme2,    &
           its2,ite2,jts2,jte2,    &
           v2,                     &  ! input
           ims1,ime1,jms1,jme1,    &
           its1,ite1,jts1,jte1,    &
           v1)                        ! output

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT

      implicit none

      !*** purpose
      ! sum cell values in mesh2 to cell values of coarser mesh1

      !*** arguments
      ! the dimensions are in cells, not nodes!

      integer, intent(in)::its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
      real, intent(out)::v1(ims1:ime1,jms1:jme1)
      integer, intent(in)::its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
      real, intent(in)::v2(ims2:ime2,jms2:jme2)

      integer:: i1,i2,j1,j2,ir,jr,isz1,isz2,jsz1,jsz2,ioff,joff,ibase,jbase
      real t


      !check mesh dimensions and domain dimensions
!      call check_mesh_2dim(its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1)
!      call check_mesh_2dim(its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2)

      ! compute mesh sizes
      isz1 = ite1-its1+1
      jsz1 = jte1-jts1+1
      isz2 = ite2-its2+1
      jsz2 = jte2-jts2+1

      ! check mesh sizes
      if (isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0) then
        write (ERROR_UNIT, *) 'all mesh sizes must be positive'
      endif

      ! compute mesh ratios
      ir=isz2/isz1
      jr=jsz2/jsz1

      if(isz2.ne.isz1*ir .or. jsz2.ne.jsz1*jr)then
        write (ERROR_UNIT, *) 'input mesh size must be multiple of output mesh size'
      endif

      ! v1 = sum(v2)
      do j1=jts1,jte1
          jbase=jts2+jr*(j1-jts1)
          do i1=its1,ite1
             ibase=its2+ir*(i1-its1)
             t=0.
             do joff=0,jr-1
                 j2=joff+jbase
                 do ioff=0,ir-1
                     i2=ioff+ibase
                     t=t+v2(i2,j2)
                 enddo
             enddo
             v1(i1,j1)=t
          enddo
      enddo

      return

!      9 continue
      !$OMP CRITICAL(FIRE_UTIL_CRIT)
!      write(msg,91)its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
!      call message(msg)
!      write(msg,91)its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
!      call message(msg)
!      write(msg,92)'input  mesh size:',isz2,jsz2
!      call message(msg)
!      91 format('dimensions: ',8i8)
!      write(msg,92)'output mesh size:',isz1,jsz1
!      call message(msg)
!      92 format(a,2i8)
      !$OMP END CRITICAL(FIRE_UTIL_CRIT)
!      call crash('module_fr_spread_util:sum_mesh_cells: bad mesh sizes')

    end subroutine sum_2d_cells

    subroutine sum_2d_fire_vars (this, atm, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (domain), intent(in out) :: this          ! fire state
      type (wrf_t), intent(in out) :: atm                 ! atm state
      type (namelist_t), intent(in) :: config_flags   ! namelist

      real :: s
      integer :: i, j

      ! sum the fluxes over atm cells
      call sum_2d_cells(                              &
            this%ifms,this%ifme, this%jfms,this%jfme, &
            this%ifts,this%ifte, this%jfts,this%jfte, &
            this%fuel_frac,                           &
            atm%ims,atm%ime, atm%jms,atm%jme,         &
            atm%its,atm%ite, atm%jts,atm%jte,         &
            atm%avg_fuel_frac)
      call sum_2d_cells(                              &
            this%ifms,this%ifme, this%jfms,this%jfme, &
            this%ifts,this%ifte, this%jfts,this%jfte, &
            this%fgrnhfx,                             &
            atm%ims,atm%ime, atm%jms,atm%jme,         &
            atm%its,atm%ite, atm%jts,atm%jte,         &
            atm%grnhfx)
      call sum_2d_cells(                              &
            this%ifms,this%ifme, this%jfms,this%jfme, &
            this%ifts,this%ifte, this%jfts,this%jfte, &
            this%fgrnqfx,                             &
            atm%ims,atm%ime, atm%jms,atm%jme,         &
            atm%its,atm%ite, atm%jts,atm%jte,         &
            atm%grnqfx)

      write (OUTPUT_UNIT, '(a,f6.3)') 'fire-atmosphere feedback scaling ', config_flags%fire_atm_feedback

      s = 1./(this%sr_x*this%sr_y)
      do j=atm%jts,atm%jte
        do i=atm%its,atm%ite
          ! DME heat fluxes contribution for the case wiythout feedback
          atm%grnhfx_fu(i,j)=atm%grnhfx(i,j)*s
          atm%grnqfx_fu(i,j)=atm%grnqfx(i,j)*s
          ! scale surface fluxes to get the averages
          atm%avg_fuel_frac(i,j)=atm%avg_fuel_frac(i,j)*s
          atm%grnhfx(i,j)=config_flags%fire_atm_feedback*atm%grnhfx(i,j)*s
          atm%grnqfx(i,j)=config_flags%fire_atm_feedback*atm%grnqfx(i,j)*s
          ! we do not have canopy fluxes yet...
          atm%canhfx(i,j)=0
          atm%canqfx(i,j)=0
        enddo
      enddo

    end subroutine sum_2d_fire_vars

  end module state_mod

