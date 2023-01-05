  module state_mod

    use namelist_mod, only : namelist_t, NUM_FMC
    use geogrid_mod, only : geogrid_t
    use proj_lc_mod, only : proj_lc_t
    use datetime_mod, only : datetime_t
    use netcdf_mod, only : Create_netcdf_file, Add_netcdf_dim, Add_netcdf_var
    use wrf_mod, only : wrf_t

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
      real, dimension(:, :), allocatable :: lats   ! "latitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lons   ! "longitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lats_c ! "latitude of corners of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lons_c ! "longitude of corners of fire cells" "degrees"
      real, dimension(:, :), allocatable :: fxlong ! "longitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: fxlat ! "latitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: fz0 ! "roughness length of fire cells" "m"
      real, dimension(:, :), allocatable :: nfuel_cat ! "fuel data"
      real, dimension(:, :), allocatable :: fuel_time ! "fuel"
      real, dimension(:, :), allocatable :: avg_fuel_frac ! "fuel remaining averaged to atmospheric grid" "1"
      real, dimension(:, :), allocatable :: grnhfx ! "heat flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: grnqfx ! "moisture flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: canhfx ! "heat flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: canqfx ! "moisture flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: grnhfx_fu ! "heat flux from ground fire (feedback unsensitive)" "W/m^2"
      real, dimension(:, :), allocatable :: grnqfx_fu ! "moisture flux from ground fire (feedback unsensitive)" "W/m^2"
      real, dimension(:, :), allocatable :: uah, vah ! "wind at fire_wind_height" "m/s"
      real, dimension(:, :), allocatable :: emis_smoke
      real, dimension(:, :), allocatable :: fire_t2       ! "TEMP at 2 M"       "K"

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
    contains
      procedure, public :: Handle_output => Handle_output
      procedure, public :: Save_state => Save_state
    end type state_fire_t

    type, extends (state_fire_t) :: domain
      integer :: ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe, &
                 its, ite, jts, jte, kts, kte
      integer :: ifds, ifde, jfds, jfde, kfds, kfde, ifms, ifme, jfms, jfme, kfms, kfme, &
                 ifps, ifpe, jfps, jfpe, kfps, kfpe, ifts, ifte, jfts, jfte, kfts, kfte
      integer :: sr_x = 0, sr_y = 0
      real :: dxf, dyf
      real :: cen_lat, cen_lon
      integer :: num_tiles
      integer, dimension (:), allocatable :: i_start, i_end, j_start, j_end
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
        ! feedback to atm
      real, dimension(:), allocatable :: c1h ! "half levels, c1h = d bf / d eta, using znw"        "Dimensionless"
      real, dimension(:), allocatable :: c2h ! "half levels, c2h = (1-c1h)*(p0-pt)"                "Pa"
      real, dimension (:, :, :), allocatable :: rthfrten ! "temperature tendency" "K/s"
      real, dimension (:, :, :), allocatable :: rqvfrten ! "RQVFRTEN" "humidity tendency" Stagger in z

        ! Fire vars in the atm grid
      real, dimension(:, :), allocatable :: rain_old ! "previous value of accumulated rain" "mm"
      real, dimension(:, :), allocatable :: t2_old   ! "previous value of air temperature at 2m" "K"
      real, dimension(:, :), allocatable :: q2_old   ! "previous value of 2m specific humidity" "kg/kg"
      real, dimension(:, :), allocatable :: psfc_old ! "previous value of surface pressure" "Pa"
      real, dimension(:, :), allocatable :: rh_fire  ! "relative humidity at the surface" "1"
    contains
      procedure, public :: Handle_wrfdata_update => Handle_wrfdata_update
      procedure, public :: Initialization => Init_domain
      procedure, public :: Init_latlons_fire => Init_latlons_fire
      procedure, public :: Print => Print_domain
    end type domain

  contains

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

    subroutine Handle_wrfdata_update (this, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (domain), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags

      type (wrf_t) :: wrf
      logical, parameter :: DEBUG_LOCAL = .true.
      integer :: i, j, k


      if (this%datetime_now == this%datetime_next_atm_update) then
        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Updating wrfdata...'
        if (DEBUG_LOCAL) call this%datetime_now%Print_datetime ()

        wrf = wrf_t (file_name = 'wrf.nc')

          ! Update t2
        call wrf%Get_t2 (this%datetime_now)
        this%t2(this%ids:this%ide - 1, this%jds:this%jde - 1) = wrf%t2(:, :)
        call wrf%Destroy_t2 ()

          ! Update z0
        call wrf%Get_z0 (this%datetime_now)
        this%z0(this%ids:this%ide - 1, this%jds:this%jde - 1) = wrf%z0(:, :)
        call wrf%Destroy_z0 ()

          ! Update MUT
        call wrf%Get_mut (this%datetime_now)
        this%mut(this%ids:this%ide - 1, this%jds:this%jde - 1) = wrf%mut(:, :)
        call wrf%Destroy_mut ()

          ! Update U3D
        call wrf%Get_u3d_stag (this%datetime_now)
        do k = this%kds, this%kde - 1
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide
              this%u_2(i, k, j) = wrf%u3d(i, j, k)
            end do
          end do
        end do
        call wrf%Destroy_u3d ()

          ! Update V3D
        call wrf%Get_v3d_stag (this%datetime_now)
        do k = this%kds, this%kde - 1
          do j = this%jds, this%jde
            do i = this%ids, this%ide - 1
              this%v_2(i, k, j) = wrf%v3d(i, j, k)
            end do
          end do
        end do
        call wrf%Destroy_v3d ()

          ! PHB
        call wrf%Get_phb_stag (this%datetime_now)
        do k = this%kds, this%kde
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide - 1
              this%phb(i, k, j) = wrf%phb(i, j, k)
            end do
          end do
        end do
        call wrf%Destroy_phb ()

          ! PH
        call wrf%Get_ph_stag (this%datetime_now)
        do k = this%kds, this%kde
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide - 1
              this%ph_2(i, k, j) = wrf%ph(i, j, k)
            end do
          end do
        end do
        call wrf%Destroy_phb ()

          ! DZ8W
        call wrf%Get_dz8w (this%datetime_now)
        do k = this%kds, this%kde - 1
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide - 1
              this%dz8w(i, k, j) = wrf%dz8w(i, j, k)
            end do
          end do
        end do
        call wrf%Destroy_dz8w ()

          ! Z_AT_W
        call wrf%Get_z_at_w (this%datetime_now)
        do k = this%kds, this%kde
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide - 1
              this%z_at_w(i, k, j) = wrf%z_at_w(i, j, k)
            end do
          end do
        end do
        call wrf%Destroy_z_at_w ()

          ! RHO
        call wrf%Get_rho (this%datetime_now)
        do k = this%kds, this%kde - 1
          do j = this%jds, this%jde - 1
            do i = this%ids, this%ide - 1
              this%rho(i, k, j) = wrf%rho(i, j, k)
            end do
          end do
        end do
        call wrf%Destroy_rho ()

        call this%datetime_next_atm_update%Add_seconds (config_flags%interval_atm)

!print *, 'ABORTING in Handle_wrfdata_update until interpolation is implemented'
!stop
      end if

    end subroutine Handle_wrfdata_update

    subroutine Init_domain (this, config_flags, geogrid)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
      implicit none

      class (domain), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      type (geogrid_t), intent (in), optional :: geogrid

      real, parameter :: DEFAULT_Z0 = 0.1, DEFAULT_HT = 0.0, DEFAULT_ZSF = 0.0, DEFAULT_DZDXF = 0.0, &
          DEFAULT_DZDYF = 0.0, DEFAULT_C1H = 1.0, DEFAULT_C2H = 0.0
        ! Atm vars needed by the fuel moisture model
      real, parameter :: DEFAULT_T2 = 0.0, DEFAULT_Q2 = 0.0, DEFAULT_PSFC = 0.0, DEFAULT_RAINC = 0.0, &
          DEFAULT_RAINNC = 0.0
      logical :: use_geogrid


      if (present (geogrid)) then
        use_geogrid = .true.
      else
        use_geogrid = .false.
      end if

        ! Domain dimensions
      if (use_geogrid) then
        if (geogrid%ids == config_flags%ids) then
          this%ids = config_flags%ids
        else
          write (ERROR_UNIT, *) 'ids in namelist and geogrid differ'
          stop
        end if
        if (geogrid%ide == config_flags%ide) then
          this%ide = config_flags%ide
        else
          write (ERROR_UNIT, *) 'ide in namelist and geogrid differ'
          stop
        end if
        if (geogrid%jds == config_flags%jds) then
          this%jds = config_flags%jds
        else
          write (ERROR_UNIT, *) 'jds in namelist and geogrid differ'
          stop
        end if
        if (geogrid%jde == config_flags%jde) then
          this%jde = config_flags%jde
        else
          write (ERROR_UNIT, *) 'jde in namelist and geogrid differ'
          stop
        end if
      else
        this%ids = config_flags%ids
        this%ide = config_flags%ide
        this%jds = config_flags%jds
        this%jde = config_flags%jde
      end if
      this%kds = config_flags%kds
      this%kde = config_flags%kde

      this%ims = config_flags%ids - N_POINTS_IN_HALO
      this%ime = config_flags%ide + N_POINTS_IN_HALO
      this%kms = config_flags%kds
      this%kme = config_flags%kde
      this%jms = config_flags%jds - N_POINTS_IN_HALO
      this%jme = config_flags%jde + N_POINTS_IN_HALO

      this%ips = config_flags%ids
      this%ipe = config_flags%ide
      this%kps = config_flags%kds
      this%kpe = config_flags%kde
      this%jps = config_flags%jds
      this%jpe = config_flags%jde

      this%its = config_flags%ids
      this%ite = config_flags%ide
      this%kts = config_flags%kds
      this%kte = config_flags%kde
      this%jts = config_flags%jds
      this%jte = config_flags%jde

      this%num_tiles = 1
      allocate (this%i_start(this%num_tiles))
      this%i_start = this%ids 
      allocate (this%i_end(this%num_tiles))
      this%i_end = this%ide
      allocate (this%j_start(this%num_tiles))
      this%j_start = this%jds 
      allocate (this%j_end(this%num_tiles))
      this%j_end = this%jde

        ! Datetimes
      this%datetime_start = datetime_t (config_flags%start_year, config_flags%start_month, config_flags%start_day, &
          config_flags%start_hour, config_flags%start_minute, config_flags%start_second)
      this%datetime_end = datetime_t (config_flags%end_year, config_flags%end_month, config_flags%end_day, &
          config_flags%end_hour, config_flags%end_minute, config_flags%end_second)
      this%datetime_now = this%datetime_start

      this%datetime_next_output = this%datetime_start
      call this%datetime_next_output%Add_seconds (config_flags%interval_output)

      this%datetime_next_atm_update = this%datetime_start

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
      this%z0 = DEFAULT_Z0
      allocate (this%mut(this%ims:this%ime, this%jms:this%jme))
      allocate (this%ht(this%ims:this%ime, this%jms:this%jme))
      this%ht = DEFAULT_HT
      allocate (this%rainc(this%ims:this%ime, this%jms:this%jme))
      this%rainc = DEFAULT_RAINC
      allocate (this%rainnc(this%ims:this%ime, this%jms:this%jme))
      this%rainnc = DEFAULT_RAINNC
      allocate (this%t2(this%ims:this%ime, this%jms:this%jme))
      this%t2 = DEFAULT_T2
      allocate (this%q2(this%ims:this%ime, this%jms:this%jme))
      this%q2 = DEFAULT_Q2
      allocate (this%psfc(this%ims:this%ime, this%jms:this%jme))
      this%psfc = DEFAULT_PSFC

      allocate (this%c1h(this%kms:this%kme))
      this%c1h = DEFAULT_C1H
      allocate (this%c2h(this%kms:this%kme))
      this%c2h = DEFAULT_C2H

      allocate (this%rthfrten(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))
      allocate (this%rqvfrten(this%ims:this%ime, this%kms:this%kme, this%jms:this%jme))

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

      allocate (this%fmc_gc(this%ims:this%ime, NUM_FMC, this%jms:this%jme))
      allocate (this%fmc_equi(this%ims:this%ime, NUM_FMC, this%jms:this%jme))
      allocate (this%fmc_lag(this%ims:this%ime, NUM_FMC, this%jms:this%jme))
      allocate (this%fmep(this%ims:this%ime, NUM_FMEP, this%jms:this%jme))

        ! Grid dimensions
      if_geogrid: if (use_geogrid) then
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

        ! Fire grid
      call Get_ijk_from_subgrid (this, this%ifds, this%ifde, this%jfds, this%jfde, this%kfds, this%kfde, &
          this%ifms, this%ifme, this%jfms, this%jfme, this%kfms, this%kfme, this%ifps, this%ifpe, this%jfps, &
          this%jfpe, this%kfps, this%kfpe, this%ifts, this%ifte, this%jfts, this%jfte, this%kfts, this%kfte)

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
      allocate (this%fire_t2(this%ifms:this%ifme, this%jfms:this%jfme))

      this%dt = config_flags%dt

      allocate (this%xlat(this%ims:this%ime, this%jms:this%jme))
      allocate (this%xlong(this%ims:this%ime, this%jms:this%jme))
      allocate (this%zsf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%dzdxf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%dzdyf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%nfuel_cat(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%emis_smoke(this%ifms:this%ifme, this%jfms:this%jfme))
      this%emis_smoke = 0.0

      if_geogrid2d: if (use_geogrid) then
        this%xlat = 0.0
        this%xlat(this%ids:this%ide - 1, this%jds:this%jde - 1) = geogrid%xlat
        this%xlong = 0.0
        this%xlong(this%ids:this%ide - 1, this%jds:this%jde - 1) = geogrid%xlong
        this%zsf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%elevations
        this%dzdxf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%dz_dxs
        this%dzdyf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%dz_dys
        this%nfuel_cat(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%fuel_cats
      else
        this%zsf = DEFAULT_ZSF
        this%dzdxf = DEFAULT_DZDXF
        this%dzdyf = DEFAULT_DZDYF
      end if if_geogrid2d

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

    subroutine Get_ijk_from_subgrid (  grid ,                &
                           ids0, ide0, jds0, jde0, kds0, kde0,    &
                           ims0, ime0, jms0, jme0, kms0, kme0,    &
                           ips0, ipe0, jps0, jpe0, kps0, kpe0,    &
                           its0, ite0, jts0, jte0, kts0, kte0     )

    ! return the values for subgrid whose refinement is in grid%sr
    ! note when using this routine, it does not affect K. For K 
    ! (vertical), it just returns what get_ijk_from_grid does

      type (domain), intent (in) :: grid
      integer, intent(out) ::                                 &
                       ids0, ide0, jds0, jde0, kds0, kde0,    &
                       ims0, ime0, jms0, jme0, kms0, kme0,    &
                       ips0, ipe0, jps0, jpe0, kps0, kpe0,    &
                       its0, ite0, jts0, jte0, kts0, kte0
        ! Local
      integer ::                              &
                ids, ide, jds, jde, kds, kde, &
                ims, ime, jms, jme, kms, kme, &
                ips, ipe, jps, jpe, kps, kpe, &
                its, ite, jts, jte, kts, kte


      ids0 = grid%ids
      ide0 = grid%ide * grid%sr_x
      ims0 = (grid%ims - 1) * grid%sr_x + 1
      ime0 = grid%ime * grid%sr_x
      ips0 = (grid%ips - 1) * grid%sr_x + 1
      ipe0 = grid%ipe * grid%sr_x
      its0 = (grid%its - 1) * grid%sr_x + 1
      ite0 = (grid%ite - grid%ids + 1) * grid%sr_x + ids0 - 1

      jds0 = grid%jds
      jde0 = grid%jde * grid%sr_y
      jms0 = (grid%jms - 1) * grid%sr_y + 1
      jme0 = grid%jme * grid%sr_y
      jps0 = (grid%jps - 1) * grid%sr_y + 1
      jpe0 = grid%jpe * grid%sr_y
      jts0 = (grid%jts - 1) * grid%sr_y + 1
      jte0 = (grid%jte - grid%jds + 1) * grid%sr_y + jds0 - 1

      kds0 = grid%kds
      kde0 = grid%kde
      kms0 = grid%kms
      kme0 = grid%kme
      kps0 = grid%kps
      kpe0 = grid%kpe
      kts0 = grid%kts
      kte0 = grid%kte

      return

    end subroutine Get_ijk_from_subgrid

    subroutine Print_domain (this)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

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

      write (OUTPUT_UNIT, *) 'its = ', this%its, 'ite = ', this%ite
      write (OUTPUT_UNIT, *) 'jts = ', this%jts, 'jte = ', this%jte
      write (OUTPUT_UNIT, *) 'kts = ', this%kts, 'kte = ', this%kte


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
      write (OUTPUT_UNIT, *) 'shape ph_2 = ', shape (this%ph_2)
      write (OUTPUT_UNIT, *) 'shape phb = ', shape (this%phb)
      write (OUTPUT_UNIT, *) 'shape u_2 = ', shape (this%u_2)
      write (OUTPUT_UNIT, *) 'shape v_2 = ', shape (this%v_2)
      write (OUTPUT_UNIT, *) 'shape i_start = ', shape (this%i_start)

    end subroutine Print_domain

    subroutine Save_state (this)

      implicit none

      class (state_fire_t), intent (in) :: this

      character (len = :), allocatable :: file_output


      file_output='fire_output_'//this%datetime_now%datetime//'.nc'

      call Create_netcdf_file (file_name = file_output)

      call Add_netcdf_dim (file_output, 'nx', this%nx)
      call Add_netcdf_dim (file_output, 'ny', this%ny)

      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fgrnhfx', this%fgrnhfx(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fire_area', this%fire_area(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'emis_smoke', this%emis_smoke(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fire_t2', this%fire_t2(1:this%nx, 1:this%ny))

    end subroutine Save_state

  end module state_mod

