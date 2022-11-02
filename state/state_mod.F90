  module state_mod

    use namelist_mod, only : namelist_t, NUM_FMC
    use geogrid_mod, only : geogrid_t

    implicit none

    private

    public :: domain, P_FIRE_SMOKE, NUM_TRACER

    integer, parameter :: NUM_TRACER = 1, NUM_FMEP = 2, P_FIRE_SMOKE = 1
    integer, parameter :: N_POINTS_IN_HALO = 5

    type :: state_t
      real, dimension (:, :), allocatable :: lats, lons, elevations, dz_dxs, dz_dys, fuel_cats
      real :: dx = 200.0 , dy = 200.0
      real :: dt = 2.0              ! "TEMPORAL RESOLUTION"      "SECONDS"
      integer :: itimestep = 0

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
      real, dimension(:, :), allocatable :: avg_fuel_frac ! "fuel remaining averaged to atmospheric grid" "1"
      real, dimension(:, :), allocatable :: grnhfx ! "heat flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: grnqfx ! "moisture flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: canhfx ! "heat flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: canqfx ! "moisture flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: grnhfx_fu ! "heat flux from ground fire (feedback unsensitive)" "W/m^2"
      real, dimension(:, :), allocatable :: grnqfx_fu ! "moisture flux from ground fire (feedback unsensitive)" "W/m^2"
      real, dimension(:, :), allocatable :: uah, vah ! "wind at fire_wind_height" "m/s"

        ! FMC model
      real, dimension(:, :, :), allocatable :: fmc_gc ! "fuel moisture contents by class" "1"
      real, dimension(:, :, :), allocatable :: fmep  ! "fuel moisture extended model parameters" "1"
      real, dimension(:, :, :), allocatable :: fmc_equi ! "fuel moisture contents by class equilibrium (diagnostics only)" "1"
      real, dimension(:, :, :), allocatable :: fmc_lag ! "fuel moisture contents by class time lag (diagnostics only)" "h"

      real :: fmoist_lasttime       ! "last time the moisture model was run" "s"
      real :: fmoist_nexttime       ! "next time the moisture model will run" "s"
      real :: u_frame               ! "FRAME X WIND"         "m s-1"
      real :: v_frame               ! "FRAME Y WIND"         "m s-1"
    end type state_t

    type, extends (state_t) :: domain
      integer :: ids, ide, jds, jde, kds, kde, ims, ime, jms, jme, kms, kme, ips, ipe, jps, jpe, kps, kpe
      integer :: ifds, ifde, jfds, jfde, kfds, kfde, ifms, ifme, jfms, jfme, kfms, kfme, &
                 ifps, ifpe, jfps, jfpe, kfps, kfpe
      integer :: sr_x = 0, sr_y = 0
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
        ! 1D
      real, dimension(:), allocatable :: c1h ! "half levels, c1h = d bf / d eta, using znw"        "Dimensionless"
      real, dimension(:), allocatable :: c2h ! "half levels, c2h = (1-c1h)*(p0-pt)"                "Pa"
        ! feedback to atm
      real, dimension (:, :, :), allocatable :: rthfrten ! "temperature tendency" "K/s"
      real, dimension (:, :, :), allocatable :: rqvfrten ! "RQVFRTEN" "humidity tendency" Stagger in z

        ! Fire vars in the atm grid
      real, dimension(:, :), allocatable :: rain_old ! "previous value of accumulated rain" "mm"
      real, dimension(:, :), allocatable :: t2_old   ! "previous value of air temperature at 2m" "K"
      real, dimension(:, :), allocatable :: q2_old   ! "previous value of 2m specific humidity" "kg/kg"
      real, dimension(:, :), allocatable :: psfc_old ! "previous value of surface pressure" "Pa"
      real, dimension(:, :), allocatable :: rh_fire  ! "relative humidity at the surface" "1"
    contains
      procedure, public :: Print => Print_domain
    end type domain

    interface domain
      module procedure Domain_const
    end interface domain

  contains

    function Domain_const (config_flags, geogrid) result (return_value)

      implicit none

      type (namelist_t), intent (in) :: config_flags
      type (geogrid_t), intent (in), optional :: geogrid
      type (domain) :: return_value

      call Domain_init (return_value, config_flags, geogrid)

    end function Domain_const

    subroutine Domain_init (this, config_flags, geogrid)

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

        ! Fill in atm dims including
        ! domain decomposition
!        config_flags%ids = geogrid%ids
!        config_flags%ide = geogrid%ide
!        config_flags%jds = geogrid%jds
!        config_flags%jde = geogrid%jde
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
        else
          write (ERROR_UNIT, *) 'sr_x in namelist and in geogrid differ'
          stop
        end if
        if (geogrid%sr_y == config_flags%sr_y) then
          this%sr_y = geogrid%sr_y
        else
          write (ERROR_UNIT, *) 'sr_y in namelist and in geogrid differ'
          stop
        end if
      else
        this%dx = config_flags%dx
        this%dy = config_flags%dy
        this%dt = config_flags%dt
        this%sr_x = config_flags%sr_x
        this%sr_y = config_flags%sr_y
      end if if_geogrid

        ! Fire grid
      call Get_ijk_from_subgrid (this, this%ifds, this%ifde, this%jfds, this%jfde, this%kfds, this%kfde, &
          this%ifms, this%ifme, this%jfms, this%jfme, this%kfms, this%kfme, this%ifps, this%ifpe, this%jfps, &
          this%jfpe, this%kfps, this%kfpe)

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

      this%dt = config_flags%dt

      allocate (this%xlat(this%ims:this%ime, this%jms:this%jme))
      allocate (this%xlong(this%ims:this%ime, this%jms:this%jme))
      allocate (this%zsf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%dzdxf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%dzdyf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%nfuel_cat(this%ifms:this%ifme, this%jfms:this%jfme))

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

    end subroutine Domain_init

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

  end module state_mod
