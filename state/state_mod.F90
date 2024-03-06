  module state_mod

    use constants_mod, only : PI
    use datetime_mod, only : datetime_t
    use fmc_mod, only : fmc_t
    use fuel_mod, only : fuel_t, FUEL_ANDERSON, Crosswalk_from_scottburgan_to_anderson
    use geogrid_mod, only : geogrid_t
    use ignition_line_mod, only : ignition_line_t
    use namelist_mod, only : namelist_t
    use netcdf_mod, only : Create_netcdf_file, Add_netcdf_dim, Add_netcdf_var
    use proj_lc_mod, only : proj_lc_t
    use ros_mod, only : ros_t
    use stderrout_mod, only : Stop_simulation, Print_message
    use tiles_mod, only : Calc_tiles_dims
    use wrf_mod, only : wrf_t, G, RERADIUS

    implicit none

    private

    public :: state_fire_t

    integer, parameter :: N_POINTS_IN_HALO = 5

    type :: state_fire_t
      integer :: ifds, ifde, jfds, jfde, kfds, kfde, ifms, ifme, jfms, jfme, kfms, kfme, &
                 ifts, ifte, jfts, jfte, kfts, kfte, ifps, ifpe, jfps, jfpe, kfps, kfpe
      real :: dx = 200.0 , dy = 200.0
      real :: dt = 2.0              ! "TEMPORAL RESOLUTION"      "SECONDS"
      integer :: itimestep = 0
      integer :: num_tiles = 1
      type (datetime_t) :: datetime_start, datetime_end, datetime_now, datetime_next_output, datetime_next_atm_update

      real, dimension(:, :), allocatable :: uf ! W-E winds used in fire module
      real, dimension(:, :), allocatable :: vf ! S-N winds used in fire module
      real, dimension(:, :), allocatable :: zsf    ! terrain height
      real, dimension(:, :), allocatable :: dzdxf  ! terrain grad
      real, dimension(:, :), allocatable :: dzdyf  ! terrain grad
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
      real, dimension(:, :), allocatable :: lfn_out
      real, dimension(:, :), allocatable :: fuel_load_g ! [kg m-2]
      real, dimension(:, :), allocatable :: flame_length ! "fire flame length" "m"
      real, dimension(:, :), allocatable :: ros_front ! "rate of spread at fire front" "m/s"
      real, dimension(:, :), allocatable :: tign_g ! "ignition time on ground" "s"
      real, dimension(:, :), allocatable :: fuel_frac ! "fuel remaining" "1"
      real, dimension(:, :), allocatable :: fire_area ! "fraction of cell area on fire" "1"
      real, dimension(:, :), allocatable :: fuel_frac_burnt_dt ! "fraction of fuel burnt on current dt" "-"
      real, dimension(:, :), allocatable :: fgrnhfx ! "heat flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: fgrnqfx ! "moisture flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: fcanhfx ! "heat flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: fcanqfx ! "moisture flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: ros ! "rate of spread" "m/s"
      real, dimension(:, :), allocatable :: lats   ! "latitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lons   ! "longitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lats_c ! "latitude of corners of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lons_c ! "longitude of corners of fire cells" "degrees"
      real, dimension(:, :), allocatable :: fz0 ! "roughness length of fire cells" "m"
      real, dimension(:, :), allocatable :: nfuel_cat ! "fuel data"
      real, dimension(:, :), allocatable :: fuel_time ! "fuel"
      real, dimension(:, :), allocatable :: emis_smoke

      class (fuel_t), allocatable :: fuels
      class (ros_t), allocatable :: ros_param
      type (ignition_line_t) :: ignition_lines
      class (fmc_t), allocatable :: fmc_param

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

      integer, dimension(:), allocatable :: i_start, i_end, j_start, j_end

      real :: unit_fxlong, unit_fxlat
      integer :: nx ! "number of longitudinal grid points" "1"
      integer :: ny ! "number of latitudinal grid points" "1"
      real :: cen_lat, cen_lon
    contains
      procedure, public :: Convert_sb_to_ander => Convert_scottburgan_to_anderson
      procedure, public :: Handle_output => Handle_output
      procedure, public :: Handle_wrfdata_update => Handle_wrfdata_update
      procedure, public :: Init_fuel_vars => Init_fuel_vars
      procedure, public :: Initialization => Init_domain
      procedure, public :: Init_ignition_lines => Init_ignition_lines
      procedure :: Init_latlons => Init_latlons
      procedure :: Init_tiles => Init_tiles
      procedure :: Interpolate_vars_atm_to_fire => Interpolate_vars_atm_to_fire
      procedure, public :: Interpolate_profile => Interpolate_profile
      procedure, public :: Print => Print_domain ! private
      procedure, public :: Save_state => Save_state
    end type state_fire_t

  contains

    subroutine Convert_scottburgan_to_anderson (this)

      implicit none

      class (state_fire_t), intent(in out) :: this

      integer :: i, j, ij, ifts, ifte, jfts, jfte


      do ij = 1, this%num_tiles
        ifts = this%i_start(ij)
        ifte = this%i_end(ij)
        jfts = this%j_start(ij)
        jfte = this%j_end(ij)

        do j = jfts, jfte
          do i = ifts, ifte
            this%nfuel_cat(i, j) = real (Crosswalk_from_scottburgan_to_anderson (int (this%nfuel_cat(i, j))))
          end do
        end do
      end do

    end subroutine Convert_scottburgan_to_anderson

    subroutine Handle_output (this, config_flags)

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags

      logical, parameter :: DEBUG_LOCAL = .false.


      if (this%datetime_now == this%datetime_next_output) then
        if (DEBUG_LOCAL) call Print_message ('Writing output...')
        call this%Save_state ()
        call this%datetime_next_output%Add_seconds (config_flags%interval_output)
      end if

    end subroutine Handle_output

    subroutine Handle_wrfdata_update (this, wrf, config_flags)

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      type (wrf_t), intent (in out) :: wrf

      logical, parameter :: DEBUG_LOCAL = .true.


      If_update_atm: if (this%datetime_now == this%datetime_next_atm_update) then
        if (DEBUG_LOCAL) call Print_message ('Updating wrfdata...')
        if (DEBUG_LOCAL) call this%datetime_now%Print_datetime ()

        call wrf%Update_atm_state (this%datetime_now)

        call this%interpolate_vars_atm_to_fire(wrf, config_flags)

        call this%datetime_next_atm_update%Add_seconds (config_flags%interval_atm)

      end if If_update_atm

    end subroutine Handle_wrfdata_update

    subroutine Init_domain (this, config_flags, geogrid)

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      type (geogrid_t), intent (in) :: geogrid

      logical, parameter :: DEBUG_LOCAL = .false.
      integer :: ids0, ide0, jds0, jde0


        ! Domain dimensions
      ids0 = geogrid%ifds
      ide0 = geogrid%ifde
      jds0 = geogrid%jfds
      jde0 = geogrid%jfde

      this%ifds = ids0
      this%ifde = ide0
      this%ifms = ids0 - N_POINTS_IN_HALO
      this%ifme = ide0 + N_POINTS_IN_HALO
      this%ifps = ids0
      this%ifpe = ide0

      this%jfds = jds0
      this%jfde = jde0
      this%jfms = jds0 - N_POINTS_IN_HALO
      this%jfme = jde0 + N_POINTS_IN_HALO
      this%jfps = jds0
      this%jfpe = jde0

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

      this%cen_lat = geogrid%cen_lat
      this%cen_lon = geogrid%cen_lon

      this%dx = geogrid%dx / geogrid%sr_x
      this%dy = geogrid%dy / geogrid%sr_y

      if (DEBUG_LOCAL) call this%Print()

      this%nx = this%ifde
      this%ny = this%jfde

      allocate (this%uf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%vf(this%ifms:this%ifme, this%jfms:this%jfme))
      this%uf = 0.
      this%vf = 0.
      allocate (this%fmc_g(this%ifms:this%ifme, this%jfms:this%jfme))
      this%fmc_g = config_flags%fuelmc_g

        ! Init lfn more than the largest domain side
      allocate (this%lfn(this%ifms:this%ifme, this%jfms:this%jfme))
      this%lfn(this%ifds:this%ifde, this%jfds:this%jfde) = 2.0 * &
          max ((this%ifde - this%ifds + 1) * this%dx, (this%jfde - this%jfds + 1) * this%dy)

      allocate (this%lfn_hist(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_0(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_1(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_2(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_s0(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_s1(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_s2(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_s3(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lfn_out(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fuel_load_g(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%flame_length(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%ros_front(this%ifms:this%ifme, this%jfms:this%jfme))

        ! Init tign_g a bit into the future
      allocate (this%tign_g(this%ifms:this%ifme, this%jfms:this%jfme))
      this%tign_g(this%ifps:this%ifpe, this%jfps:this%jfpe) = epsilon (this%tign_g)

      allocate (this%fuel_frac(this%ifms:this%ifme, this%jfms:this%jfme))
      this%fuel_frac(this%ifds:this%ifde, this%jfds:this%jfde) = 1.0

      allocate (this%fire_area(this%ifms:this%ifme, this%jfms:this%jfme))
      this%fire_area(this%ifds:this%ifde, this%jfds:this%jfde) = 0.0

      allocate (this%fuel_frac_burnt_dt(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fgrnhfx(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fgrnqfx(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fcanhfx(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fcanqfx(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%ros(this%ifms:this%ifme, this%jfms:this%jfme))
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

      this%dt = config_flags%dt

      allocate (this%zsf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%dzdxf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%dzdyf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%nfuel_cat(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%emis_smoke(this%ifms:this%ifme, this%jfms:this%jfme))
      this%emis_smoke = 0.0

      this%zsf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%elevations
      this%dzdxf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%dz_dxs
      this%dzdyf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%dz_dys
      this%nfuel_cat(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%fuel_cats

      if (config_flags%fire_is_real_perim) then
        if (allocated (geogrid%lfn_init)) then
          this%lfn_hist(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%lfn_init
        else
          Call Stop_simulation ('Attenting to initialize fire from given  perimeter but no initialization data present')
        end if
      end if

      this%unit_fxlat = 2.0 * PI / (360.0 * RERADIUS)  ! earth circumference in m / 360 degrees
      this%unit_fxlong = cos (this%cen_lat * 2.0 * PI / 360.0) * this%unit_fxlat  ! latitude
      call this%Init_latlons (geogrid)

      call this%Init_tiles (config_flags)

      if (config_flags%fuel_opt == FUEL_ANDERSON) call this%Convert_sb_to_ander ()

    end subroutine Init_domain

    subroutine Init_fuel_vars (this)

      implicit none

      class (state_fire_t), intent(in out) :: this

      integer :: ij, i, j, ifts, ifte, jfts, jfte, k


      do ij = 1, this%num_tiles
        ifts = this%i_start(ij)
        ifte = this%i_end(ij)
        jfts = this%j_start(ij)
        jfte = this%j_end(ij)
        do j = jfts, jfte
          do i = ifts, ifte
            k = int (this%nfuel_cat(i, j))
            this%fuel_load_g(i, j) = this%fuels%fgi(k)
            if (k == this%fuels%no_fuel_cat) then
              this%fuel_time(i, j) = 0.0
            else
                ! set fuel time constant (the e-folding time)
                ! burn time from fuels: weight=1000 => 40% decrease over 10 min
                ! fuel decreases as exp(-t/fuel_time)
                ! exp(-600*0.85/1000) = approx 0.6
              this%fuel_time(i, j) = this%fuels%weight(k) / 0.85
            end if
          end do
        end do
      end do

    end subroutine Init_fuel_vars

    subroutine Init_ignition_lines (this, config_flags)

      implicit none

      class (state_fire_t), intent (in out) :: this

      type (namelist_t), intent (in) :: config_flags


      call this%ignition_lines%Init (config_flags)

    end subroutine Init_ignition_lines

    subroutine Init_latlons (this, geogrid)

      implicit none

      class (state_fire_t), intent (in out) :: this
      type (geogrid_t), intent(in) :: geogrid

      real, parameter :: OFFSET = 0.5
      type (proj_lc_t) :: proj
      integer :: i, j
      real :: i_atm, j_atm, offset_corners_x, offset_corners_y


      allocate (this%lons(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lats(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lons_c(this%nx + 1, this%ny + 1))
      allocate (this%lats_c(this%nx + 1, this%ny + 1))

      proj = geogrid%Get_atm_proj ()

      offset_corners_x = (1.0 / real (geogrid%sr_x)) / 2.0
      offset_corners_y = (1.0 / real (geogrid%sr_y)) / 2.0

      do j = 1, this%ny
        do i = 1, this%nx
          i_atm = (i - OFFSET) / geogrid%sr_x + OFFSET
          j_atm = (j - OFFSET) / geogrid%sr_y + OFFSET
          call proj%Calc_latlon (i = i_atm, j = j_atm, lat = this%lats(i, j), lon = this%lons(i, j))
          call proj%Calc_latlon (i = i_atm - offset_corners_x, j = j_atm - offset_corners_y, &
              lat = this%lats_c(i, j), lon = this%lons_c(i, j))
        end do
      end do

      do j = 1, this%ny
        i_atm = (this%nx - OFFSET) / geogrid%sr_x + OFFSET
        j_atm = (j - OFFSET) / geogrid%sr_y + OFFSET
        call proj%Calc_latlon (i = i_atm + offset_corners_x, j = j_atm - offset_corners_y, &
            lat = this%lats_c(this%nx + 1, j), lon = this%lons_c(this%nx + 1, j))
      end do

      do i = 1, this%nx
        i_atm = (i - OFFSET) / geogrid%sr_x + OFFSET
        j_atm = (this%ny - OFFSET) / geogrid%sr_y + OFFSET
        call proj%Calc_latlon (i = i_atm - offset_corners_x, j = j_atm + offset_corners_y, &
            lat = this%lats_c(i, this%ny + 1), lon = this%lons_c(i, this%ny + 1))
      end do

      i_atm = (this%nx - OFFSET) / geogrid%sr_x + OFFSET
      j_atm = (this%ny - OFFSET) / geogrid%sr_y + OFFSET
      call proj%Calc_latlon (i = i_atm + offset_corners_x, j = j_atm + offset_corners_y, &
          lat = this%lats_c(this%nx + 1, this%ny + 1), lon = this%lons_c(this%nx + 1, this%ny + 1))

    end subroutine Init_latlons

    subroutine Init_tiles (this, config_flags)

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags

      integer :: num_tiles

      num_tiles = config_flags%num_tiles
      call Calc_tiles_dims (this%ifps, this%ifpe, this%jfps, this%jfpe, num_tiles, &
          this%i_start, this%i_end, this%j_start, this%j_end)

      if (num_tiles /= config_flags%num_tiles) then
        call Stop_simulation ('Not able to use the number of tiles specified')
      end if

    end subroutine Init_tiles

    subroutine Interpolate_vars_atm_to_fire (this, wrf, config_flags)

      implicit none

      class (state_fire_t), intent(in out) :: this    ! fire state
      type (wrf_t), intent(inout) :: wrf                 ! atm state
      type (namelist_t), intent (in) :: config_flags

      real, dimension(:, :), allocatable :: var2d
      integer :: i, j


          ! Alternative interpolation in testing mode (no impact on the fire evolution)
          ! We need the fire grid lat/lon
      if (allocated (this%lats) .and. allocated (this%lons)) then

        If_start: if (this%datetime_now == this%datetime_start) then
          call wrf%interp_var2grid_nearest (this%lats(this%ifds:this%ifde, this%jfds:this%jfde), &
              this%lons(this%ifds:this%ifde, this%jfds:this%jfde), 'fz0', var2d)
              this%fz0(this%ifds:this%ifde, this%jfds:this%jfde) = var2d
        endif If_start

        do j = 1, wrf%jde
          do i = 1, wrf%ide
            call this%interpolate_profile (config_flags, config_flags%fire_wind_height, this%kfds, this%kfde, &
                wrf%u3d_stag(i,:,j),wrf%v3d_stag(i,:,j), wrf%phl_stag(i,:,j), wrf%ua(i,j),wrf%va(i,j),wrf%z0_stag(i,j))
          end do
        end do

        call wrf%interp_var2grid_nearest (this%lats(this%ifds:this%ifde, this%jfds:this%jfde), &
            this%lons(this%ifds:this%ifde, this%jfds:this%jfde), 'uf', var2d)
            this%uf(this%ifds:this%ifde, this%jfds:this%jfde) = var2d

        call wrf%interp_var2grid_nearest (this%lats(this%ifds:this%ifde, this%jfds:this%jfde), &
            this%lons(this%ifds:this%ifde, this%jfds:this%jfde), 'vf', var2d)
            this%vf(this%ifds:this%ifde, this%jfds:this%jfde) = var2d

        call wrf%interp_var2grid_nearest (this%lats(this%ifds:this%ifde, this%jfds:this%jfde), &
            this%lons(this%ifds:this%ifde, this%jfds:this%jfde), 't2', var2d)
            this%fire_t2(this%ifds:this%ifde, this%jfds:this%jfde) = var2d

        call wrf%interp_var2grid_nearest (this%lats(this%ifds:this%ifde, this%jfds:this%jfde), &
            this%lons(this%ifds:this%ifde, this%jfds:this%jfde), 'q2', var2d)
            this%fire_q2(this%ifds:this%ifde, this%jfds:this%jfde) = var2d

        call wrf%interp_var2grid_nearest (this%lats(this%ifds:this%ifde, this%jfds:this%jfde), &
            this%lons(this%ifds:this%ifde, this%jfds:this%jfde), 'psfc', var2d)
            this%fire_psfc(this%ifds:this%ifde, this%jfds:this%jfde) = var2d

        call wrf%interp_var2grid_nearest (this%lats(this%ifds:this%ifde, this%jfds:this%jfde), &
            this%lons(this%ifds:this%ifde, this%jfds:this%jfde), 'rain', var2d)
            this%fire_rain(this%ifds:this%ifde, this%jfds:this%jfde) = var2d

        deallocate (var2d)
      end if

    end subroutine Interpolate_vars_atm_to_fire

    subroutine Interpolate_profile (this, config_flags, fire_wind_height, kfds, kfde, &
        uin, vin, phl, uout, vout,z0f)

      implicit none

      class (state_fire_t), intent (in) :: this
      type (namelist_t), intent (in) :: config_flags
      real, intent (in) :: fire_wind_height
      integer, intent (in) :: kfds, kfde
      real, intent (in) :: uin(:), vin(:), phl(:)
      real, intent (out) :: uout, vout
      real, intent (in) :: z0f


      real, parameter :: VK_KAPPA = 0.4
      real, dimension (kfds:kfde - 1) :: altw, hgt
      integer :: k, kdmax
      real :: loght, loglast, logz0, logfwh, ht, r_nan, fire_wind_height_local, z0fc, &
          ust_d, wsf, wsf1, uf_temp, vf_temp


        ! max layer to interpolate from, can be less
      kdmax = kfde - 2
      do k = kfds, kdmax + 1
          ! altitude of the bottom w-point
        altw(k) = phl(k) / G
      end do

      do k = kfds, kdmax
          ! height of the mass point above the ground
        hgt(k) = 0.5 * (altw(k) + altw(k + 1)) - altw(kfds)
      end do

        ! extrapolate mid-flame height from fire_lsm_zcoupling_ref?
      if (config_flags%fire_lsm_zcoupling) then
        logfwh = log (config_flags%fire_lsm_zcoupling_ref)
        fire_wind_height_local = config_flags%fire_lsm_zcoupling_ref
      else
        logfwh = log (fire_wind_height)
        fire_wind_height_local = fire_wind_height
      end if

        ! interpolate u
      if (fire_wind_height_local > z0f)then
        do k = kfds, kdmax
          ht = hgt(k)
          if (ht >= fire_wind_height_local) then
              ! found layer k this point is in
            loght = log(ht)
            if (k == kfds) then
                ! first layer, log linear interpolation from 0 at zr
              logz0 = log(z0f)
              uout = uin(k) * (logfwh - logz0) / (loght - logz0)
              vout = vin(k) * (logfwh - logz0) / (loght - logz0)
            else
                ! log linear interpolation
              loglast = log (hgt(k - 1))
              uout = uin(k - 1) + (uin(k) - uin(k - 1)) * (logfwh - loglast) / (loght - loglast)
              vout = vin(k - 1) + (vin(k) - vin(k - 1)) * (logfwh - loglast) / (loght - loglast)
            end if
            exit
          end if
          if (k == kdmax) then
              ! last layer, still not high enough
            uout = uin(k)
            vout = vin(k)
          end if
        end do
      else
          ! roughness higher than the fire wind height
        uout = 0.0
        vout = 0.0
      end if

        ! Extrapol wind to target height
      if (config_flags%fire_lsm_zcoupling) then
        uf_temp = uout
        vf_temp = vout
        wsf = max (sqrt (uf_temp ** 2.0 + vf_temp ** 2.0), 0.1)
        z0fc = z0f
        ust_d = wsf * VK_KAPPA / log(config_flags%fire_lsm_zcoupling_ref / z0fc)
        wsf1 = (ust_d / VK_KAPPA) * log((fire_wind_height + z0fc) / z0fc)
        uout = wsf1 * uf_temp / wsf
        vout = wsf1 * vf_temp / wsf
      end if

    end subroutine Interpolate_profile

    subroutine Print_domain (this)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (state_fire_t), intent(in out) :: this


      write (OUTPUT_UNIT, *) ''

      write (OUTPUT_UNIT, *) 'ifds = ', this%ifds, 'ifde = ', this%ifde
      write (OUTPUT_UNIT, *) 'jfds = ', this%jfds, 'jfde = ', this%jfde
      write (OUTPUT_UNIT, *) 'kfds = ', this%kfds, 'kfde = ', this%kfde

      write (OUTPUT_UNIT, *) 'ifms = ', this%ifms, 'ifme = ', this%ifme
      write (OUTPUT_UNIT, *) 'jfms = ', this%jfms, 'jfme = ', this%jfme
      write (OUTPUT_UNIT, *) 'kfms = ', this%kfms, 'kfme = ', this%kfme

      write (OUTPUT_UNIT, *) 'ifts = ', this%ifts, 'ifte = ', this%ifte
      write (OUTPUT_UNIT, *) 'jfts = ', this%jfts, 'jfte = ', this%jfte
      write (OUTPUT_UNIT, *) 'kfts = ', this%kfts, 'kfte = ', this%kfte

      write (OUTPUT_UNIT, *) ''

    end subroutine Print_domain

    subroutine Save_state (this)

      implicit none

      class (state_fire_t), intent (in) :: this

      character (len = :), allocatable :: file_output


      file_output='fire_output_'//this%datetime_now%datetime//'.nc'

      call Create_netcdf_file (file_name = file_output)

      call Add_netcdf_dim (file_output, 'nx', this%nx)
      call Add_netcdf_dim (file_output, 'ny', this%ny)

      call Add_netcdf_var (file_output, ['nx', 'ny'], 'lats', this%lats(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'lons', this%lons(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fgrnhfx', this%fgrnhfx(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fire_area', this%fire_area(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fuel_frac_burnt_dt', this%fuel_frac_burnt_dt(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'fuel_frac', this%fuel_frac(1:this%nx, 1:this%ny))
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
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'lfn', this%lfn(1:this%nx, 1:this%ny))
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'nfuel_cat', this%nfuel_cat(1:this%nx, 1:this%ny))

    end subroutine Save_state

  end module state_mod

