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

    public :: state_fire_t, P_FIRE_SMOKE, NUM_TRACER

    integer, parameter :: NUM_TRACER = 1, NUM_FMEP = 2, P_FIRE_SMOKE = 1
    integer, parameter :: N_POINTS_IN_HALO = 5

    type :: state_fire_t
      integer :: ifds, ifde, jfds, jfde, kfds, kfde, ifms, ifme, jfms, jfme, kfms, kfme, &
                 ifts, ifte, jfts, jfte, kfts, kfte
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
      real, dimension(:, :), allocatable :: test
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
      integer :: nx ! "number of longitudinal grid points" "1"
      integer :: ny ! "number of latitudinal grid points" "1"
      real :: cen_lat, cen_lon
    contains
      procedure, public :: Handle_output => Handle_output
      procedure, public :: Handle_wrfdata_update => Handle_wrfdata_update
      procedure, public :: Initialization => Init_domain
      procedure :: Init_latlons_fire => Init_latlons_fire
      procedure :: Interpolate_vars_atm_to_fire => Interpolate_vars_atm_to_fire
      procedure, public :: Interpolate_wind3d => Interpolate_wind3d
      procedure, public :: Print => Print_domain ! private
      procedure, public :: Provide_atm_feedback => Provide_atm_feedback
      procedure, public :: Save_state => Save_state
      procedure :: Sum_2d_fire_vars => Sum_2d_fire_vars
    end type state_fire_t

  contains

    subroutine calc_smoke_emissions(         &
           grid,config_flags,                &
           ifts,ifte,jfts,jfte)

      implicit none

      type (state_fire_t), intent(in out) :: grid   ! data
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

      type (state_fire_t), intent(in out) :: grid
      type (namelist_t), intent(in) :: config_flags

         ! real
         ! 1 degree in m (approximate OK)
      grid%unit_fxlat = 2.0 * PI / (360.0 * RERADIUS)  ! earth circumference in m / 360 degrees
      grid%unit_fxlong = cos (grid%cen_lat * 2.0 * PI / 360.0) * grid%unit_fxlat  ! latitude

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

    subroutine Handle_wrfdata_update (this, wrf, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      type (wrf_t), intent (in out) :: wrf

      logical, parameter :: DEBUG_LOCAL = .true.


      If_update_atm: if (this%datetime_now == this%datetime_next_atm_update) then
        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Updating wrfdata...'
        if (DEBUG_LOCAL) call this%datetime_now%Print_datetime ()

        call wrf%Update_atm_state (this%datetime_now)

        call wrf%interpolate_wind2fire(config_flags,                            & ! flag for debug output
                config_flags%fire_wind_height,                                  & ! height to interpolate to
                wrf%ids,wrf%ide-1, wrf%kds,wrf%kde, wrf%jds,wrf%jde-1,          &
                wrf%ims,wrf%ime, wrf%kms,wrf%kme, wrf%jms,wrf%jme,              &
                wrf%ips,min(wrf%ipe,wrf%ide-1), wrf%jps,min(wrf%jpe,wrf%jde-1), &
                wrf%i_start(1),min(wrf%i_end(1),wrf%ide-1),                     &
                wrf%j_start(1),min(wrf%j_end(1),wrf%jde-1),                     &
                this%ifds,this%ifde-wrf%sr_x, this%jfds,this%jfde-wrf%sr_y,     &
                this%ifms, this%ifme, this%jfms, this%jfme,                     &
                this%ifts, this%ifte, this%jfts, this%jfte,   &
                wrf%sr_x,wrf%sr_y,                            & ! atm/fire this ratio
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

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      type (geogrid_t), intent (in) :: geogrid

      integer :: ids0, ide0, jds0, jde0


        ! Domain dimensions
      ids0 = geogrid%ifds
      ide0 = geogrid%ifde
      jds0 = geogrid%jfds
      jde0 = geogrid%jfde

      this%ifds = ids0
      this%ifde = ide0
      this%ifms = ids0 - N_POINTS_IN_HALO * geogrid%sr_x
      this%ifme = ide0 + N_POINTS_IN_HALO * geogrid%sr_x
      this%ifts = ids0
      this%ifte = ide0

      this%jfds = jds0
      this%jfde = jde0
      this%jfms = jds0 - N_POINTS_IN_HALO * geogrid%sr_y
      this%jfme = jde0 + N_POINTS_IN_HALO * geogrid%sr_y
      this%jfts = jds0
      this%jfte = jde0

      this%kfds = config_flags%kds
      this%kfde = config_flags%kde
      this%kfms = config_flags%kds
      this%kfme = config_flags%kde
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

      call this%Print()

      this%nx = this%ifde
      this%ny = this%jfde



      allocate (this%uf(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%vf(this%ifms:this%ifme, this%jfms:this%jfme))
      this%uf = 0.
      this%vf = 0.
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

      allocate (this%test(this%ifms:this%ifme, this%jfms:this%jfme))
      this%test = 0.0

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


      this%zsf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%elevations
      this%dzdxf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%dz_dxs
      this%dzdyf(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%dz_dys
      this%nfuel_cat(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%fuel_cats

      call calc_unit_fxlat_fxlong (this, config_flags)
      call Init_fxlatfxlong (this,geogrid)
      call this%Init_latlons_fire (geogrid)

    end subroutine Init_domain

    subroutine Init_latlons_fire (this, geogrid)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT

      implicit none

      class (state_fire_t), intent (in out) :: this
      type (geogrid_t), intent(in) :: geogrid

      real, parameter :: OFFSET = 0.5
      type (proj_lc_t) :: proj
      integer :: i, j
      real :: i_atm, j_atm, offset_corners_x, offset_corners_y


      allocate (this%lons(this%nx, this%ny))
      allocate (this%lats(this%nx, this%ny))
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

    end subroutine Init_latlons_fire

    subroutine Interpolate_vars_atm_to_fire (this, wrf)

        implicit none

        class (state_fire_t), intent(in out) :: this    ! fire state
        type (wrf_t), intent(in) :: wrf                 ! atm state

        real, dimension(:, :), allocatable :: var2d


        If_start: if (this%datetime_now == this%datetime_start) then

          call wrf%interpolate_z2fire(                     &
              this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
              this%ifms, this%ifme, this%jfms, this%jfme,  &
              this%ifts,this%ifte,this%jfts,this%jfte,     &
              wrf%sr_x,wrf%sr_y,                           & ! atm/fire wrf ratio
              wrf%z0_stag,                                 &
              this%fz0,1)

        endif If_start

        call wrf%interpolate_z2fire(                    &
            this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
            this%ifms, this%ifme, this%jfms, this%jfme,  &
            this%ifts,this%ifte,this%jfts,this%jfte,     &
            wrf%sr_x,wrf%sr_y,                           & ! atm/fire this ratio
            wrf%t2_stag,                                 &
            this%fire_t2,1)

          ! Alternative interpolation in testing mode (no impact on the fire evolution)
          ! We need the fire grid lat/lon
        if (allocated (this%lats) .and. allocated (this%lons)) then
          call wrf%interp_var2grid_nearest (this%lats(this%ifds:this%ifde, this%jfds:this%jfde), &
              this%lons(this%ifds:this%ifde, this%jfds:this%jfde), 't2', var2d)
              this%test(this%ifds:this%ifde, this%jfds:this%jfde) = var2d
              deallocate (var2d)
        end if

         call wrf%interpolate_z2fire(                    &
            this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
            this%ifms, this%ifme, this%jfms, this%jfme,  &
            this%ifts,this%ifte,this%jfts,this%jfte,     &
            wrf%sr_x,wrf%sr_y,                           & ! atm/fire wrf ratio
            wrf%q2_stag,                                 &
            this%fire_q2,1)

         call wrf%interpolate_z2fire(                    &
            this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
            this%ifms, this%ifme, this%jfms, this%jfme,  &
            this%ifts,this%ifte,this%jfts,this%jfte,     &
            wrf%sr_x,wrf%sr_y,                           & ! atm/fire wrf ratio
            wrf%psfc_stag,                                 &
            this%fire_psfc,1)

         call wrf%interpolate_z2fire(                    &
            this%ifds, this%ifde, this%jfds, this%jfde,  & ! fire this dimensions
            this%ifms, this%ifme, this%jfms, this%jfme,  &
            this%ifts,this%ifte,this%jfts,this%jfte,     &
            wrf%sr_x,wrf%sr_y,                           & ! atm/fire wrf ratio
            wrf%rainc_stag+wrf%rainnc_stag,                                 &
            this%fire_rain,1)

    end subroutine Interpolate_vars_atm_to_fire

    subroutine Init_fxlatfxlong (this, geogrid)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT

      implicit none

      class (state_fire_t), intent (in out) :: this
      type (geogrid_t), intent(in), optional :: geogrid

      real, dimension(:,:), allocatable :: xlat, xlong
      integer ids,ide,jds,jde,ims,ime,jms,jme,its,ite,jts,jte


      ids = geogrid%ids
      ide = geogrid%ide
      jds = geogrid%jds
      jde = geogrid%jde

      ims = geogrid%ids - N_POINTS_IN_HALO
      ime = geogrid%ide + N_POINTS_IN_HALO
      jms = geogrid%jds - N_POINTS_IN_HALO
      jme = geogrid%jde + N_POINTS_IN_HALO

      its = ids
      ite = ide
      jts = jds
      jte = jde

      allocate (xlat(ims:ime,jms:jme))
      allocate (xlong(ims:ime,jms:jme))

      xlat = 0.
      xlong = 0.
      xlat(ids:ide-1,jds:jde-1) = geogrid%xlat
      xlong(ids:ide-1,jds:jde-1) = geogrid%xlong

      call interpolate_z2fire(ids,ide, jds,jde,       &
          ims,ime, jms,jme, its,ite, jts,jte,         &
          this%ifds,this%ifde, this%jfds,this%jfde,   & ! fire this dimensions
          this%ifms,this%ifme, this%jfms,this%jfme,   &
          this%ifts,this%ifte, this%jfts,this%jfte,   &
          geogrid%sr_x, geogrid%sr_y,                 & ! atm/fire this ratio
          xlat,this%fxlat,0)

      call interpolate_z2fire(ids,ide, jds,jde,       &
          ims,ime, jms,jme, its,ite, jts,jte,         &
          this%ifds,this%ifde, this%jfds,this%jfde,   & ! fire this dimensions
          this%ifms,this%ifme, this%jfms,this%jfme,   &
          this%ifts,this%ifte, this%jfts,this%jfte,   &
          geogrid%sr_x,geogrid%sr_y,                  & ! atm/fire this ratio
          xlong,this%fxlong,0)

      deallocate(xlat, xlong)

    end subroutine Init_fxlatfxlong

    subroutine Continue_at_boundary(ix,iy,bias, & ! do x direction or y direction
          ims,ime,jms,jme, &                ! memory dims
          ids,ide,jds,jde, &                ! domain dims
          its,ite,jts,jte, &                ! tile dims
          itso,iteo,jtso,jteo, &            ! tile dims where set
          lfn)
                              ! array
      implicit none
      !*** description
      ! extend array by one beyond the domain by linear continuation
      !*** arguments
      integer, intent(in) :: ix,iy               ! not 0 = do x or y (1 or 2) direction
      real, intent(in) :: bias                   ! 0=none, 1.=max
      integer, intent(in) :: ims,ime,jms,jme, &  ! memory dims
                             ids,ide,jds,jde, &  ! domain dims
                             its,ite,jts,jte     ! tile dims
      integer, intent(out) :: itso,jtso,iteo,jteo  ! where set
      real, intent(inout), dimension(ims:ime,jms:jme) :: lfn
      !*** local
      integer i,j
      character(len=128)::msg
      integer::its1,ite1,jts1,jte1
      integer,parameter::halo=1     ! only 1 domain halo is needed since ENO1 is used near domain boundaries
      !*** executable

      ! for dislay only
      itso = its
      jtso = jts
      iteo = ite
      jteo = jte
      ! go halo width beyond if at patch boundary but not at domain boudnary
      ! assume we have halo need to compute the value we do not have
      ! the next thread that would conveniently computer the extended values at patch corners
      ! besides halo may not transfer values outside of the domain

      its1 = its
      jts1 = jts
      ite1 = ite
      jte1 = jte
      if(.not.its.eq.ids)its1=its-halo
      if(.not.jts.eq.jds)jts1=jts-halo
      if(.not.ite.eq.ide)ite1=ite+halo
      if(.not.jte.eq.jde)jte1=jte+halo
      !$OMP CRITICAL(FIRE_UTIL_CRIT)
      write(msg,'(a,2i5,a,f5.2)')'continue_at_boundary: directions',ix,iy,' bias ',bias
!      call message(msg)
      !$OMP END CRITICAL(FIRE_UTIL_CRIT)
      if(ix.ne.0)then
          if(its.eq.ids)then
              do j=jts1,jte1
                  lfn(ids-1,j)=EX(lfn(ids,j),lfn(ids+1,j))
              enddo
              itso=ids-1
          endif
          if(ite.eq.ide)then
              do j=jts1,jte1
                  lfn(ide+1,j)=EX(lfn(ide,j),lfn(ide-1,j))
              enddo
              iteo=ide+1
          endif
      !$OMP CRITICAL(FIRE_UTIL_CRIT)
          write(msg,'(8(a,i5))')'continue_at_boundary: x:',its,':',ite,',',jts,':',jte,' ->',itso,':',iteo,',',jts1,':',jte1
!          call message(msg)
      !$OMP END CRITICAL(FIRE_UTIL_CRIT)
      endif
      if(iy.ne.0)then
          if(jts.eq.jds)then
              do i=its1,ite1
                  lfn(i,jds-1)=EX(lfn(i,jds),lfn(i,jds+1))
              enddo
              jtso=jds-1
          endif
          if(jte.eq.jde)then
              do i=its1,ite1
                  lfn(i,jde+1)=EX(lfn(i,jde),lfn(i,jde-1))
              enddo
              jteo=jde+1
          endif
      !$OMP CRITICAL(FIRE_UTIL_CRIT)
          write(msg,'(8(a,i5))')'continue_at_boundary: y:',its,':',ite,',',jts,':',jte,' ->',its1,':',ite1,',',jtso,':',jteo
      !$OMP END CRITICAL(FIRE_UTIL_CRIT)
!          call message(msg)
      endif
      ! corners of the domain
      if(ix.ne.0.and.iy.ne.0)then
          if(its.eq.ids.and.jts.eq.jds)lfn(ids-1,jds-1)=EX(lfn(ids,jds),lfn(ids+1,jds+1))
          if(its.eq.ids.and.jte.eq.jde)lfn(ids-1,jde+1)=EX(lfn(ids,jde),lfn(ids+1,jde-1))
          if(ite.eq.ide.and.jts.eq.jds)lfn(ide+1,jds-1)=EX(lfn(ide,jds),lfn(ide-1,jds+1))
          if(ite.eq.ide.and.jte.eq.jde)lfn(ide+1,jde+1)=EX(lfn(ide,jde),lfn(ide-1,jde-1))
      endif
      return

      contains
        real function EX(a,b)
        !*** statement function
        real a,b
        EX=(1.-bias)*(2.*a-b)+bias*max(2.*a-b,a,b)   ! extrapolation, max quarded
        end function EX
    end subroutine Continue_at_boundary

    subroutine Interpolate_z2fire( &
          ids,ide, jds,jde,        &
          ims,ime, jms,jme,        &
          its,ite, jts,jte,        &
          ifds,ifde, jfds,jfde,    & ! fire grid dimensions
          ifms,ifme, jfms,jfme,    &
          ifts,ifte, jfts,jfte,    &
          ir, jr,                  & ! atm/fire grid ratio
          zs,                      & ! atm grid arrays in
          zsf, flag_z0)              ! fire grid arrays out

      implicit none
      !*** purpose: interpolate height or any other 2d variable defined at mesh cell centers

      !*** arguments
      integer, intent(in) :: ids,ide, jds,jde,         &
                             ims,ime, jms,jme,         &
                             its,ite, jts,jte,         &
                             ifds,ifde, jfds,jfde,     & ! fire domain bounds
                             ifms,ifme, jfms,jfme,     & ! fire memory bounds
                             ifts,ifte, jfts,jfte,     & ! fire tile bounds
                             ir, jr                        ! atm/fire grid refinement ratio
      real, intent(in), dimension(ims:ime, jms:jme) :: zs  ! terrain height at atm cell centers & ! terrain height
      real,intent(out), dimension(ifms:ifme, jfms:jfme) :: &
          zsf                                              ! terrain height fire grid nodes
      integer,intent(in) :: flag_z0


      !*** local
      real, dimension(its-2:ite+2,jts-2:jte+2) :: za      ! terrain height
      integer :: i,j,jts1,jte1,its1,ite1,jfts1,jfte1,ifts1,ifte1,itso,jtso,iteo,jteo

      ! terrain height

      jts1 = max(jts-1, jds) ! lower loop limit by one less when at end of domain
      its1 = max(its-1, ids) ! ASSUMES THE HALO IS THERE if patch != domain
      jte1 = min(jte+1, jde)
      ite1 = min(ite+1, ide)

      do j = jts1,jte1
          do i = its1,ite1
              ! copy to local array
              za(i, j) = zs(i, j)
          enddo
      enddo

      call continue_at_boundary(1,1,0., & ! do x direction or y direction
      its-2,ite+2,jts-2,jte+2,           &                ! memory dims
      ids,ide,jds,jde, &            ! domain dims - winds defined up to +1
      its1,ite1,jts1,jte1, &                ! tile dims
      itso,jtso,iteo,jteo, &
      za)                               ! array

      ! interpolate to tile plus strip along domain boundary if at boundary
      jfts1 = snode(jfts, jfds, -1) ! lower loop limit by one less when at end of domain
      ifts1 = snode(ifts, ifds, -1)
      jfte1 = snode(jfte, jfde, +1)
      ifte1 = snode(ifte, ifde, +1)

      call interpolate_2d(  &
          its-2,ite+2,jts-2,jte+2,     & ! memory dims atm grid tile
          its1-1,ite1+1,jts1-1,jte1+1, & ! where atm grid values set
          ifms,ifme,jfms,jfme,         & ! array dims fire grid
          ifts1,ifte1,jfts1,jfte1,     & ! dimensions fire grid tile
          ir,jr,                       & ! refinement ratio
          real(ids),real(jds),ifds+(ir-1)*0.5,jfds+(jr-1)*0.5, & ! line up by lower left corner of domain
          za,                     & ! in atm grid
          zsf)                      ! out fire grid

      if (flag_z0 .eq. 1 ) then
        do j=jfts1,jfte1
          do i=ifts1,ifte1
            zsf(i, j) = max(zsf(i, j), 0.001)
          enddo
        enddo
      endif

    contains

      ! function to go beyond domain boundary if tile is next to it
      pure integer function snode(t,d,i)
        implicit none
        integer, intent(in)::t,d,i
        if(t.ne.d)then
            snode=t
        else
            snode=t+i
        endif
      end function snode

      subroutine interpolate_2d(  &
            ims2,ime2,jms2,jme2, & ! array coarse grid
            its2,ite2,jts2,jte2, & ! dimensions coarse grid
            ims1,ime1,jms1,jme1, & ! array coarse grid
            its1,ite1,jts1,jte1, & ! dimensions fine grid
            ir,jr,               & ! refinement ratio
            rip2,rjp2,rip1,rjp1, & ! (rip2,rjp2) on grid 2 lines up with (rip1,rjp1) on grid 1
            v2, &                  ! in coarse grid
            v1  )                  ! out fine grid
        implicit none

        !*** purpose
        ! interpolate nodal values in mesh2 to nodal values in mesh1
        ! interpolation runs over the mesh2 region its2:ite2,jts2:jte2
        ! only the part of mesh 1 in the region its1:ite1,jts1:jte1 is modified

        !*** arguments

        integer, intent(in)::its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
        integer, intent(in)::its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
        integer, intent(in)::ir,jr
        real,intent(in):: rjp1,rip1,rjp2,rip2
        real, intent(out)::v1(ims1:ime1,jms1:jme1)
        real, intent(in)::v2(ims2:ime2,jms2:jme2)

        !*** local
        integer:: i1,i2,j1,j2,is,ie,js,je
        real:: tx,ty,rx,ry
        real:: rio,rjo
        intrinsic::ceiling,floor


        !*** executable

        ! compute mesh ratios
        rx=1./ir
        ry=1./jr

        do j2=jts2,jte2-1             ! loop over mesh 2 cells
          rjo=rjp1+jr*(j2-rjp2)       ! mesh 1 coordinate of the mesh 2 patch start
          js=max(jts1,ceiling(rjo))   ! lower bound of mesh 1 patch for this mesh 2 cell
          je=min(jte1,floor(rjo)+jr)  ! upper bound of mesh 1 patch for this mesh 2 cell
          do i2=its2,ite2-1
            rio=rip1+ir*(i2-rip2)
            is=max(its1,ceiling(rio))
            ie=min(ite1,floor(rio)+ir)
            do j1=js,je
              ty = (j1-rjo)*ry
              do i1=is,ie
                ! in case mesh 1 node lies on the boundary of several mesh 2 cells
                ! the result will be written multiple times with the same value
                ! up to a rounding error
                tx = (i1-rio)*rx
                !print *,'coarse ',i2,j2,'to',i2+1,j2+1,' fine ',is,js,' to ',ie,je
                v1(i1,j1)=                     &
                      (1-tx)*(1-ty)*v2(i2,j2)  &
                 +    (1-tx)*ty  *v2(i2,j2+1)  &
                 +      tx*(1-ty)*v2(i2+1,j2)  &
                 +        tx*ty  *v2(i2+1,j2+1)
              enddo
            enddo
          enddo
        enddo

      end subroutine interpolate_2d

    end subroutine Interpolate_z2fire

    subroutine Interpolate_wind3d (this, config_flags,    & ! for debug output, <= 0 no output
          fire_wind_height,                               & ! interpolation height
          ifds, ifde, kfds, kfde, jfds, jfde,             & ! fire grid dimensions
          u3d,v3d,                                        & ! atm grid arrays in
          phl,                                            &
          u2d,v2d,z0f)                                      ! fire grid arrays out

      implicit none

      class (state_fire_t), intent(in) :: this
      type (namelist_t), intent(in) :: config_flags
      real, intent(in) :: fire_wind_height                  ! height above the terrain for vertical interpolation
      integer, intent(in) ::                              &
          ifds,ifde, kfds,kfde, jfds,jfde ! fire domain bounds

      real,intent(in)::u3d(:),v3d(:), & ! atm wind velocity, staggered
          phl(:)                                   ! geopotential
      real,intent(out):: u2d,v2d    ! wind velocity fire grid nodes
      real,intent(in):: z0f          ! roughness length in fire grid

      !*** local
      real, dimension(kfds:kfde-1):: altw, hgt
      integer:: k
      integer::kdmax
      real:: loght,loglast,logz0,logfwh,ht
      real::r_nan
      integer::i_nan
      equivalence (i_nan,r_nan)
      real::fire_wind_height_local,z0fc
      real::ust_d,wsf,wsf1,uf_temp,vf_temp
      real,parameter::vk_kappa=0.4

      !*** executable
      ! debug init local arrays
      i_nan=2147483647
      u2d=r_nan
      v2d=r_nan
      altw=r_nan
      hgt=r_nan

      !                            ^ j
      !        ------------        |
      !        |          |         ----> i
      !        u    p     |
      !        |          |    nodes in cell (i,j)
      !        ------v-----    view from top
      !
      !             v(ide,jde+1)
      !            -------x------
      !            |            |
      !            |            |
      ! u(ide,jde) x      x     x u(ide+1,jde)
      !            | p(ide,hde) |
      !            |            |   p=ph,phb,z0,...
      !            -------x------
      !              v(ide,jde)
      !
      ! staggered values set u(ids:ide+1,jds:jde) v(ids:ide,jds:jde+1)
      ! p=ph+phb set at (ids:ide,jds:jde)
      ! location of u(i,j) needs p(i-1,j) and p(i,j)
      ! location of v(i,j) needs p(i,j-1) and p(i,j)
      ! *** NOTE need HALO in ph, phb
      ! so we can compute only u(ids+1:ide,jds:jde) v(ids:ide,jds+1,jde)
      ! unless we extend p at the boundary
      ! but because we care about the fire way in the inside it does not matter
      ! if the fire gets close to domain boundary the simulation is over anyway

      kdmax=kfde-2   ! max layer to interpolate from, can be less
      do k = kfds,kdmax+1
        altw(k) = phl(k) / G             ! altitude of the bottom w-point
      enddo

      do k = kfds,kdmax
        hgt(k) = 0.5 * (altw(k)+ altw(k+1)) - altw(kfds) ! height of the mass point above the ground
      enddo

      ! DME
      if (config_flags%fire_lsm_zcoupling) then
        logfwh = log(config_flags%fire_lsm_zcoupling_ref)
        fire_wind_height_local = config_flags%fire_lsm_zcoupling_ref
      else
        logfwh = log(fire_wind_height)
        fire_wind_height_local = fire_wind_height
      endif

       ! interpolate u
      if(fire_wind_height_local > z0f)then
        do k=kfds,kdmax
          ht = hgt(k)      ! height of this m point above the ground
          if( .not. ht < fire_wind_height_local) then ! found layer k this point is in
            loght = log(ht)
            if(k.eq.kfds)then               ! first layer, log linear interpolation from 0 at zr
              logz0 = log(z0f)
              u2d= u3d(k)*(logfwh-logz0)/(loght-logz0)
            else                           ! log linear interpolation
              loglast=log(hgt(k-1))
              u2d= u3d(k-1) + (u3d(k) - u3d(k-1)) * ( logfwh - loglast) / (loght - loglast)
            endif
            goto 10
          endif
          if(k.eq.kdmax)then                 ! last layer, still not high enough
            u2d=u3d(k)
          endif
        enddo
      10 continue
      else  ! roughness higher than the fire wind height
        u2d=0.
      endif

       ! interpolate v
      if(fire_wind_height_local > z0f)then       !
        do k=kfds,kdmax
          ht = hgt(k)      ! height of this u point above the ground
          if( .not. ht < fire_wind_height_local) then ! found layer k this point is in
            loght = log(ht)
            if(k.eq.kfds)then               ! first layer, log linear interpolation from 0 at zr
              logz0 = log(z0f)
              v2d= v3d(k)*(logfwh-logz0)/(loght-logz0)
            else                           ! log linear interpolation
              loglast=log(hgt(k-1))
              v2d= v3d(k-1) + (v3d(k) - v3d(k-1)) * ( logfwh - loglast) / (loght - loglast)
            endif
            goto 11
          endif
          if(k.eq.kdmax)then                 ! last layer, still not high enough
            v2d=v3d(k)
          endif
        enddo
        11 continue
      else  ! roughness higher than the fire wind height
        v2d=0.
      endif

      ! DME here code to extrapolate mid-flame height velocity -> fire_lsm_zcoupling = .true.
      if (config_flags%fire_lsm_zcoupling) then
            uf_temp=u2d
            vf_temp=v2d
            wsf=max(sqrt(uf_temp**2.+vf_temp**2.),0.1)
            z0fc=z0f
            ust_d=wsf*vk_kappa/log(config_flags%fire_lsm_zcoupling_ref/z0fc)
            wsf1=(ust_d/vk_kappa)*log((fire_wind_height+z0fc)/z0fc)
            u2d=wsf1*uf_temp/wsf
            v2d=wsf1*vf_temp/wsf
      endif

      return

    end subroutine Interpolate_wind3d

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

    subroutine Provide_atm_feedback (this, wrf, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (state_fire_t), intent(in out) :: this
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
                  this%ifts,this%ifte,this%jfts,this%jfte,         &
                  wrf%ids,wrf%ide,wrf%kds,wrf%kde,wrf%jds,wrf%jde, &
                  wrf%ims,wrf%ime,wrf%kms,wrf%kme,wrf%jms,wrf%jme, &
                  wrf%ips,wrf%ipe,wrf%kps,wrf%kpe,wrf%jps,wrf%jpe, &
                  wrf%rho_stag,wrf%dz8w_stag,                      &
                  wrf%tracer(:,:,:,p_fire_smoke),this%emis_smoke)
      end if

    end subroutine Provide_atm_feedback

    subroutine Save_state (this)

      implicit none

      class (state_fire_t), intent (in) :: this

      character (len = :), allocatable :: file_output


      file_output='fire_output_'//this%datetime_now%datetime//'.nc'

      call Create_netcdf_file (file_name = file_output)

      call Add_netcdf_dim (file_output, 'nx', this%nx)
      call Add_netcdf_dim (file_output, 'ny', this%ny)

      call Add_netcdf_var (file_output, ['nx', 'ny'], 'test', this%test(1:this%nx, 1:this%ny))

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

    end subroutine Save_state

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

      class (state_fire_t), intent(in out) :: this        ! fire state
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

      s = 1./(atm%sr_x*atm%sr_y)
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

