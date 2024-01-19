  module state_mod

    use, intrinsic :: iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT

    use namelist_mod, only : namelist_t, NUM_FMC
    use geogrid_mod, only : geogrid_t
    use proj_lc_mod, only : proj_lc_t
    use datetime_mod, only : datetime_t
    use netcdf_mod, only : Create_netcdf_file, Add_netcdf_dim, Add_netcdf_var
    use wrf_mod, only : wrf_t, G, RERADIUS
    use constants_mod, only : PI
    use stderrout_mod, only: Message

    implicit none

    private

    public :: state_fire_t

    integer, parameter :: NUM_FMEP = 2
    integer, parameter :: N_POINTS_IN_HALO = 5

    type :: state_fire_t
      integer :: ifds, ifde, jfds, jfde, kfds, kfde, ifms, ifme, jfms, jfme, kfms, kfme, &
                 ifts, ifte, jfts, jfte, kfts, kfte, ifps, ifpe, jfps, jfpe, kfps, kfpe
!      real, dimension (:, :), allocatable :: lats, lons, elevations, dz_dxs, dz_dys, fuel_cats
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
      real, dimension(:, :), allocatable :: lfn_out
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

      integer, dimension(:), allocatable :: i_start, i_end, j_start, j_end

      real :: fmoist_lasttime       ! "last time the moisture model was run" "s"
      real :: fmoist_nexttime       ! "next time the moisture model will run" "s"
      real :: dt_moisture           ! Time since moisture model run the last time
      logical :: run_advance_moisture ! Whether the moisture model should be advanced
      
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
      procedure :: Init_latlons => Init_latlons
      procedure :: Interpolate_vars_atm_to_fire => Interpolate_vars_atm_to_fire
      procedure, public :: Interpolate_profile => Interpolate_profile
      procedure, public :: Print => Print_domain ! private
      procedure, public :: Save_state => Save_state
    end type state_fire_t

  contains

    subroutine Handle_output (this, config_flags)

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags

      logical, parameter :: DEBUG_LOCAL = .false.


      if (this%datetime_now == this%datetime_next_output) then
        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Writing output...'
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
        if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) 'Updating wrfdata...'
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
      allocate (this%bbb(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%betafl(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%phiwc(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%r_0(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%fgip(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%ischap(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%iboros(this%ifms:this%ifme, this%jfms:this%jfme))
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
      allocate (this%flame_length(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%ros_front(this%ifms:this%ifme, this%jfms:this%jfme))

        ! Init tign_g a bit into the future
        ! Inits the halo because it is used by fuel_left subroutine
      allocate (this%tign_g(this%ifms:this%ifme, this%jfms:this%jfme))
      this%tign_g(this%ifms:this%ifme, this%jfms:this%jfme) = epsilon (this%tign_g)

      allocate (this%fuel_frac(this%ifms:this%ifme, this%jfms:this%jfme))
      this%fuel_frac(this%ifds:this%ifde, this%jfds:this%jfde) = 1.0

      allocate (this%fire_area(this%ifms:this%ifme, this%jfms:this%jfme))
      this%fire_area(this%ifds:this%ifde, this%jfds:this%jfde) = 0.0

      allocate (this%burnt_area_dt(this%ifms:this%ifme, this%jfms:this%jfme))
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

      if (config_flags%fire_is_real_perim) then
        if (allocated (geogrid%lfn_init)) then
          this%lfn_hist(this%ifds:this%ifde, this%jfds:this%jfde) = geogrid%lfn_init
        else
          write (ERROR_UNIT, *) 'Attenting to initialize fire from given  perimeter but no initialization data present'
          stop
        end if
      end if

      this%unit_fxlat = 2.0 * PI / (360.0 * RERADIUS)  ! earth circumference in m / 360 degrees
      this%unit_fxlong = cos (this%cen_lat * 2.0 * PI / 360.0) * this%unit_fxlat  ! latitude
      call this%Init_latlons (geogrid)

      call Init_tiles ( this, config_flags )

    end subroutine Init_domain

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!! This code is borrowed from module_tiles.F in WRF, specifically SUBROUTINE set_tiles2 !!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine Init_tiles (grid, config_flags)

     implicit none

     class (state_fire_t), intent(in out) :: grid
     type (namelist_t), intent (in) :: config_flags

     !  Local data.

     INTEGER                                :: num_tiles_x, num_tiles_y, num_tiles
     INTEGER                                :: spx, epx, spy, epy, t, tt, ts, te
     INTEGER                                :: smx, emx, smy, emy
     INTEGER                                :: ntiles
     INTEGER                                :: nt
     CHARACTER*255                          :: mess
     logical, parameter                     :: DEBUG_LOCAL = .false.

     ! We are using DATA_ORDER_XZY
     spx = grid%ifps ; epx = grid%ifpe ; spy = grid%jfps ; epy = grid%jfpe
     smx = grid%ifms ; emx = grid%ifme ; smy = grid%jfms ; emy = grid%jfme

     ! Check to make sure patches are within memory bounds
     call error_test_lt(grid%ifps,smx)
     call error_test_gt(grid%ifpe,emx)
     call error_test_lt(grid%jfps,smy)
     call error_test_gt(grid%jfpe,emy)

     num_tiles   = config_flags%num_tiles
     num_tiles_x = 0
     num_tiles_y = 0

     ALLOCATE(grid%i_start(num_tiles))
     ALLOCATE(grid%i_end(num_tiles))
     ALLOCATE(grid%j_start(num_tiles))
     ALLOCATE(grid%j_end(num_tiles))

     ! Compute number of tiles in x and y based on user-specified total number of tiles
     call least_aspect( num_tiles, 1, 1, num_tiles_y, num_tiles_x )

     ! Make sure number of tiles in x and y are at least 1
     num_tiles_x=max( num_tiles_x , 1)
     num_tiles_y=max( num_tiles_y , 1)

     ! Compute start and end tile indices
     nt = 1
     DO t = 0, num_tiles-1

       ! do y
        ntiles = t / num_tiles_x
        CALL region_bounds( spy, epy,                                  &
                            num_tiles_y, ntiles,                       &
                            ts, te )
        ! first y (major dimension)
        IF ( ts .LE. te ) THEN  ! converse happens if number of tiles > number of points in dim
!!!
! This bit allows the user to specify execution out onto the halo region
! in the call to set_tiles. If the low patch boundary specified by the arguments
! is less than what the model already knows to be the patch boundary and if
! the user hasn't erred by specifying something that would fall off memory
! (safety tests are higher up in this routine, outside the IF) then adjust
! the tile boundary of the low edge tiles accordingly. Likewise for high edges.
!          IF ( jps .lt. spy .and. ts .eq. spy ) ts = jps ;
!          IF ( jpe .gt. epy .and. te .eq. epy ) te = jpe ;
!
          grid%j_start(nt) = max ( ts , grid%jfds )
          grid%j_end(nt)   = min ( te , grid%jfde )

          ! now x
          ntiles = mod(t,num_tiles_x)
          CALL region_bounds( spx, epx,                                  &
                              num_tiles_x, ntiles,                       &
                              ts, te )
          IF ( ts .LE. te ) THEN  ! converse happens if number of tiles > number of points in dim
!            IF ( ips .lt. spx .and. ts .eq. spx ) ts = ips ;
!            IF ( ipe .gt. epx .and. te .eq. epx ) te = ipe ;
!!!
            grid%i_start(nt) = max ( ts , grid%ifds )
            grid%i_end(nt)   = min ( te , grid%ifde )
            if (DEBUG_LOCAL) then
              WRITE(OUTPUT_UNIT,'("WRF-FIRE TILE ",I3," IS ",I6," IE ",I6," JS ",I6," JE ",I6)') &
                        nt,grid%i_start(nt),grid%i_end(nt),grid%j_start(nt),grid%j_end(nt)
            endif
            nt = nt + 1
          ENDIF
        ENDIF
     END DO
     num_tiles = nt-1

     if (DEBUG_LOCAL) then
       WRITE(OUTPUT_UNIT,'("WRF-FIRE NUMBER OF TILES = ",I3," TOTAL, ",I3," IN X, ",I3," IN Y")')num_tiles,num_tiles_x,num_tiles_y
     endif

     grid%num_tiles = num_tiles

     contains

    subroutine error_test_lt (A,B)

      implicit none

      integer, intent (in) :: A, B

      if (A .lt. B) then
        write (ERROR_UNIT, *)
        write (ERROR_UNIT, *) 'FATAL ERROR: PATCH IS OUT OF MEMORY BOUNDS!'
        stop
      endif
     
    end subroutine error_test_lt

    subroutine error_test_gt (A,B)

      implicit none

      integer, intent (in) :: A, B

      if (A .gt. B) then
        write (ERROR_UNIT, *)
        write (ERROR_UNIT, *) 'FATAL ERROR: PATCH IS OUT OF MEMORY BOUNDS!'
        stop
      endif

    end subroutine error_test_gt

    end subroutine Init_tiles

    SUBROUTINE least_aspect( nparts, minparts_y, minparts_x, nparts_y, nparts_x )
    IMPLICIT NONE
    !  Input data.
    INTEGER, INTENT(IN)           :: nparts,                &
                                     minparts_y, minparts_x
    ! Output data.
    INTEGER, INTENT(OUT)          :: nparts_y, nparts_x
    ! Local data.
    INTEGER                       :: x, y, mini
    mini = 2*nparts
    nparts_y = 1
    nparts_x = nparts
    DO y = 1, nparts
       IF ( mod( nparts, y ) .eq. 0 ) THEN
          x = nparts / y
          IF (       abs( y-x ) .LT. mini       &
               .AND. y .GE. minparts_y                &
               .AND. x .GE. minparts_x    ) THEN
             mini = abs( y-x )
             nparts_y = y
             nparts_x = x
          END IF
       END IF
    END DO
    END SUBROUTINE least_aspect

    SUBROUTINE region_bounds( region_start, region_end, &
                             num_p, p,                 &
                             patch_start, patch_end )
   ! 1-D decomposition routine: Given starting and ending indices of a
   ! vector, the number of patches dividing the vector, and the number of
   ! the patch, give the start and ending indices of the patch within the
   ! vector.  This will work with tiles too.  Implementation note.  This is
   ! implemented somewhat inefficiently, now, with a loop, so we can use the
   ! locproc function above, which returns processor number for a given
   ! index, whereas what we want is index for a given processor number.
   ! With a little thought and a lot of debugging, we can come up with a
   ! direct expression for what we want.  For time being, we loop...
   ! Remember that processor numbering starts with zero.

   IMPLICIT NONE
   INTEGER, INTENT(IN)                    :: region_start, region_end, num_p, p
   INTEGER, INTENT(OUT)                   :: patch_start, patch_end
   INTEGER                                :: offset, i
   patch_end = -999999999
   patch_start = 999999999
   offset = region_start
   do i = 0, region_end - offset
     if ( locproc( i, region_end-region_start+1, num_p ) == p ) then
       patch_end = max(patch_end,i)
       patch_start = min(patch_start,i)
     endif
   enddo
   patch_start = patch_start + offset
   patch_end   = patch_end   + offset
   RETURN
   END SUBROUTINE region_bounds

   RECURSIVE SUBROUTINE rlocproc(p,maxi,nproc,ml,mr,ret)
   IMPLICIT NONE
   INTEGER, INTENT(IN)  :: p, maxi, nproc, ml, mr
   INTEGER, INTENT(OUT) :: ret
   INTEGER              :: width, rem, ret2, bl, br, mid, adjust, &
                           p_r, maxi_r, nproc_r, zero
   adjust = 0
   rem = mod( maxi, nproc )
   width = maxi / nproc
   mid = maxi / 2
   IF ( rem>0 .AND. (((mod(rem,2).EQ.0).OR.(rem.GT.2)).OR.(p.LE.mid))) THEN
     width = width + 1
   END IF
   IF ( p.LE.mid .AND. mod(rem,2).NE.0 ) THEN
     adjust = adjust + 1
   END IF
   bl = max(width,ml) ;
   br = max(width,mr) ;
   IF      (p<bl) THEN
     ret = 0
   ELSE IF (p>maxi-br-1) THEN
     ret = nproc-1
   ELSE
     p_r = p - bl
     maxi_r = maxi-bl-br+adjust
     nproc_r = max(nproc-2,1)
     zero = 0
     CALL rlocproc( p_r, maxi_r, nproc_r, zero, zero, ret2 )  ! Recursive
     ret = ret2 + 1
   END IF
   RETURN
   END SUBROUTINE rlocproc

   INTEGER FUNCTION locproc( i, m, numpart )
   implicit none
   integer, intent(in) :: i, m, numpart
   integer             :: retval, ii, im, inumpart, zero
   ii = i
   im = m
   inumpart = numpart
   zero = 0
   CALL rlocproc( ii, im, inumpart, zero, zero, retval )
   locproc = retval
   RETURN
   END FUNCTION locproc

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
            call this%interpolate_profile (config_flags,       & ! for debug output, <= 0 no output
                config_flags%fire_wind_height,                 & ! interpolation height
                this%kfds, this%kfde,                          & ! fire grid dimensions
                wrf%u3d_stag(i,:,j),wrf%v3d_stag(i,:,j),       & ! atm grid arrays in
                wrf%phl_stag(i,:,j),                           &
                wrf%ua(i,j),wrf%va(i,j),wrf%z0_stag(i,j))
          enddo
        enddo

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

    subroutine Interpolate_profile (this, config_flags,    & ! for debug output, <= 0 no output
          fire_wind_height,                               & ! interpolation height
          kfds, kfde,             & ! fire grid dimensions
          uin,vin,                                        & ! atm grid arrays in
          phl,                                            &
          uout,vout,z0f)                                      ! fire grid arrays out

      implicit none

      class (state_fire_t), intent(in) :: this
      type (namelist_t), intent(in) :: config_flags
      real, intent(in) :: fire_wind_height                  ! height above the terrain for vertical interpolation
      integer, intent(in) :: kfds, kfde ! fire domain bounds
      real, intent(in) :: uin(:), vin(:), phl(:) ! staggered atm wind velocity, and geopoential heigh
      real, intent(out) :: uout, vout    ! wind velocity fire grid nodes
      real, intent(in) :: z0f          ! roughness length in fire grid

      !*** local
      real, dimension(kfds:kfde-1) :: altw, hgt
      integer :: k
      integer :: kdmax
      real :: loght, loglast, logz0, logfwh, ht
      real :: r_nan
      real :: fire_wind_height_local, z0fc
      real :: ust_d, wsf, wsf1, uf_temp, vf_temp
      real, parameter :: vk_kappa = 0.4


      !*** executable
      kdmax = kfde - 2          ! max layer to interpolate from, can be less
      do k = kfds, kdmax + 1
        altw(k) = phl(k) / G    ! altitude of the bottom w-point
      enddo

      do k = kfds, kdmax
        hgt(k) = 0.5 * (altw(k) + altw(k+1)) - altw(kfds) ! height of the mass point above the ground
      enddo

      ! extrapolate mid-flame height from fire_lsm_zcoupling_ref?
      if (config_flags%fire_lsm_zcoupling) then
        logfwh = log(config_flags%fire_lsm_zcoupling_ref)
        fire_wind_height_local = config_flags%fire_lsm_zcoupling_ref
      else
        logfwh = log(fire_wind_height)
        fire_wind_height_local = fire_wind_height
      endif

      ! interpolate u
      if (fire_wind_height_local > z0f)then
        do k= kfds, kdmax
          ht = hgt(k) ! height of this mass point above the ground
          if(ht >= fire_wind_height_local) then ! found layer k this point is in
            loght = log(ht)
            if(k == kfds)then               ! first layer, log linear interpolation from 0 at zr
              logz0 = log(z0f)
              uout = uin(k) * (logfwh - logz0) / (loght - logz0)
              vout = vin(k) * (logfwh - logz0) / (loght - logz0)
            else                           ! log linear interpolation
              loglast = log(hgt(k-1))
              uout = uin(k-1) + (uin(k) - uin(k-1)) * (logfwh - loglast) / (loght - loglast)
              vout = vin(k-1) + (vin(k) - vin(k-1)) * (logfwh - loglast) / (loght - loglast)
            endif
            exit
          endif
          if(k.eq.kdmax)then                 ! last layer, still not high enough
            uout = uin(k)
            vout = vin(k)
          endif
        enddo
      else  ! roughness higher than the fire wind height
        uout = 0.0
        vout = 0.0
      endif

      ! DME here code to extrapolate mid-flame height velocity -> fire_lsm_zcoupling = .true.
      if (config_flags%fire_lsm_zcoupling) then
        uf_temp=uout
        vf_temp=vout
        wsf = max(sqrt(uf_temp ** 2. + vf_temp ** 2.), 0.1)
        z0fc = z0f
        ust_d = wsf * vk_kappa / log(config_flags%fire_lsm_zcoupling_ref / z0fc)
        wsf1 = (ust_d / vk_kappa) * log((fire_wind_height + z0fc) / z0fc)
        uout = wsf1 * uf_temp / wsf
        vout = wsf1 * vf_temp / wsf
      endif

    end subroutine Interpolate_profile

    subroutine Print_domain (this)

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
      call Add_netcdf_var (file_output, ['nx', 'ny'], 'burnt_area_dt', this%burnt_area_dt(1:this%nx, 1:this%ny))
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

