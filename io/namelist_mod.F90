  module namelist_mod

    implicit none

    private

    public :: namelist_t, NUM_FMC, FIRE_MAX_IGNITIONS_IN_NAMELIST

    integer, parameter :: NUM_FMC = 5
    integer, parameter :: FIRE_MAX_IGNITIONS_IN_NAMELIST = 5

    type :: namelist_t
      integer :: start_year = -1, start_month = -1, start_day = -1, start_hour = -1, start_minute = -1, start_second = -1, &
          end_year = -1, end_month = -1, end_day = -1, end_hour = -1, end_minute = -1, end_second = -1, interval_output = -1, &
          interval_atm = -1
      integer :: num_tiles = 1, num_tiles_x = 1, num_tiles_y = 1
      real :: dt = 2.0
      integer :: fire_print_msg = 0           ! "write fire statistics, 0 no writes, 1+ for more"  ""
      real :: fire_atm_feedback = 1.0         ! "the heat fluxes to the atmosphere are multiplied by this" "1"
      integer :: fire_boundary_guard = 8      ! "cells to stop when fire close to domain boundary"
      integer :: fire_upwinding = 9           ! "upwind normal spread: 1=standard, 2=godunov, 3=eno, 4=sethian, 5=2nd-order,
                                              ! 6=WENO3, 7=WENO5, 8=hybrid WENO3/ENO1, 9=hybrid WENO5/ENO1" "1"
      real :: fire_viscosity = 0.4            ! "artificial viscosity in level set method" "1"
      real :: fire_lfn_ext_up = 1.0           ! "0.=extend level set function at boundary by reflection, 1.=always up" "1"
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
      integer :: fmoist_freq = 0              ! "frequency to run moisture model 0: use fmoist_dt, k>0: every k timesteps" "1"
      real :: fmoist_dt = 600                 ! "moisture model time step" "s"
      real :: fire_ext_grnd = 50.0            ! "extinction depth of sfc fire heat"   "m"
      real :: fire_ext_crwn = 50.0            ! "extinction depth of crown fire heat" "m"
      real :: fire_crwn_hgt = 15.0            ! "height that heat from crown fire is released" "m"
      real :: fire_wind_height = 6.096        ! "height of uah,vah wind in fire spread formula" "m"
      logical :: fire_is_real_perim = .false. ! .false. = point/line ignition, .true. = observed perimeter"
      integer :: nfmc = NUM_FMC               ! "number of fuel moisture classes" related to NUM_NFMC
      real :: fmep_decay_tlag = 999999        ! "time constant of assimilated adjustments of equilibria decay" "1"
      integer :: tracer_opt = 0               ! 3) Activates smoke tracer
      real :: fire_tracer_smoke = 0.02        ! "parts per unit of burned fuel becoming smoke (tracer_opt=3)" "g_smoke/kg_air"
      real :: fuelmc_g = 0.08                 ! Fuel moisture content ground (Dead FMC)
      real :: fuelmc_g_live = 0.30            ! Fuel moisture content ground (Live FMC). 30% Completely cured, treat as dead fuel
      real :: fuelmc_c = 1.00                 ! Fuel moisture content canopy

        ! Ignitions
      integer :: fire_num_ignitions = 0 ! "number of ignition lines"

      real :: fire_ignition_start_lon1 = 0.0  ! "long coord of start of ignition line" "deg"
      real :: fire_ignition_start_lat1 = 0.0  ! "lat coord of start of ignition line" "deg"
      real :: fire_ignition_end_lon1 = 0.0    ! "long coord of end of ignition line" "deg"
      real :: fire_ignition_end_lat1 = 0.0    ! "lat coord of end of ignition line" "deg"
      real :: fire_ignition_ros1 = 0.01       ! "rate of spread during ignition" "m/s"
      real :: fire_ignition_start_time1 = 0.0 ! "ignition line start time" "s"
      real :: fire_ignition_end_time1 = 0.0   ! "ignition line end time" "s"
      real :: fire_ignition_radius1 = 0.0     ! "ignite all within the radius" "m"

      real :: fire_ignition_start_lon2 = 0.0
      real :: fire_ignition_start_lat2 = 0.0
      real :: fire_ignition_end_lon2 = 0.0
      real :: fire_ignition_end_lat2 = 0.0
      real :: fire_ignition_ros2 = 0.01
      real :: fire_ignition_start_time2 = 0.0
      real :: fire_ignition_end_time2 = 0.0
      real :: fire_ignition_radius2 = 0.0

      real :: fire_ignition_start_lon3 = 0.0
      real :: fire_ignition_start_lat3 = 0.0
      real :: fire_ignition_end_lon3 = 0.0
      real :: fire_ignition_end_lat3 = 0.0
      real :: fire_ignition_ros3 = 0.01
      real :: fire_ignition_start_time3 = 0.0
      real :: fire_ignition_end_time3 = 0.0
      real :: fire_ignition_radius3 = 0.0

      real :: fire_ignition_start_lon4 = 0.0
      real :: fire_ignition_start_lat4 = 0.0
      real :: fire_ignition_end_lon4 = 0.0
      real :: fire_ignition_end_lat4 = 0.0
      real :: fire_ignition_ros4 = 0.01
      real :: fire_ignition_start_time4 = 0.0
      real :: fire_ignition_end_time4 = 0.0
      real :: fire_ignition_radius4 = 0.0

      real :: fire_ignition_start_lon5 = 0.0
      real :: fire_ignition_start_lat5 = 0.0
      real :: fire_ignition_end_lon5 = 0.0
      real :: fire_ignition_end_lat5 = 0.0
      real :: fire_ignition_ros5 = 0.01
      real :: fire_ignition_start_time5 = 0.0
      real :: fire_ignition_end_time5 = 0.0
      real :: fire_ignition_radius5 = 0.0

        ! Atmosphere
      integer :: ids = 1, jds = 1, kds = 1, kde = 1
    contains
      procedure, public :: Initialization => Init_namelist
      procedure, public :: Init_fire_block => Init_fire_block
      procedure, public :: Init_time_block => Init_time_block
      procedure, public :: Init_atm_block => Init_atm_block_legacy
    end type namelist_t

  contains

    subroutine Init_atm_block_legacy (this, file_name)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT

      implicit none

      class (namelist_t), intent (in out) :: this
      character (len = *), intent (in) :: file_name

      integer :: kde, interval_atm
      integer, parameter :: MAX_CHAR_LEN = 250
      integer :: unit_nml, io_stat

      namelist /atm/ kde, interval_atm


      interval_atm = 0
        ! The following vars are legacy vars
      kde = 2

      open (newunit = unit_nml, file = trim (file_name), action = 'read', iostat = io_stat)
      if (io_stat /= 0) then
        write (ERROR_UNIT, *) 'Problems opening namelist file ', trim (file_name)
        stop
      end if

      read (unit_nml, nml = atm, iostat = io_stat)
      if (io_stat /= 0) then
        write (ERROR_UNIT, *) 'Problems reading namelist atm block'
        stop
      end if
      close (unit_nml)

      this%interval_atm = interval_atm

        ! Legacy vars
      this%kde = kde

    end subroutine Init_atm_block_legacy

    subroutine Init_fire_block (this, file_name)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT

      implicit none

      class (namelist_t), intent (in out) :: this
      character (len = *), intent (in) :: file_name

      integer :: fire_print_msg = 0           ! "write fire statistics, 0 no writes, 1+ for more"  ""
      real :: fire_atm_feedback = 1.0         ! "the heat fluxes to the atmosphere are multiplied by this" "1"
      integer :: fire_boundary_guard = 8      ! "cells to stop when fire close to domain boundary"
      integer :: fire_upwinding = 9           ! "upwind normal spread: 1=standard, 2=godunov, 3=eno, 4=sethian, 5=2nd-order, 6=WENO3, 7=WENO5, 8=hybrid WENO3/ENO1, 9=hybrid WENO5/ENO1" "1"
      real :: fire_viscosity = 0.4            ! "artificial viscosity in level set method" "1"
      real :: fire_lfn_ext_up = 1.0           ! "0.=extend level set function at boundary by reflection, 1.=always up" "1"
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
      integer :: fmoist_freq = 0              ! "frequency to run moisture model 0: use fmoist_dt, k>0: every k timesteps" "1"
      real :: fmoist_dt = 600                 ! "moisture model time step" "s"
      real :: fire_ext_grnd = 50.0            ! "extinction depth of sfc fire heat"   "m"
      real :: fire_ext_crwn = 50.0            ! "extinction depth of crown fire heat" "m"
      real :: fire_crwn_hgt = 15.0            ! "height that heat from crown fire is released" "m"
      real :: fire_wind_height = 6.096        ! "height of uah,vah wind in fire spread formula" "m"
      logical :: fire_is_real_perim = .false. ! .false. = point/line ignition, .true. = observed perimeter"
      integer :: nfmc = NUM_FMC               ! "number of fuel moisture classes" related to NUM_NFMC
      real :: fmep_decay_tlag = 999999        ! "time constant of assimilated adjustments of equilibria decay" "1"
      integer :: tracer_opt = 0               ! 3) Activates smoke tracer
      real :: fire_tracer_smoke = 0.02        ! "parts per unit of burned fuel becoming smoke (tracer_opt=3)" "g_smoke/kg_air"
      real :: fuelmc_g = 0.08                 ! Fuel moisture content ground (Dead FMC)
      real :: fuelmc_g_live = 0.30            ! Fuel moisture content ground (Live FMC). 30% Completely cured, treat as dead fuel
      real :: fuelmc_c = 1.00                 ! Fuel moisture content canopy

        ! ignitions
      integer :: fire_num_ignitions = 0

      real :: fire_ignition_start_lon1 = 0.0  ! "long coord of start of ignition line" "deg"
      real :: fire_ignition_start_lat1 = 0.0  ! "lat coord of start of ignition line" "deg"
      real :: fire_ignition_end_lon1 = 0.0    ! "long coord of end of ignition line" "deg"
      real :: fire_ignition_end_lat1 = 0.0    ! "lat coord of end of ignition line" "deg"
      real :: fire_ignition_ros1 = 0.01       ! "rate of spread during ignition" "m/s"
      real :: fire_ignition_start_time1 = 0.0 ! "ignition line start time" "s"
      real :: fire_ignition_end_time1 = 0.0   ! "ignition line end time" "s"
      real :: fire_ignition_radius1 = 0.0     ! "ignite all within the radius" "m"

      real :: fire_ignition_start_lon2 = 0.0, fire_ignition_start_lat2 = 0.0, fire_ignition_end_lon2 = 0.0, &
          fire_ignition_end_lat2 = 0.0, &
          fire_ignition_ros2 = 0.01, fire_ignition_start_time2 = 0.0, fire_ignition_end_time2 = 0.0, fire_ignition_radius2 = 0.0

      real :: fire_ignition_start_lon3 = 0.0, fire_ignition_start_lat3 = 0.0, fire_ignition_end_lon3 = 0.0, &
          fire_ignition_end_lat3 = 0.0, &
          fire_ignition_ros3 = 0.01, fire_ignition_start_time3 = 0.0, fire_ignition_end_time3 = 0.0, fire_ignition_radius3 = 0.0

      real :: fire_ignition_start_lon4 = 0.0, fire_ignition_start_lat4 = 0.0, fire_ignition_end_lon4 = 0.0, &
          fire_ignition_end_lat4 = 0.0, &
          fire_ignition_ros4 = 0.01, fire_ignition_start_time4 = 0.0, fire_ignition_end_time4 = 0.0, fire_ignition_radius4 = 0.0

      real :: fire_ignition_start_lon5 = 0.0, fire_ignition_start_lat5 = 0.0, fire_ignition_end_lon5 = 0.0, &
          fire_ignition_end_lat5 = 0.0, &
          fire_ignition_ros5 = 0.01, fire_ignition_start_time5 = 0.0, fire_ignition_end_time5 = 0.0, fire_ignition_radius5 = 0.0

      namelist /fire/  fire_print_msg, fire_atm_feedback, fire_boundary_guard, &
          fire_upwinding, fire_viscosity, fire_lfn_ext_up, fire_advection, fire_lsm_reinit, &
          fire_lsm_reinit_iter, fire_upwinding_reinit, fire_lsm_band_ngp, fire_lsm_zcoupling, fire_lsm_zcoupling_ref, &
          fire_viscosity_bg, fire_viscosity_band, fire_viscosity_ngp, fire_slope_factor, fire_fmc_read, fmoist_run, &
          fmoist_freq, fmoist_dt, fire_ext_grnd, fire_ext_crwn, fire_crwn_hgt, &
          fire_wind_height, fire_is_real_perim, nfmc, fmep_decay_tlag, tracer_opt, fire_tracer_smoke, fuelmc_g, &
          fuelmc_g_live, fuelmc_c, &
            ! Ignitions
          fire_num_ignitions, &
            ! Ignition 1
          fire_ignition_start_lon1, fire_ignition_start_lat1, fire_ignition_end_lon1, fire_ignition_end_lat1, &
          fire_ignition_ros1, fire_ignition_start_time1, fire_ignition_end_time1, fire_ignition_radius1, &
            ! Ignition 2
          fire_ignition_start_lon2, fire_ignition_start_lat2, fire_ignition_end_lon2, fire_ignition_end_lat2, &
          fire_ignition_ros2, fire_ignition_start_time2, fire_ignition_end_time2, fire_ignition_radius2, &
            ! Ignition 3
          fire_ignition_start_lon3, fire_ignition_start_lat3, fire_ignition_end_lon3, fire_ignition_end_lat3, &
          fire_ignition_ros3, fire_ignition_start_time3, fire_ignition_end_time3, fire_ignition_radius3, &
            ! Ignition 4
          fire_ignition_start_lon4, fire_ignition_start_lat4, fire_ignition_end_lon4, fire_ignition_end_lat4, &
          fire_ignition_ros4, fire_ignition_start_time4, fire_ignition_end_time4, fire_ignition_radius4, &
            ! Ignition 5
          fire_ignition_start_lon5, fire_ignition_start_lat5, fire_ignition_end_lon5, fire_ignition_end_lat5, &
          fire_ignition_ros5, fire_ignition_start_time5, fire_ignition_end_time5, fire_ignition_radius5

      integer :: unit_nml, io_stat


      open (newunit = unit_nml, file = trim (file_name), action = 'read', iostat = io_stat)
      if (io_stat /= 0) then
        write (ERROR_UNIT, *) 'Problems opening namelist file ', trim (file_name)
        stop
      end if

      read (unit_nml, nml = fire)
      if (io_stat /= 0) then
        write (ERROR_UNIT, *) 'Problems reading namelist fire block'
        stop
      end if

      close (unit_nml)

      this%fire_print_msg = fire_print_msg
      this%fire_atm_feedback = fire_atm_feedback
      this%fire_boundary_guard = fire_boundary_guard
      this%fire_upwinding = fire_upwinding
      this%fire_viscosity = fire_viscosity
      this%fire_lfn_ext_up = fire_lfn_ext_up
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
      this%fmoist_freq = fmoist_freq
      this%fmoist_dt = fmoist_dt
      this%fire_ext_grnd = fire_ext_grnd
      this%fire_ext_crwn = fire_ext_crwn
      this%fire_crwn_hgt = fire_crwn_hgt
      this%fire_wind_height = fire_wind_height
      this%fire_is_real_perim = fire_is_real_perim
      this%nfmc = nfmc
      this%fmep_decay_tlag = fmep_decay_tlag
      this%tracer_opt = tracer_opt
      this%fire_tracer_smoke = fire_tracer_smoke
      this%fuelmc_g = fuelmc_g
      this%fuelmc_g_live = fuelmc_g_live
      this%fuelmc_c = fuelmc_c

      this%fire_num_ignitions = fire_num_ignitions

      this%fire_ignition_start_lon1 = fire_ignition_start_lon1
      this%fire_ignition_start_lat1 = fire_ignition_start_lat1
      this%fire_ignition_end_lon1 = fire_ignition_end_lon1
      this%fire_ignition_end_lat1 = fire_ignition_end_lat1
      this%fire_ignition_ros1 = fire_ignition_ros1
      this%fire_ignition_start_time1 = fire_ignition_start_time1
      this%fire_ignition_end_time1 = fire_ignition_end_time1
      this%fire_ignition_radius1 = fire_ignition_radius1

      this%fire_ignition_start_lon2 = fire_ignition_start_lon2
      this%fire_ignition_start_lat2 = fire_ignition_start_lat2
      this%fire_ignition_end_lon2 = fire_ignition_end_lon2
      this%fire_ignition_end_lat2 = fire_ignition_end_lat2
      this%fire_ignition_ros2 = fire_ignition_ros2
      this%fire_ignition_start_time2 = fire_ignition_start_time2
      this%fire_ignition_end_time2 = fire_ignition_end_time2
      this%fire_ignition_radius2 = fire_ignition_radius2

      this%fire_ignition_start_lon3 = fire_ignition_start_lon3
      this%fire_ignition_start_lat3 = fire_ignition_start_lat3
      this%fire_ignition_end_lon3 = fire_ignition_end_lon3
      this%fire_ignition_end_lat3 = fire_ignition_end_lat3
      this%fire_ignition_ros3 = fire_ignition_ros3
      this%fire_ignition_start_time3 = fire_ignition_start_time3
      this%fire_ignition_end_time3 = fire_ignition_end_time3
      this%fire_ignition_radius3 = fire_ignition_radius3

      this%fire_ignition_start_lon4 = fire_ignition_start_lon4
      this%fire_ignition_start_lat4 = fire_ignition_start_lat4
      this%fire_ignition_end_lon4 = fire_ignition_end_lon4
      this%fire_ignition_end_lat4 = fire_ignition_end_lat4
      this%fire_ignition_ros4 = fire_ignition_ros4
      this%fire_ignition_start_time4 = fire_ignition_start_time4
      this%fire_ignition_end_time4 = fire_ignition_end_time4
      this%fire_ignition_radius4 = fire_ignition_radius4

      this%fire_ignition_start_lon5 = fire_ignition_start_lon5
      this%fire_ignition_start_lat5 = fire_ignition_start_lat5
      this%fire_ignition_end_lon5 = fire_ignition_end_lon5
      this%fire_ignition_end_lat5 = fire_ignition_end_lat5
      this%fire_ignition_ros5 = fire_ignition_ros5
      this%fire_ignition_start_time5 = fire_ignition_start_time5
      this%fire_ignition_end_time5 = fire_ignition_end_time5
      this%fire_ignition_radius5 = fire_ignition_radius5

    end subroutine Init_fire_block

    subroutine Init_time_block (this, file_name)

      use, intrinsic :: iso_fortran_env, only : ERROR_UNIT

      implicit none

      class (namelist_t), intent (in out) :: this
      character (len = *), intent (in) :: file_name

      integer :: start_year, start_month, start_day, start_hour, start_minute, start_second, &
          end_year, end_month, end_day, end_hour, end_minute, end_second, interval_output, &
          num_tiles, num_tiles_x, num_tiles_y
      real :: dt

      integer :: unit_nml, io_stat

      namelist /time/ start_year, start_month, start_day, start_hour, start_minute, start_second, &
          end_year, end_month, end_day, end_hour, end_minute, end_second, dt, interval_output, &
          num_tiles, num_tiles_x, num_tiles_y


      start_year = 0
      start_month = 0
      start_day = 0
      start_hour = 0
      start_minute = 0
      start_second = 0
      end_year = 0
      end_month = 0
      end_day = 0
      end_hour = 0
      end_minute = 0
      end_second = 0
      dt = 2.0
      interval_output = 0
      num_tiles = 1
      num_tiles_x = 1
      num_tiles_y = 1

      open (newunit = unit_nml, file = trim (file_name), action = 'read', iostat = io_stat)
      if (io_stat /= 0) then
        write (ERROR_UNIT, *) 'Problems opening namelist file ', trim (file_name)
        stop
      end if

      read (unit_nml, nml = time, iostat = io_stat)
      if (io_stat /= 0) then
        write (ERROR_UNIT, *) 'Problems reading namelist time block'
        stop
      end if
      close (unit_nml)

      this%start_year = start_year
      this%start_month = start_month
      this%start_day = start_day
      this%start_hour = start_hour
      this%start_minute = start_minute
      this%start_second = start_second
      this%end_year = end_year
      this%end_month = end_month
      this%end_day = end_day
      this%end_hour = end_hour
      this%end_minute = end_minute
      this%end_second = end_second
      this%dt = dt
      this%interval_output = interval_output

      this%num_tiles = num_tiles
      this%num_tiles_x = num_tiles_x
      this%num_tiles_y = num_tiles_y

    end subroutine Init_time_block

    subroutine Init_namelist (this, file_name)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT

      implicit none

      class (namelist_t), intent (out) :: this
      character (len = *), intent (in) :: file_name

      logical, parameter :: DEBUG_LOCAL = .true.


      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Entering subroutine Read_namelist'

      call this%Init_time_block (file_name = trim (file_name))
      call this%Init_fire_block (file_name = trim (file_name))
      call this%Init_atm_block (file_name = trim (file_name))

      if (DEBUG_LOCAL) write (OUTPUT_UNIT, *) '  Leaving subroutine Read_namelist'

    end subroutine Init_namelist

  end module namelist_mod
