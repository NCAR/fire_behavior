  module wrf_fire_test2_mod

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

    use wrf_atmosphere_mod, only : domain
    use namelist_mod, only : namelist_t
    use module_fr_fire_util, only : set_ideal_coord

    implicit none

    private

    public :: Set_wrf_fire_test2, n_steps_test2, read_wrf_input_test2, check_tends_test2
    integer :: n_steps_test2 = 0
    logical :: read_wrf_input_test2 = .true.
    logical :: check_tends_test2 = .false.

      ! Grid settins
    integer, parameter :: IDS = 1, IDE = 20, KDS = 1, KDE = 51, JDS = 1, JDE = 40 
    integer, parameter :: SR_X = 4, SR_Y = 4
    integer, parameter :: N_TIME_STEPS = 4

      ! Debuging
    integer, parameter :: I_TO_PRINT = 15, J_TO_PRINT = 15

  contains

    subroutine Set_wrf_fire_test2 (grid, config_flags)

      implicit none

      type (domain), intent (in out) :: grid
      type (namelist_t), intent (in out) :: config_flags

      logical, parameter :: DEBUG = .true.


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Set_wrf_fire_test2'

        ! Set config_flags
      call Load_config_flags_test2 (config_flags)
      if (DEBUG) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Contents of config_flags:'
!        call config_flags%Print ()
      end if

        ! Set grid
      grid = domain (ids = IDS, ide = IDE, kds = KDS, kde = KDE, jds = JDS, jde = JDE, sr_x = SR_X, sr_y = SR_Y)
      if (DEBUG) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Contents of grid:'
        call grid%Print ()
      end if

      call Load_domain_test2 (grid)

        ! Number of time steps
      n_steps_test2 = N_TIME_STEPS

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Set_wrf_fire_test2'

    end subroutine Set_wrf_fire_test2

    subroutine Load_config_flags_test2 (config_flags)

      implicit none

      type (namelist_t), intent (in out) :: config_flags

      logical, parameter :: DEBUG = .true.


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Load_config_flags_test2'

      config_flags%fire_print_msg = 1

      config_flags%fire_fuel_read = 0
      config_flags%fire_fuel_cat = 1

      config_flags%fire_num_ignitions = 1
      config_flags%fire_ignition_ros1 = 0.05
      config_flags%fire_ignition_start_x1 = 700.0
      config_flags%fire_ignition_start_y1 = 500.0
      config_flags%fire_ignition_end_x1 = 500.0
      config_flags%fire_ignition_end_y1 = 1500.0
      config_flags%fire_ignition_radius1 = 80.0
      config_flags%fire_ignition_start_time1 = 1.0
      config_flags%fire_ignition_end_time1 = 15.0

      config_flags%fire_wind_height = 1.0

      config_flags%fire_fuel_left_irl = 2
      config_flags%fire_fuel_left_jrl = 2
      config_flags%fire_atm_feedback = 0.0
      config_flags%fire_grows_only = 1
      config_flags%fire_viscosity = 0.4
      config_flags%fire_upwinding = 9
      config_flags%fire_fuel_left_method = 1
      config_flags%fire_lfn_ext_up = 1.0

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Load_config_flags_test2'

    end subroutine Load_config_flags_test2

    subroutine Load_domain_test2 (grid)

      implicit none

      type (domain), intent (in out) :: grid

      logical, parameter  :: DEBUG = .true.
      real, parameter :: HT = 0.0, T2 = 0.0, Q2 = 0.0, PSFC = 0.0, RAINC = 0.0, RAINNC = 0.0, &
          ZSF = 0.0, DZDXF = 0.0, DZDYF = 0.0, DX = 50.0, DY = 50.0, DT = 0.5

      real :: fdx, fdy


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Load_domain_test2'

      grid%dx = DX
      grid%dy = DY
      grid%dt = DT

        ! 2D arrays
      grid%ht = HT
      grid%t2 = T2
      grid%q2 = Q2
      grid%psfc = PSFC
      grid%rainc = RAINC
      grid%rainnc = RAINNC

      grid%zsf = ZSF
      grid%dzdxf = DZDXF
      grid%dzdyf = DZDYF

        ! 1D arrays
      grid%c1h(:) = 1.0
      grid%c2h(:) = 0.0

        ! Ideal coordinates
      call set_ideal_coord( grid%dx,grid%dy, &
                  grid%ids, grid%ide, grid%jds, grid%jde, &
                  grid%ims, grid%ime, grid%jms, grid%jme, &
                  grid%ids, grid%ide, grid%jds, grid%jde, & ! originaly tile dims. domain dim are the same here
                  grid%xlong,grid%xlat)

      fdx = grid%dx / grid%sr_x
      fdy = grid%dy / grid%sr_y

      call set_ideal_coord (fdx, fdy, &
          grid%ifds, grid%ifde, grid%jfds, grid%jfde, &
          grid%ifms, grid%ifme, grid%jfms, grid%jfme, &
          grid%ifds, grid%ifde, grid%jfds, grid%jfde, & ! originaly tile dims. domain dim are the same here
          grid%fxlong,grid%fxlat)

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Load_domain_test2'

    end subroutine Load_domain_test2

  end module wrf_fire_test2_mod

