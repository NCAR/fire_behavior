  module wrf_fire_test1_mod

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

    use wrf_atmosphere_mod, only : domain, grid_config_rec_type

    implicit none

    private

    public :: Set_wrf_fire_test1, n_steps
    integer :: n_steps = 0

  contains

    subroutine Set_wrf_fire_test1 (grid, config_flags)

      implicit none

      type (domain), intent (in out) :: grid
      type (grid_config_rec_type), intent (in out) :: config_flags

        ! Configuration
      integer, parameter :: IDS = 1, IDE = 51, KDS = 1, KDE = 51, JDS = 1, JDE = 51
      integer, parameter :: SR_X = 2, SR_Y = 2
      integer, parameter :: N_TIME_STEPS = 0

      logical, parameter :: DEBUG = .true.


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Set_wrf_fire_test1'

        ! Set config_flags
      call Load_config_flags_test1 (config_flags)
      if (DEBUG) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Contents of config_flags:'
        call config_flags%Print ()
      end if

        ! Set grid
      grid = domain (ids = IDS, ide = IDE, kds = KDS, kde = KDE, jds = JDS, jde = JDE, sr_x = SR_X, sr_y = SR_Y)
      if (DEBUG) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Contents of grid:'
        call grid%Print ()
      end if

      call Load_domain_test1 (grid)

        ! Number of time steps
      n_steps = N_TIME_STEPS

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Set_wrf_fire_test1'

    end subroutine Set_wrf_fire_test1

    subroutine Load_config_flags_test1 (config_flags)

      implicit none

      type (grid_config_rec_type), intent (in out) :: config_flags

      logical, parameter :: DEBUG = .true.


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Load_config_flags_test1'

      config_flags%fire_print_msg = 2
      config_flags%fire_num_ignitions = 1
      config_flags%fire_ignition_radius1 = 200.0

      config_flags%fire_fuel_read = 0
      config_flags%fire_fuel_cat = 1

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Load_config_flags_test1'

    end subroutine Load_config_flags_test1

    subroutine Load_domain_test1 (grid)

      implicit none

      type (domain), intent (in out) :: grid

      logical, parameter :: DEBUG = .true.


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Load_domain_test1'

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Load_domain_test1'

    end subroutine Load_domain_test1

  end module wrf_fire_test1_mod
