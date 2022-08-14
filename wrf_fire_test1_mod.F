  module wrf_fire_test1_mod

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

    use wrf_atmosphere_mod, only : domain, grid_config_rec_type

    implicit none

    private

    public :: Set_wrf_fire_test1

  contains

    subroutine Set_wrf_fire_test1 (grid, config_flags)

      implicit none

      type (domain), intent (in out) :: grid
      type (grid_config_rec_type), intent (in out) :: config_flags

      logical, parameter :: DEBUG = .true.


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Set_wrf_fire_test1'

      call Load_config_flags_test1 (config_flags)

      call Load_domain_test1 (grid)

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Set_wrf_fire_test1'

    end subroutine Set_wrf_fire_test1

    subroutine Load_config_flags_test1 (config_flags)

      implicit none

      type (grid_config_rec_type), intent (in out) :: config_flags

      logical, parameter :: DEBUG = .true.


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Load_config_flags_test1'

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
