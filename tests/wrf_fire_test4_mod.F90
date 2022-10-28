  module wrf_fire_test4_mod

    use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

    use state_mod, only : domain
    use namelist_mod, only : namelist_t
    use module_fr_fire_util, only : set_ideal_coord
    use geogrid_mod, only : geogrid_t

    implicit none

    private

    public :: Set_wrf_fire_test4, n_steps_test4, read_wrf_input_test4, check_tends_test4
    integer :: n_steps_test4 = 0
    logical :: read_wrf_input_test4 = .true.
    logical :: check_tends_test4 = .false.

      ! Grid settins
    integer, parameter :: KDS = 1, KDE = 45
    integer, parameter :: N_TIME_STEPS = 2

  contains

    subroutine Set_wrf_fire_test4 (grid, config_flags)

      implicit none

      type (domain), intent (in out) :: grid
      type (namelist_t), intent (in out) :: config_flags

      type (geogrid_t) :: geogrid
      logical, parameter :: DEBUG = .true.


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Set_wrf_fire_test4'

        ! Set grid
      geogrid = geogrid_t (file_name = 'geo_em.d01.nc')
      if (DEBUG) write (OUTPUT_UNIT, *) '  Init WRF grid derived type'
      grid = domain (ids = geogrid%ids, ide = geogrid%ide, kds = KDS, kde = KDE, jds = geogrid%jds, jde = geogrid%jde, &
          sr_x = geogrid%sr_x, sr_y = geogrid%sr_y, zsf = geogrid%elevations, dzdxf = geogrid%dz_dxs, dzdyf = geogrid%dz_dys, &
          nfuel_cat = geogrid%fuel_cats, dx = geogrid%dx, dy = geogrid%dy)
      if (DEBUG) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) 'Contents of grid:'
        call grid%Print ()
      end if

      call Load_domain_test4 (grid, geogrid%xlat, geogrid%xlong)

        ! Number of time steps
      n_steps_test4 = N_TIME_STEPS

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Set_wrf_fire_test4'

    end subroutine Set_wrf_fire_test4

    subroutine Load_domain_test4 (grid, xlat, xlong)

      implicit none

      type (domain), intent (in out) :: grid
      real, dimension (:, :), intent (in) :: xlat, xlong

      logical, parameter  :: DEBUG = .true.
      real, parameter :: HT = 0.0, T2 = 0.0, Q2 = 0.0, PSFC = 0.0, RAINC = 0.0, RAINNC = 0.0, &
          DT = 0.5


      if (DEBUG) write (OUTPUT_UNIT, *) '  Entering subroutine Load_domain_test4'

      grid%dt = DT

        ! 2D arrays
      grid%ht = HT
      grid%t2 = T2
      grid%q2 = Q2
      grid%psfc = PSFC
      grid%rainc = RAINC
      grid%rainnc = RAINNC

        ! 1D arrays
      grid%c1h(:) = 1.0
      grid%c2h(:) = 0.0

        ! lat lon
      grid%xlat = 0.0
      grid%xlat (grid%ids:grid%ide - 1, grid%jds:grid%jde - 1) = xlat
      grid%xlong = 0.0
      grid%xlong (grid%ids:grid%ide - 1, grid%jds:grid%jde - 1) = xlong

      grid%itimestep = 0

      if (DEBUG) write (OUTPUT_UNIT, *) '  Leaving subroutine Load_domain_test4'

    end subroutine Load_domain_test4

  end module wrf_fire_test4_mod

