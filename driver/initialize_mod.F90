  module initialize_mod

    use state_mod, only : state_fire_t
    use namelist_mod, only : namelist_t
    use geogrid_mod, only : geogrid_t
    use wrf_mod, only : wrf_t
    use module_fr_fire_driver, only : init_fire_driver
    use module_fr_fire_util, only : message

    private

    public :: Init_fire_state, Init_atm_state

  contains

    subroutine Init_atm_state (atm_state, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT

      implicit none

      type (wrf_t), intent (in out) :: atm_state
      type (namelist_t), intent (in) :: config_flags

      type (geogrid_t) :: geogrid


      geogrid = geogrid_t (file_name = 'geo_em.d01.nc')
      atm_state = wrf_t ('wrf.nc', config_flags, geogrid)

    end subroutine Init_atm_state

    subroutine Init_fire_state (grid, config_flags, wrf)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT

      implicit none

      type (state_fire_t), intent (in out) :: grid
      type (namelist_t), intent (in) :: config_flags
      type (wrf_t), intent (in out), optional :: wrf

      type (geogrid_t) :: geogrid
      logical, parameter :: DEBUG_LOCAL = .true.
      integer :: i, j, unit_out, unit_out2


      if (DEBUG_LOCAL) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) '  Entering subroutine Init_state'
      end if

        ! Fire state initialization
      geogrid = geogrid_t (file_name = 'geo_em.d01.nc')
      call grid%Initialization (config_flags, geogrid)

        ! Atmosphere to Fire
      if (present (wrf)) then
        call grid%Handle_wrfdata_update (wrf, config_flags)
      end if

        ! Fire init
      call message ('Init_fire_state: FIRE initialization start',config_flags%fire_print_msg)
      call init_fire_driver (grid, config_flags)
      call message ('Init_fire_state: FIRE initialization complete',config_flags%fire_print_msg)

      if (DEBUG_LOCAL) then
          ! print lat/lons
        open (newunit = unit_out, file = 'latlons_c.dat')
        open (newunit = unit_out2, file = 'latlons.dat')
        do j = 1, grid%ny + 1
          do i = 1, grid%nx + 1
            write (unit_out, *) grid%lons_c(i, j), grid%lats_c(i, j)
            if (i /= grid%nx + 1 .and. j /= grid%ny + 1) write (unit_out2, *) grid%lons(i, j), grid%lats(i, j)
          end do
        end do
        close (unit_out)
        close (unit_out2)

        if (present (wrf)) then
          open (newunit = unit_out, file = 'wrf_latlons_atm.dat')
          do j = 1, wrf%jde - 1
            do i = 1, wrf%ide - 1
              write (unit_out, *) wrf%xlong(i, j), wrf%xlat(i, j)
            end do
          end do
          close (unit_out)
        end if
      end if

      if (DEBUG_LOCAL) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) '  Leaving subroutine Init_state'
      end if

    end subroutine Init_fire_state

  end module initialize_mod

