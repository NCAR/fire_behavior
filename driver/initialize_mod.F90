  module initialize_mod

    use state_mod, only : domain
    use namelist_mod, only : namelist_t
    use geogrid_mod, only : geogrid_t
    use module_fr_fire_driver_wrf, only : fire_driver_em_init
    use wrf_mod, only : wrf_t

    private

    public :: Init_fire_state, Init_atm_state

  contains

    subroutine Init_atm_state (atm_state, config_flags)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT

      implicit none

      type (wrf_t), intent (in out) :: atm_state
      type (namelist_t), intent (in) :: config_flags

      type (geogrid_t) :: geogrid


      select case (config_flags%atm_model)
        case ('test1')
          atm_state = wrf_t (config_flags=config_flags)
          call atm_state%Load_atmosphere_test1 (config_flags)

        case ('wrfdata_legacy')
          if (config_flags%fire_fuel_read == -1) then
            geogrid = geogrid_t (file_name = 'geo_em.d01.nc')
            atm_state = wrf_t (config_flags=config_flags, geogrid=geogrid)
          else
            atm_state = wrf_t (config_flags=config_flags)
          end if
          call atm_state%Read_wrf_input()

        case ('wrfdata')
          geogrid = geogrid_t (file_name = 'geo_em.d01.nc')
          atm_state = wrf_t ('wrf.nc', config_flags, geogrid)

        case default
          write (ERROR_UNIT, *) 'Not ready to use atm model ', config_flags%atm_model

      end select

    end subroutine Init_atm_state

    subroutine Init_fire_state (grid, config_flags, wrf)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT, ERROR_UNIT

      implicit none

      type (domain), intent (in out) :: grid
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
      if (config_flags%fire_fuel_read == -1) then
        geogrid = geogrid_t (file_name = 'geo_em.d01.nc')
        call grid%Initialization (config_flags, geogrid)
      else
        call grid%Initialization (config_flags)
      end if

        ! Atmosphere to Fire
      if (present (wrf)) then
      select case ( config_flags%atm_model)
        case ('test1')
          call grid%Handle_wrfdata_update (wrf, config_flags, .true.)

        case ('wrfdata_legacy')
          call grid%Handle_wrfdata_update (wrf, config_flags, .true.)

        case ('wrfdata')
          call grid%Handle_wrfdata_update (wrf, config_flags)

        case default
          write (ERROR_UNIT, *) 'Not ready to use atm model ', config_flags%atm_model
      end select
      else
        if (allocated (grid%lats)) then
         grid%fxlat = grid%lats
         grid%fxlong = grid%lons
        else
          write (ERROR_UNIT, *) 'fxlat, fxlong are not assigned'
          stop
        end if
      end if

        ! Fire init
      call fire_driver_em_init (grid, config_flags)

      if (DEBUG_LOCAL .and. config_flags%fire_fuel_read == -1) then
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

        open (newunit = unit_out, file = 'wrf_latlons_fire.dat')
        do j = 1, grid%jfde
          do i = 1, grid%ifde
            write (unit_out, *) grid%fxlong(i, j), grid%fxlat(i, j)
          end do
        end do
        close (unit_out)
      end if

      if (DEBUG_LOCAL) then
        write (OUTPUT_UNIT, *) ''
        write (OUTPUT_UNIT, *) '  Leaving subroutine Init_state'
      end if

    end subroutine Init_fire_state

  end module initialize_mod

