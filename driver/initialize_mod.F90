  module initialize_mod

    use state_mod, only : state_fire_t
    use namelist_mod, only : namelist_t
    use geogrid_mod, only : geogrid_t
    use wrf_mod, only : wrf_t
    use fire_driver_mod, only : Init_fire_components
    use stderrout_mod, only: Print_message

    private

    public :: Init_fire_state, Init_atm_state, Init_fire_state_within_wrf

  contains

    subroutine Init_atm_state (atm_state, config_flags)

      implicit none

      type (wrf_t), intent (in out) :: atm_state
      type (namelist_t), intent (in) :: config_flags

      logical, parameter :: DEBUG_LOCAL = .false.


      if (DEBUG_LOCAL) call Print_message ('  Entering subroutine Init_atm_state')

      atm_state = wrf_t ('wrf.nc', config_flags)

      if (DEBUG_LOCAL) call Print_message ('  Leaving subroutine Init_atm_state')

    end subroutine Init_atm_state

    subroutine Init_fire_state (grid, config_flags, wrf)

      implicit none

      type (state_fire_t), intent (in out) :: grid
      type (namelist_t), intent (in) :: config_flags
      type (wrf_t), intent (in out), optional :: wrf

      type (geogrid_t) :: geogrid
      logical, parameter :: DEBUG_LOCAL = .false.
      integer :: i, j, unit_out, unit_out2


      if (DEBUG_LOCAL) call Print_message ('  Entering subroutine Init_state')

        ! Fire state initialization
      if (DEBUG_LOCAL) call Print_message ('  Reading geogrid file')
      geogrid = geogrid_t (file_name = 'geo_em.d01.nc')

      if (DEBUG_LOCAL) call Print_message ('  Initializing state')
      call grid%Initialization (config_flags, geogrid)

        ! Atmosphere to Fire
      if (present (wrf)) then
        if (DEBUG_LOCAL) call Print_message ('  Initializing atmospheric state')
        call grid%Handle_wrfdata_update (wrf, config_flags)
      end if

        ! Fire init
      call Init_fire_components (grid, config_flags)

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
              write (unit_out, *) wrf%lons(i, j), wrf%lats(i, j)
            end do
          end do
          close (unit_out)
        end if
      end if

      if (DEBUG_LOCAL) call Print_message ('  Leaving subroutine Init_state')

    end subroutine Init_fire_state

    subroutine Init_fire_state_within_wrf (state, config_flags)

      implicit none

      type (state_fire_t), intent (in out), optional :: state
      type (namelist_t), intent (in), optional :: config_flags

      logical :: is_state_present, is_nml_present


      is_state_present = present (state)
      is_nml_present = present (config_flags)

      print *, 'Initializing the CFBM model within WRF'
      print *, 'Is state present? ', is_state_present
      print *, 'Is nml present? ', is_nml_present

    end subroutine Init_fire_state_within_wrf

  end module initialize_mod
