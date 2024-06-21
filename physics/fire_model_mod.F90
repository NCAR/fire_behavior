  module fire_model_mod

    use fire_physics_mod, only: Calc_flame_length, Calc_fire_fluxes, Calc_smoke_emissions
    use level_set_mod, only: Calc_fuel_left, Update_ignition_times, Reinit_level_set, Prop_level_set, Extrapol_var_at_bdys, Stop_if_close_to_bdy
    use namelist_mod, only : namelist_t
    use ros_mod, only : ros_t
    use state_mod, only: state_fire_t
    use stderrout_mod, only : Print_message

    private

    public :: Advance_fire_model

  contains

    pure subroutine Copy_lfnout_to_lfn (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, lfn_out, lfn)

      implicit none

      integer, intent (in) :: ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme
      real, dimension (ifms:ifme, jfms:jfme), intent (in) :: lfn_out
      real, dimension (ifms:ifme, jfms:jfme), intent (out) :: lfn

      integer :: i, j


      do j = jfts, jfte
        do i = ifts, ifte
          lfn(i, j) = lfn_out(i, j)
        end do
      end do

    end subroutine Copy_lfnout_to_lfn

    subroutine Advance_fire_model (config_flags, grid, i_start, i_end, j_start, j_end)

      ! Purpose advance the fire from time_start to time_start + dt

      implicit none

      type (namelist_t), intent (in) :: config_flags
      type (state_fire_t), intent (in out) :: grid
      integer, intent (in) :: i_start, i_end, j_start, j_end

      integer :: ifds, ifde, jfds, jfde, ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme
      real :: tbound, time_start
      logical, parameter :: DEBUG_LOCAL = .false.


      if (DEBUG_LOCAL) call Print_message ('Entering Advance_fire_model...')

      ifds = grid%ifds
      ifde = grid%ifde
      jfds = grid%jfds
      jfde = grid%jfde

      ifts = i_start
      ifte = i_end
      jfts = j_start
      jfte = j_end

      ifms = grid%ifms
      ifme = grid%ifme
      jfms = grid%jfms
      jfme = grid%jfme

      time_start = grid%itimestep * grid%dt

      if (DEBUG_LOCAL) call Print_message ('calling Prop_level_set...')
      call Prop_level_set (ifds, ifde, jfds, jfde, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, time_start, grid%dt, grid%dx, grid%dy, &
          config_flags%fire_upwinding, config_flags%fire_viscosity, config_flags%fire_viscosity_bg, config_flags%fire_viscosity_band, &
          config_flags%fire_viscosity_ngp, config_flags%fire_lsm_band_ngp, tbound, grid%lfn, grid%lfn_0, grid%lfn_1, grid%lfn_2, &
          grid%lfn_out, grid%tign_g, grid%ros, grid%uf, grid%vf, grid%dzdxf, grid%dzdyf, grid%ros_param)

      if (DEBUG_LOCAL) call Print_message ('calling Stop_if_close_to_bdy...')
      call Stop_if_close_to_bdy (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, ifds, jfds, ifde, jfde, grid%lfn_out)

      if (DEBUG_LOCAL) call Print_message ('calling Update_ignition_times...')
      call Update_ignition_times (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, ifds, jfds, ifde, jfde, &
          time_start, grid%dt, grid%lfn, grid%lfn_out, grid%tign_g)

      if (DEBUG_LOCAL) call Print_message ('calling Calc_flame_length...')
      call Calc_flame_length (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
           grid%ros, grid%ros_param%iboros, grid%flame_length, grid%ros_front, grid%fire_area)

      if (DEBUG_LOCAL) call Print_message ('calling Reinit_level_set...')
      if (config_flags%fire_lsm_reinit) call Reinit_level_set (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
          ifds, ifde, jfds, jfde, time_start, grid%dt, grid%dx, grid%dy, config_flags%fire_upwinding_reinit, &
          config_flags%fire_lsm_reinit_iter, config_flags%fire_lsm_band_ngp, grid%lfn, grid%lfn_2, grid%lfn_s0, &
           grid%lfn_s1, grid%lfn_s2, grid%lfn_s3, grid%lfn_out, grid%tign_g)

      if (DEBUG_LOCAL) call Print_message ('calling Copy_lfnout_to_lfn...')
      call Copy_lfnout_to_lfn (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, grid%lfn_out, grid%lfn)
 
      if (DEBUG_LOCAL) call Print_message ('calling Ignite_prescribed_fires...')
      call Ignite_prescribed_fires (grid, config_flags, time_start, ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde)

      if (DEBUG_LOCAL) call Print_message ('calling Calc_fuel_left...')
      call Calc_fuel_left (ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, ifts, ifte, jfts, jfte, &
          grid%lfn,grid%tign_g,grid%fuel_time, time_start + grid%dt, grid%fuel_frac, grid%fire_area, &
          grid%fuel_frac_burnt_dt)

      if (DEBUG_LOCAL) call Print_message ('calling Calc_fire_fluxes...')
      call Calc_fire_fluxes (grid%dt, grid, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
          ifts, ifte, jfts, jfte, grid%fuel_load_g, grid%fuel_frac_burnt_dt, grid%fgrnhfx, grid%fgrnqfx)

      if (DEBUG_LOCAL) call Print_message ('calling Calc_smoke_emissions...')
      call Calc_smoke_emissions (grid, config_flags, ifts, ifte, jfts, jfte)

      if (DEBUG_LOCAL) call Print_message ('Leaving Advance_fire_model...')

    end subroutine Advance_fire_model

    subroutine Ignite_prescribed_fires (grid, config_flags, time_start, ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde)

      implicit none

      type (namelist_t), intent (in) :: config_flags
      type (state_fire_t), intent (in out) :: grid
      real, intent (in) :: time_start
      integer, intent (in) :: ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde

      real, parameter :: EPSILON = 0.00001
      integer :: i, j, ig, ignitions_done, start_time_ig, end_time_ig, ignited
        ! number of gridpts ignited in a given ignition
      integer :: ignited_tile(config_flags%fire_num_ignitions)


      ig = 1
      start_time_ig = grid%ignition_lines%start_time(ig)
      end_time_ig  = grid%ignition_lines%end_time(ig)
      ignitions_done = 0

      if (config_flags%fire_is_real_perim .and. time_start >= start_time_ig .and. time_start < start_time_ig + grid%dt) then
        ignited = 0
        do j = jfts, jfte
          do i = ifts, ifte
            grid%lfn(i, j) = grid%lfn_hist(i, j)
            if (abs(grid%lfn(i, j)) < EPSILON) then
              grid%tign_g(i, j) = time_start
              ignited = ignited + 1
            end if
          end do
        end do

        call Extrapol_var_at_bdys (ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, &
            ifts, ifte, jfts, jfte, grid%lfn)

      else if (.not. config_flags%fire_is_real_perim) then
        do ig = 1, config_flags%fire_num_ignitions
          call grid%ignition_lines%Ignite_fire (ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
              ig, time_start, time_start + grid%dt,  grid%lons, grid%lats, grid%unit_fxlong, grid%unit_fxlat, &
              grid%lfn, grid%tign_g, ignited)
          ignitions_done = ignitions_done + 1
          ignited_tile(ignitions_done) = ignited
        end do
      end if

    end subroutine Ignite_prescribed_fires

  end module fire_model_mod
