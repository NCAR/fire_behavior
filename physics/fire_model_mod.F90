  module fire_model_mod

    use fire_physics_mod, only: Calc_flame_length, Calc_fire_fluxes, Calc_smoke_emissions
    use level_set_mod, only: Calc_fuel_left, Update_ignition_times, Reinit_level_set, Prop_level_set, &
        Stop_if_close_to_bdy, Copy_lfnout_to_lfn, Reinit_level_set_fast_dist, Check_isolated_negative_lfn, Extrapol_var_at_bdys
    use ignition_line_mod, only : ignition_line_t
    use namelist_mod, only : namelist_t
    use ros_mod, only : ros_t
    use state_mod, only: state_fire_t, N_POINTS_IN_HALO
    use stderrout_mod, only : Print_message, Stop_simulation

#ifdef DM_PARALLEL
    use mpi_mod, only : Do_halo_exchange_with_corners
#endif

    private

    public :: Advance_fire_model

  contains

    subroutine Advance_fire_model (config_flags, grid)

      ! Purpose advance the fire from time_start to time_start + dt

      implicit none

      type (namelist_t), intent (in) :: config_flags
      type (state_fire_t), intent (in out) :: grid

      integer :: ij, ifds, ifde, jfds, jfde, ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme
      real :: tbound, time_start, time_end, ignition_time
      logical, parameter :: DEBUG_LOCAL = .false.


      if (DEBUG_LOCAL) call Print_message ('Entering Advance_fire_model...')

      ifds = grid%ifds
      ifde = grid%ifde
      jfds = grid%jfds
      jfde = grid%jfde

      ifms = grid%ifms
      ifme = grid%ifme
      jfms = grid%jfms
      jfme = grid%jfme

      if (grid%itimestep < 1) call Stop_simulation ('Fire advance requires itimestep >= 1')
      ! Advance_state increments first, so itimestep indexes the interval end.
      ! Keep this counter for FMC and coupling; the first interval is [0,dt].
      time_start = (grid%itimestep - 1) * grid%dt
      ! Form the endpoint with the same multiplication used for ignition times
      ! and the driver clock, avoiding a differently rounded time_start + dt.
      time_end = grid%itimestep * grid%dt

      if (DEBUG_LOCAL) call Print_message ('calling Prop_level_set...')
      call Prop_level_set (ifds, ifde, jfds, jfde, ifms, ifme, jfms, jfme, &
          grid%num_tiles, grid%i_start, grid%i_end, grid%j_start, grid%j_end, time_start, grid%dt, grid%dx, grid%dy, &
          config_flags%fire_upwinding, config_flags%fire_viscosity, config_flags%fire_viscosity_bg, config_flags%fire_viscosity_band, &
          config_flags%fire_viscosity_ngp, config_flags%fire_lsm_band_ngp, tbound, grid%lfn, grid%lfn_0, grid%lfn_1, grid%lfn_2, &
          grid%lfn_out, grid%tign_g, grid%ros, grid%uf, grid%vf, grid%dzdxf, grid%dzdyf, grid%ros_param, grid%cart_comm, &
          grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, grid%grad_norm_ls, grid%grad_norm_residual_sq_sum, &
          grid%grad_norm_residual_sq_sum_band, grid%grad_norm_residual_rms_band)

      if (DEBUG_LOCAL) call Print_message ('calling Stop_if_close_to_bdy...')
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, ifts, ifte, jfts, jfte)
      do ij = 1, grid%num_tiles
        ifts = grid%i_start(ij)
        ifte = grid%i_end(ij)
        jfts = grid%j_start(ij)
        jfte = grid%j_end(ij)

        call Stop_if_close_to_bdy (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, ifds, jfds, ifde, jfde, grid%lfn_out)
      end do
      !$OMP END PARALLEL DO

      if (DEBUG_LOCAL) call Print_message ('calling Update_ignition_times...')
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, ifts, ifte, jfts, jfte)
      do ij = 1, grid%num_tiles
        ifts = grid%i_start(ij)
        ifte = grid%i_end(ij)
        jfts = grid%j_start(ij)
        jfte = grid%j_end(ij)

        call Update_ignition_times (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, ifds, jfds, ifde, jfde, &
            time_start, grid%dt, grid%lfn, grid%lfn_out, grid%tign_g)
      end do
      !$OMP END PARALLEL DO

#ifdef DM_PARALLEL
      call Do_halo_exchange_with_corners (grid%tign_g, ifms, ifme, jfms, jfme, grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, N_POINTS_IN_HALO, grid%cart_comm)
#endif

      if (DEBUG_LOCAL) call Print_message ('calling Calc_flame_length...')
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, ifts, ifte, jfts, jfte)
      do ij = 1, grid%num_tiles
        ifts = grid%i_start(ij)
        ifte = grid%i_end(ij)
        jfts = grid%j_start(ij)
        jfte = grid%j_end(ij)

        call Calc_flame_length (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
            grid%ros, grid%ros_param%iboros, grid%flame_length, grid%ros_front, grid%fire_area)
      end do
      !$OMP END PARALLEL DO

      if (config_flags%fast_dist_reinit_opt > 0 .and. grid%itimestep > 0 .and. mod (grid%itimestep, config_flags%fast_dist_reinit_freq) == 0) then
        if (DEBUG_LOCAL) call Print_message ('calling Reinit_level_set_fast_dist...')
        call Reinit_level_set_fast_dist (grid%lfn_s0, grid%lfn_out, grid%i_start, grid%i_end, grid%j_start, grid%j_end, &
             ifms, ifme, jfms, jfme, grid%num_tiles, config_flags%fast_dist_reinit_opt, grid%dx, grid%dy, &
             grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, grid%ifds, grid%ifde, grid%jfds, grid%jfde, grid%cart_comm)
      end if

      if (DEBUG_LOCAL) call Print_message ('calling Reinit_level_set...')
      if (config_flags%fire_lsm_reinit) call Reinit_level_set (grid%num_tiles, grid%i_start, grid%i_end, grid%j_start, grid%j_end, &
          ifms, ifme, jfms, jfme, &
          ifds, ifde, jfds, jfde, time_start, grid%dt, grid%dx, grid%dy, config_flags%fire_upwinding_reinit, &
          config_flags%fire_lsm_reinit_iter, config_flags%fire_lsm_band_ngp, grid%lfn, grid%lfn_2, grid%lfn_s0, &
          grid%lfn_s1, grid%lfn_s2, grid%lfn_s3, grid%lfn_out, grid%tign_g, grid%cart_comm, &
          grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, config_flags%reinit_pseudot_coef, grid%grad_norm_reinit)

      if (DEBUG_LOCAL) call Print_message ('calling Copy_lfnout_to_lfn...')
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, ifts, ifte, jfts, jfte)
      do ij = 1, grid%num_tiles
        ifts = grid%i_start(ij)
        ifte = grid%i_end(ij)
        jfts = grid%j_start(ij)
        jfte = grid%j_end(ij)

        call Copy_lfnout_to_lfn (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, grid%lfn_out, grid%lfn)
      end do
      !$OMP END PARALLEL DO

#ifdef DM_PARALLEL
      call Do_halo_exchange_with_corners (grid%lfn, ifms, ifme, jfms, jfme, grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, N_POINTS_IN_HALO, grid%cart_comm)
#endif

      if (config_flags%check_isolated_neg_lfn == 1) call Check_isolated_negative_lfn (grid)

      ! Install an aligned perimeter at the inclusive interval end. It begins
      ! propagating next step; zero-time perimeters were installed at initialization.
      ! Mid-step activation would age fuel without proportional propagation.
      ! Check_nml rejects it pending a separate review of split advances.
      if (config_flags%fire_is_real_perim .and. .not. grid%fire_perimeter_ignited) then
        ignition_time = anint (config_flags%fire_ignition_start_time1 / grid%dt) * grid%dt
        if (ignition_time <= time_end) call grid%Ignite_fire_perimeter (ignition_time)
      end if
 
      if (DEBUG_LOCAL) call Print_message ('calling Ignite_fire_line...')
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, ifts, ifte, jfts, jfte)
      do ij = 1, grid%num_tiles
        ifts = grid%i_start(ij)
        ifte = grid%i_end(ij)
        jfts = grid%j_start(ij)
        jfte = grid%j_end(ij)

        if (.not. config_flags%fire_is_real_perim) &
            call Ignite_fire_line (grid%ignition_lines, config_flags%fire_num_ignitions, time_start, time_end, &
                ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, grid%lons, grid%lats, grid%unit_fxlong, &
                grid%unit_fxlat, grid%lfn, grid%tign_g)
      end do
      !$OMP END PARALLEL DO

#ifdef DM_PARALLEL
      call Do_halo_exchange_with_corners (grid%tign_g, ifms, ifme, jfms, jfme, grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, N_POINTS_IN_HALO, grid%cart_comm)
      call Do_halo_exchange_with_corners (grid%lfn, ifms, ifme, jfms, jfme, grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, N_POINTS_IN_HALO, grid%cart_comm)
#endif

      ! Synchronize ignition changes above, then refresh physical boundaries
      ! before subcell fuel-area and ignition-age interpolation.
      call Extrapol_var_at_bdys (ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, &
          grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, grid%lfn)
      ! With finite positive lfn throughout the boundary stencil, these ignition
      ! times do not contribute to fuel consumption. Refresh for consistency.
      ! The boundary guard precedes ignition, so newly installed geometry still
      ! needs separate review before assuming this condition at every boundary.
      call Extrapol_var_at_bdys (ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, &
          grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, grid%tign_g)

      if (DEBUG_LOCAL) call Print_message ('calling Calc_fuel_left...')
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, ifts, ifte, jfts, jfte)
      do ij = 1, grid%num_tiles
        ifts = grid%i_start(ij)
        ifte = grid%i_end(ij)
        jfts = grid%j_start(ij)
        jfte = grid%j_end(ij)
        call Calc_fuel_left (ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, ifts, ifte, jfts, jfte, &
            grid%lfn,grid%tign_g,grid%fuel_time, time_end, grid%fuel_frac, grid%fire_area, &
            grid%fuel_frac_burnt_dt)
      end do
      !$OMP END PARALLEL DO

      if (DEBUG_LOCAL) call Print_message ('calling Calc_fire_fluxes...')
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, ifts, ifte, jfts, jfte)
      do ij = 1, grid%num_tiles
        ifts = grid%i_start(ij)
        ifte = grid%i_end(ij)
        jfts = grid%j_start(ij)
        jfte = grid%j_end(ij)
        call Calc_fire_fluxes (grid%dt, grid, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
            ifts, ifte, jfts, jfte, grid%fuel_load_g, grid%fuel_frac_burnt_dt, grid%fgrnhfx, grid%fgrnqfx)
      end do
      !$OMP END PARALLEL DO

      if (DEBUG_LOCAL) call Print_message ('calling Calc_smoke_emissions...')
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, ifts, ifte, jfts, jfte)
      do ij = 1, grid%num_tiles
        ifts = grid%i_start(ij)
        ifte = grid%i_end(ij)
        jfts = grid%j_start(ij)
        jfte = grid%j_end(ij)

        call Calc_smoke_emissions (grid, config_flags, ifts, ifte, jfts, jfte)
      end do
      !$OMP END PARALLEL DO

      if (DEBUG_LOCAL) call Print_message ('Leaving Advance_fire_model...')

    end subroutine Advance_fire_model

    subroutine Ignite_fire_line (ignition_lines, num_ignitions, time_start, time_end, &
        ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, lons, lats, unit_fxlong, unit_fxlat, lfn, tign)

      implicit none

      type (ignition_line_t), intent (in) :: ignition_lines
      integer, intent (in) :: num_ignitions, ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme
      real, intent (in) :: time_start, time_end, unit_fxlong, unit_fxlat
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: lons, lats
      real, dimension(ifms:ifme, jfms:jfme), intent (in out) :: lfn, tign

      integer :: ig, ignited


      ! Apply each prescribed line; this expansion is separate from propagation.
      do ig = 1, num_ignitions
        call ignition_lines%Ignite_fire (ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
            ig, time_start, time_end, lons, lats, unit_fxlong, unit_fxlat, lfn, tign, ignited)
      end do

    end subroutine Ignite_fire_line

  end module fire_model_mod
