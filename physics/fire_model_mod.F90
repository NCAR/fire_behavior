  module fire_model_mod

    use fire_physics_mod, only: Calc_flame_length, Calc_fire_fluxes, Calc_smoke_emissions
    use level_set_mod, only: Calc_fuel_left, Update_ignition_times, Reinit_level_set, Prop_level_set, Extrapol_var_at_bdys, &
        Stop_if_close_to_bdy, Copy_lfnout_to_lfn, Reinit_level_set_fast_dist, Check_isolated_negative_lfn, &
        Compute_active_front_masks, Expand_diagnostic_band
    use namelist_mod, only : namelist_t
    use ros_mod, only : ros_t
    use state_mod, only: state_fire_t, N_POINTS_IN_HALO
    use stderrout_mod, only : Print_message

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
      integer :: exact_active_front_calls
      real :: tbound, time_start
      logical :: do_fastdist, has_pre_reinit_mask, reinit_scheduled
      character (len = 128) :: msg
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

      time_start = grid%itimestep * grid%dt
      do_fastdist = config_flags%fast_dist_reinit_opt > 0 .and. grid%itimestep > 0 .and. &
          mod (grid%itimestep, config_flags%fast_dist_reinit_freq) == 0
      reinit_scheduled = do_fastdist .or. config_flags%fire_lsm_reinit
      exact_active_front_calls = 0
      has_pre_reinit_mask = .false.

      if (DEBUG_LOCAL) call Print_message ('calling Prop_level_set...')
      call Prop_level_set (ifds, ifde, jfds, jfde, ifms, ifme, jfms, jfme, &
          grid%num_tiles, grid%i_start, grid%i_end, grid%j_start, grid%j_end, time_start, grid%dt, grid%dx, grid%dy, &
          config_flags%fire_upwinding, config_flags%fire_viscosity, config_flags%fire_viscosity_bg, config_flags%fire_viscosity_band, &
          config_flags%fire_viscosity_ngp, config_flags%fire_lsm_band_ngp, tbound, grid%lfn, grid%lfn_0, grid%lfn_1, grid%lfn_2, &
          grid%lfn_out, grid%tign_g, grid%ros, grid%uf, grid%vf, grid%dzdxf, grid%dzdyf, grid%ros_param, grid%cart_comm, &
          grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, grid%grad_norm_ls, grid%grad_norm_residual_sq_sum, &
          grid%grad_norm_residual_sq_sum_band, grid%grad_norm_residual_rms_band, grid%lfn_tend_dbg)

      if (config_flags%use_active_front .and. reinit_scheduled) then
        call Compute_active_front_masks (ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, &
            grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, config_flags%active_front_band_ngp, grid%cart_comm, &
            grid%lfn_out, grid%nfuel_cat, grid%active_front_mask, grid%barrier_contact_front_mask, grid%band_mask)
        call Update_ros_lfn_error (grid, grid%active_front_mask)
        exact_active_front_calls = exact_active_front_calls + 1
        has_pre_reinit_mask = .true.
      end if

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
            grid%ros, grid%ros_param%iboros, grid%flame_length, grid%fire_area)
      end do
      !$OMP END PARALLEL DO

      if (do_fastdist) then
        if (DEBUG_LOCAL) call Print_message ('calling Reinit_level_set_fast_dist...')
        call Reinit_level_set_fast_dist (grid%lfn_s0, grid%lfn_out, grid%i_start, grid%i_end, grid%j_start, grid%j_end, &
             ifms, ifme, jfms, jfme, grid%num_tiles, config_flags%fast_dist_reinit_opt, grid%dx, grid%dy, &
             grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, grid%ifds, grid%ifde, grid%jfds, grid%jfde, grid%cart_comm)
      end if

      if (DEBUG_LOCAL) call Print_message ('calling Reinit_level_set...')
      grid%lfn_pre_reinit_dbg = grid%lfn_out
      if (config_flags%fire_lsm_reinit) call Reinit_level_set (grid%num_tiles, grid%i_start, grid%i_end, grid%j_start, grid%j_end, &
          ifms, ifme, jfms, jfme, &
          ifds, ifde, jfds, jfde, time_start, grid%dt, grid%dx, grid%dy, config_flags%fire_upwinding_reinit, &
          config_flags%fire_lsm_reinit_iter, config_flags%fire_lsm_band_ngp, grid%lfn, grid%lfn_2, grid%lfn_s0, &
          grid%lfn_s1, grid%lfn_s2, grid%lfn_s3, grid%lfn_out, grid%tign_g, grid%cart_comm, &
          grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, config_flags%reinit_pseudot_coef, &
          config_flags%reinit_pseudot_rate, config_flags%reinit_pseudot_cfl, grid%grad_norm_reinit, &
          config_flags%reinit_rs_buffer_ngp, grid%rs_interface_mask, grid%rs_distance_dbg)
      grid%lfn_post_reinit_dbg = grid%lfn_out

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

      if (config_flags%check_isolated_neg_lfn > 0) &
          call Check_isolated_negative_lfn (grid, mode = config_flags%check_isolated_neg_lfn)
 
      if (DEBUG_LOCAL) call Print_message ('calling Ignite_prescribed_fires...')
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, ifts, ifte, jfts, jfte)
      do ij = 1, grid%num_tiles
        ifts = grid%i_start(ij)
        ifte = grid%i_end(ij)
        jfts = grid%j_start(ij)
        jfte = grid%j_end(ij)

        call Ignite_prescribed_fires (grid, config_flags, time_start, ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde)
      end do
      !$OMP END PARALLEL DO

#ifdef DM_PARALLEL
      call Do_halo_exchange_with_corners (grid%tign_g, ifms, ifme, jfms, jfme, grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, N_POINTS_IN_HALO, grid%cart_comm)
      call Do_halo_exchange_with_corners (grid%lfn, ifms, ifme, jfms, jfme, grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, N_POINTS_IN_HALO, grid%cart_comm)
#endif

      if (config_flags%use_active_front) then
        call Compute_active_front_masks (ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, &
            grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, config_flags%active_front_band_ngp, grid%cart_comm, &
            grid%lfn, grid%nfuel_cat, grid%active_front_mask, grid%barrier_contact_front_mask, grid%band_mask)
        exact_active_front_calls = exact_active_front_calls + 1
        if (.not. has_pre_reinit_mask) call Update_ros_lfn_error (grid, grid%active_front_mask)
      end if

      if (DEBUG_LOCAL) call Print_message ('calling Calc_fuel_left...')
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, ifts, ifte, jfts, jfte)
      do ij = 1, grid%num_tiles
        ifts = grid%i_start(ij)
        ifte = grid%i_end(ij)
        jfts = grid%j_start(ij)
        jfte = grid%j_end(ij)
        call Calc_fuel_left (ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, ifts, ifte, jfts, jfte, &
            grid%lfn, grid%tign_g, grid%fuel_time, time_start + grid%dt, grid%dt, grid%fuel_frac, grid%fire_area, &
            grid%fire_area_change_rate, grid%fuel_frac_burnt_dt)
      end do
      !$OMP END PARALLEL DO

      if (.not. config_flags%use_active_front) then
        grid%active_front_mask = 0.0
        grid%barrier_contact_front_mask = 0.0
        call Expand_diagnostic_band (ifms, ifme, jfms, jfme, grid%ifps, grid%ifpe, grid%jfps, grid%jfpe, &
            config_flags%active_front_band_ngp, grid%cart_comm, grid%fire_area_change_rate, grid%nfuel_cat, grid%band_mask)
        call Update_ros_lfn_error (grid, grid%band_mask)
      end if

      if (config_flags%fire_print_msg > 1) then
        write (msg, '(a, i0)') 'Active-front exact connectivity calls this timestep=', exact_active_front_calls
        call Print_message (trim (msg))
      end if

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

    subroutine Update_ros_lfn_error (grid, support_mask)

      implicit none

      type (state_fire_t), intent (in out) :: grid
      real, dimension(grid%ifms:grid%ifme, grid%jfms:grid%jfme), intent (in) :: support_mask

      integer :: i, j
      real, parameter :: GRAD_NORM_MIN = 100.0 * epsilon (0.0)


      grid%ros_lfn_error_front = 0.0
      do j = grid%jfps, grid%jfpe
        do i = grid%ifps, grid%ifpe
          if (support_mask(i, j) > 0.5 .and. abs (grid%grad_norm_ls(i, j)) > GRAD_NORM_MIN) then
            grid%ros_lfn_error_front(i, j) = &
                -grid%lfn_tend_dbg(i, j) / grid%grad_norm_ls(i, j) - grid%ros(i, j)
          end if
        end do
      end do

    end subroutine Update_ros_lfn_error

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

      if (config_flags%fire_is_real_perim .and. .not. grid%real_perim_initialized .and. &
          time_start >= start_time_ig .and. time_start < start_time_ig + grid%dt) then
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
