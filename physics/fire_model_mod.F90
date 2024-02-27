  module fire_model_mod

    use level_set_mod, only: Ignite_fire, Fuel_left, Update_ignition_times, Reinit_level_set, Prop_level_set, Extrapol_var_at_bdys, Stop_if_close_to_bdy
    use stderrout_mod, only: Crash, Message
    use fire_physics_mod, only: Calc_flame_length, Calc_fire_fluxes, Calc_smoke_emissions
    use ignition_line_mod, only: ignition_line_t
    use ros_mod, only : ros_t
    use state_mod, only: state_fire_t
    use namelist_mod, only : namelist_t

    private

    public :: Advance_fire_model

  contains

    subroutine Advance_fire_model (config_flags, ignition_line, grid, i_start, i_end, j_start, j_end)

      ! Purpose advance the fire from time_start to time_start + dt

      implicit none

      type (namelist_t), intent (in) :: config_flags
      type(ignition_line_t), dimension (:), intent(in):: ignition_line
      type (state_fire_t), intent (in out) :: grid
      integer, intent (in) :: i_start, i_end, j_start, j_end

      real, dimension(i_start:i_end, j_start:j_end) :: fuel_frac_burnt, fuel_frac_end

      integer :: ifds, ifde, jfds, jfde, ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme
      integer :: ignited, ig, i, j
      real :: tbound, tfa, thf, mhf, tqf, mqf, aw, mw
      character (len = 128) :: msg
      integer :: stat_lev = 1
      real :: start_time_ig, end_time_ig, time_start
      real, parameter :: EPSILON = 0.00001
      integer :: ignitions_done
        ! number of gridpts ignited in a given ignition
      integer :: ignited_tile(config_flags%fire_num_ignitions)


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

      call Prop_level_set (ifds, ifde, jfds, jfde, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, time_start, grid%dt, grid%dx, grid%dy, &
          config_flags%fire_upwinding, config_flags%fire_viscosity, config_flags%fire_viscosity_bg, config_flags%fire_viscosity_band, &
          config_flags%fire_viscosity_ngp, config_flags%fire_lsm_band_ngp, tbound, grid%lfn, grid%lfn_0, grid%lfn_1, grid%lfn_2, &
          grid%lfn_out, grid%tign_g, grid%ros, grid, grid%ros_param)

      call Stop_if_close_to_bdy (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, ifds, jfds, ifde, jfde, grid%lfn_out)

      call Update_ignition_times (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, ifds, jfds, ifde, jfde, &
          time_start, grid%dt, grid%lfn, grid%lfn_out, grid%tign_g)

      call Calc_flame_length (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
           grid%ros, grid%ros_param%iboros, grid%flame_length, grid%ros_front, grid%fire_area)

      if (config_flags%fire_lsm_reinit) call Reinit_level_set (ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme, &
          ifds, ifde, jfds, jfde, time_start, grid%dt, grid%dx, grid%dy, config_flags%fire_upwinding_reinit, &
          config_flags%fire_lsm_reinit_iter, config_flags%fire_lsm_band_ngp, grid%lfn, grid%lfn_2, grid%lfn_s0, &
           grid%lfn_s1, grid%lfn_s2, grid%lfn_s3, grid%lfn_out, grid%tign_g, config_flags%fire_print_msg)

      do j = jfts, jfte
        do i = ifts, ifte
          grid%lfn(i, j) = grid%lfn_out(i, j)
        end do
      end do

        ! Check for ignitions
      ig = 1
      start_time_ig = ignition_line(ig)%start_time 
      end_time_ig  = ignition_line(ig)%end_time
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
            !  for now, check for ignition every time step...
            !        if(ignition_line(ig)%end_time>=time_start.and.ignition_line(ig)%start_time<time_start+dt)then 
          call Ignite_fire (ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
              ignition_line(ig), time_start, time_start + grid%dt,  grid%lons, grid%lats, grid%unit_fxlong, grid%unit_fxlat, & 
              grid%lfn, grid%tign_g,ignited)
          ignitions_done = ignitions_done + 1
          ignited_tile(ignitions_done) = ignited
            !        endif
        end do
      end if
           
        ! compute the heat fluxes from the fuel burned
        ! needs lfn and tign from neighbors so halo must be updated before
      call fuel_left (ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, ifts, ifte, jfts, jfte, &
          grid%lfn,grid%tign_g,grid%fuel_time, time_start + grid%dt, fuel_frac_end, grid%fire_area, &
          config_flags%fire_print_msg)

      do j = jfts, jfte
        do i = ifts, ifte
          fuel_frac_burnt(i, j) = grid%fuel_frac(i, j) - fuel_frac_end(i, j) ! fuel lost this timestep
          grid%burnt_area_dt(i, j) = fuel_frac_burnt(i, j)
          grid%fuel_frac(i, j) = fuel_frac_end(i, j) ! copy new value to state array
        end do
      end do

      call Calc_fire_fluxes (grid%dt, grid, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
          ifts, ifte, jfts, jfte, grid%fuel_load_g, fuel_frac_burnt, grid%fgrnhfx, grid%fgrnqfx)

      call Calc_smoke_emissions (grid, config_flags, ifts, ifte, jfts, jfte)

    end subroutine Advance_fire_model

  end module fire_model_mod
