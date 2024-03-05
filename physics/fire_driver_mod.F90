  module fire_driver_mod

    use fire_model_mod, only : Advance_fire_model
    use level_set_mod, only : Extrapol_var_at_bdys
    use state_mod, only : state_fire_t
    use namelist_mod, only : namelist_t
    use stderrout_mod, only : Message, Stop_simulation

    use fuel_mod, only : FUEL_ANDERSON
    use fuel_anderson_mod, only : fuel_anderson_t

    use ros_mod, only : ROS_WRFFIRE
    use ros_wrffire_mod, only : ros_wrffire_t

    use fmc_mod, only : FMC_WRFFIRE
    use fmc_wrffire_mod, only : fmc_wrffire_t

    implicit none

    private

    public :: Advance_fire_components, Init_fire_components

    integer, parameter:: REAL_SUM = 10, REAL_MAX = 20, RNRM_SUM = 30, RNRM_MAX = 40

  contains

    subroutine Init_fire_components (grid, config_flags)

      implicit none

      type (state_fire_t) :: grid
      type (namelist_t), intent(in) :: config_flags

      integer :: ij


        ! Ignitions lines
      call grid%Init_ignition_lines (config_flags)

        ! Fuel model
      select case (config_flags%fuel_opt)
        case (FUEL_ANDERSON)
          allocate (fuel_anderson_t::grid%fuels)

        case default
          call Stop_simulation ('The selected fuel_opt does not exist')
      end select
      call grid%fuels%Initialization (config_flags%fuelmc_c)
      call grid%Init_fuel_vars ()

        ! FMC model
      select case (config_flags%fmc_opt)
        case (FMC_WRFFIRE)
          allocate (fmc_wrffire_t::grid%fmc_param)

        case default
          call Stop_simulation ('The selected fmc_param does not exist')
      end select
      if (config_flags%fmoist_run) call grid%fmc_param%Init (grid%fuels, config_flags%fuelmc_g, config_flags%fuelmc_g_live, grid%ifms, &
          grid%ifme, grid%jfms, grid%jfme, grid%itimestep, grid%dt)

        ! Rate of spread parameterization
      select case (config_flags%ros_opt)
        case (ROS_WRFFIRE)
          allocate (ros_wrffire_t::grid%ros_param)

        case default
          call Stop_simulation ('The selected ros_opt does not exist')
      end select
      call grid%ros_param%Init (grid%ifms, grid%ifme, grid%jfms, grid%jfme)

      do ij = 1, grid%num_tiles
        call Extrapol_var_at_bdys (grid%ifms, grid%ifme, grid%jfms, grid%jfme, grid%ifds, grid%ifde, &
            grid%jfds, grid%jfde, grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), &
            grid%lfn)

        call Extrapol_var_at_bdys (grid%ifms, grid%ifme, grid%jfms, grid%jfme, grid%ifds, grid%ifde, &
            grid%jfds, grid%jfde, grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), &
            grid%tign_g)

        call grid%ros_param%Set_params (grid%ifms, grid%ifme, grid%jfms, grid%jfme, grid%i_start(ij), grid%i_end(ij), &
            grid%j_start(ij), grid%j_end(ij), grid%fuels, grid%nfuel_cat, grid%fmc_g)
      end do

    end subroutine Init_fire_components

    subroutine Advance_fire_components (grid, config_flags)

      implicit none

      type (state_fire_t), intent (in out) :: grid
      type (namelist_t), intent (in) :: config_flags

      integer :: stat_lev = 1
      integer :: ij


      if (config_flags%fmoist_run) call grid%fmc_param%Advance_fmc_model (config_flags%fmoist_freq, config_flags%fmoist_dt, &
          grid%itimestep, grid%dt, grid%ifms, grid%ifme, grid%jfms, grid%jfme, &
          grid%i_start, grid%i_end, grid%j_start, &
          grid%j_end, grid%num_tiles, grid%fire_rain, grid%fire_t2, grid%fire_q2, grid%fire_psfc, &
          grid%fire_rain_old, grid%fire_t2_old, grid%fire_q2_old, grid%fire_psfc_old, grid%fire_rh_fire, config_flags%fuelmc_g, &
          grid%fmc_g, grid%nfuel_cat, grid%fuels, grid%ros_param)

      do ij = 1, grid%num_tiles
        call Advance_fire_model (config_flags, grid, &
            grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij))
      end do

      if (config_flags%fire_print_msg >= stat_lev) call Print_summary (config_flags, grid)

    end subroutine Advance_fire_components

    function Calc_domain_stats (fun, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, a, b) result (return_value)

      implicit none

      integer, intent (in) :: fun, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte
      real, dimension(ifms:ifme, jfms:jfme), intent (in) :: a, b
      real :: return_value

      real :: lsum, void
      integer :: i, j
      real, save :: psum, gsum
      logical :: dosum, domax
      character (len = 256) :: msg


      if (fun == REAL_SUM) then
        void = 0.0
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = lsum + a(i,j)
          end do
        end do
      else if (fun == RNRM_SUM) then
        void = 0.0
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = lsum + sqrt (a(i, j) * a(i, j) + b(i, j) * b(i, j))
          end do
        end do
      else if (fun == REAL_MAX) then
        void = - huge (lsum)
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = max (lsum, a(i, j))
          end do
        end do
      else if (fun == RNRM_MAX) then
        void = 0.0
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = max (lsum, sqrt (a(i, j) * a(i, j) + b(i, j) * b(i, j)))
          end do
        end do
      else
        call Stop_simulation ('fun_real: bad fun')
      end if

      if (lsum .ne. lsum) call Stop_simulation ('fun_real: NaN detected')

      dosum = fun == REAL_SUM .or. fun == RNRM_SUM
      domax = fun == REAL_MAX .or. fun == RNRM_MAX

      psum = void
      if (dosum) psum = psum + lsum
      if (domax) psum = max (psum, lsum)
      gsum = psum

      return_value = gsum

    end function Calc_domain_stats

    subroutine Print_summary (config_flags, grid)

      implicit none

      type (namelist_t), intent (in) :: config_flags
      type (state_fire_t), intent (in) :: grid

      real :: tfa, thf, mhf, tqf, mqf, aw, mw
      real :: time_start
      integer :: stat_lev = 1
      integer :: ifds, ifde, jfds, jfde, ifms, ifme, jfms, jfme
      character (len = 128) :: msg

      ifds = grid%ifds
      ifde = grid%ifde
      jfds = grid%jfds
      jfde = grid%jfde

      ifms = grid%ifms
      ifme = grid%ifme
      jfms = grid%jfms
      jfme = grid%jfme

      time_start = grid%itimestep * grid%dt

      aw = Calc_domain_stats (RNRM_SUM, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, grid%uf,grid%vf) / &
          ((ifde - ifds + 1) * (jfde - jfds + 1))
      mw = Calc_domain_stats (RNRM_MAX, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, grid%uf, grid%vf)

      tfa = Calc_domain_stats (REAL_SUM, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, grid%fire_area, &
         grid%fire_area) * grid%dx * grid%dy

      thf = Calc_domain_stats (REAL_SUM, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, grid%fgrnhfx, &
          grid%fgrnhfx) * grid%dx * grid%dy

      mhf = Calc_domain_stats (REAL_MAX, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, grid%fgrnhfx, &
          grid%fgrnhfx)

      tqf = Calc_domain_stats (REAL_SUM, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, grid%fgrnqfx, &
          grid%fgrnqfx) * grid%dx * grid%dy

      mqf = Calc_domain_stats (REAL_MAX, ifms, ifme, jfms, jfme, ifds, ifde, jfds, jfde, grid%fgrnqfx, &
          grid%fgrnqfx)

      write (msg, 91) time_start, 'Average wind        ', aw, 'm/s'
      call Message (msg, config_flags%fire_print_msg, stat_lev)

      write (msg, 91) time_start, 'Maximum wind        ', mw, 'm/s'
      call Message (msg, config_flags%fire_print_msg, stat_lev)

      write (msg, 91) time_start, 'Fire area           ', tfa, 'm^2'
      call Message (msg, config_flags%fire_print_msg, stat_lev)

      write (msg, 91) time_start, 'Heat output         ', thf, 'W'
      call Message (msg, config_flags%fire_print_msg, stat_lev)

      write (msg, 91) time_start,'Max heat flux       ', mhf, 'W/m^2'
      call Message (msg, config_flags%fire_print_msg, stat_lev)

      write (msg, 91) time_start,'Latent heat output  ', tqf, 'W'
      call Message(msg, config_flags%fire_print_msg, stat_lev)

      write (msg, 91) time_start,'Max latent heat flux',mqf,'W/m^2'
      call Message (msg, config_flags%fire_print_msg, stat_lev)

 91   format('Time ',f11.3,' s ',a,e12.3,1x,a)

    end subroutine Print_summary

  end module fire_driver_mod
