
  module module_fr_fire_driver

    use module_fr_fire_model, only: Fire_model
    use module_fr_fire_fuel_moisture_model, only: Init_fuel_moisture, Fuel_moisture_model
    use ignition_line_mod, only: ignition_line_t, Initialize_ignitions
    use module_fr_fire_fuel_anderson_mod, only: fuel_anderson_t
    use ros_wrffire_mod, only : ros_wrffire_t
    use state_mod, only: state_fire_t
    use namelist_mod, only: namelist_t
    use stderrout_mod, only: Crash, Message

    implicit none

    private

    public :: Fire_driver_em, Init_fire_driver

    integer, parameter:: REAL_SUM = 10, REAL_MAX = 20, RNRM_SUM = 30, RNRM_MAX = 40

    type (ignition_line_t), dimension(:), allocatable :: ignition_lines
    type (fuel_anderson_t) :: fuel_model
    type (ros_wrffire_t) :: ros_model

  contains

    subroutine Init_fire_driver (grid, config_flags)

      implicit none

      type (state_fire_t), target :: grid
      type (namelist_t), intent(in) :: config_flags

      integer :: ij


      call Initialize_ignitions (config_flags, ignition_lines)

      if (config_flags%fmoist_run) call Init_fuel_moisture (grid, config_flags, fuel_model)

      call fuel_model%Initialization (config_flags%fuelmc_c)

      do ij=1,grid%num_tiles
        call ros_model%Set_ros_parameters (grid%ifds, grid%ifde, grid%jfds, grid%jfde, &
            grid%ifms, grid%ifme, grid%jfms, grid%jfme, grid%i_start(ij), grid%i_end(ij), &
            grid%j_start(ij), grid%j_end(ij), grid%dx, grid%dy, grid%nfuel_cat,grid%fuel_time, &
            grid, fuel_model,config_flags)
      enddo

    end subroutine Init_fire_driver

    subroutine Fire_driver_em (grid, config_flags)

      implicit none

      type (state_fire_t), intent (in out) :: grid
      type (namelist_t), intent (in) :: config_flags

      integer :: stat_lev = 1
      integer :: ij

      if (config_flags%fmoist_run) call Fuel_moisture_model (grid, config_flags, fuel_model, ros_model)

      do ij=1,grid%num_tiles
        call Fire_model (config_flags, ros_model, ignition_lines, grid, &
            grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij))
      enddo

      if (config_flags%fire_print_msg >= stat_lev) then
        call Print_summary (config_flags, grid)
      endif

    end subroutine Fire_driver_em

          real function Calc_domain_stats (fun, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, a, b) result (return_value)

#ifdef DM_PARALLEL
      USE module_dm , only : wrf_dm_sum_real , wrf_dm_max_real
#endif

      implicit none

      integer, intent(in)::  fun, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte
      real, intent(in), dimension(ifms:ifme, jfms:jfme) :: a, b

      real :: lsum, void
      integer :: i, j
      real, save :: psum, gsum
      logical :: dosum, domax
      character (len = 256) :: msg


      if(fun .eq. REAL_SUM) then
        void = 0.0
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = lsum + a(i,j)
          end do
        end do
      else if (fun .eq. RNRM_SUM) then
        void = 0.0
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = lsum + sqrt (a(i, j) * a(i, j) + b(i, j) * b(i, j))
          end do
        end do
      else if (fun .eq. REAL_MAX) then
        void = - huge (lsum)
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = max (lsum, a(i, j))
          end do
        end do
      else if (fun .eq. RNRM_MAX) then
        void = 0.0
        lsum = void
        do j = jfts, jfte
          do i = ifts, ifte
            lsum = max (lsum, sqrt (a(i, j) * a(i, j) + b(i, j) * b(i, j)))
          end do
        end do
      else
        call Crash ('fun_real: bad fun')
      end if

      if (lsum .ne. lsum) call Crash ('fun_real: NaN detected')

      dosum = fun .eq. REAL_SUM .or. fun .eq. RNRM_SUM
      domax = fun .eq. REAL_MAX .or. fun .eq. RNRM_MAX

      ! get process sum over all threads
      !$OMP SINGLE
      ! only one thread should write to shared variable
      psum = void
      !$OMP END SINGLE
      !$OMP BARRIER
      ! now all threads know psum

      !$OMP CRITICAL(RDSUM)
      ! each thread adds its own lsum
      if (dosum) psum = psum + lsum
      if (domax) psum = max (psum, lsum)
      !$OMP END CRITICAL(RDSUM)

      ! wait till all theads are done
      !$OMP BARRIER

      ! get global sum over all processes
      !$OMP SINGLE
      ! only one threads will do the mpi communication
#ifdef DM_PARALLEL
          if (dosum) gsum = wrf_dm_sum_real ( psum )
          if (domax) gsum = wrf_dm_max_real ( psum )
#else
          gsum = psum
#endif
      if (gsum .ne. gsum) call Crash ('fun_real: NaN detected')
      !$OMP END SINGLE

      !$OMP BARRIER
      ! now gsum is known to all threads

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

      !$OMP MASTER
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
      !$OMP END MASTER

 91   format('Time ',f11.3,' s ',a,e12.3,1x,a)

    end subroutine Print_summary

  end module module_fr_fire_driver
