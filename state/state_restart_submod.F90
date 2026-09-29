  submodule (state_mod) state_restart_submod

#ifdef DM_PARALLEL
    use mpi
#endif
    use fmc_wrffire_mod, only : fmc_wrffire_t
    use mpi_mod, only : Do_halo_exchange_with_corners
    use netcdf_mod, only : Add_netcdf_att, Get_netcdf_att, Get_netcdf_var_mpi, Is_netcdf_file_present
    use, intrinsic :: iso_fortran_env, only : INT32, REAL32

    implicit none

    integer, parameter :: RESTART_IO_ROOT = 0
    character (len = *), parameter :: NAME_DIM_MOISTURE_CLASS = 'moisture_class'
    character (len = *), parameter :: NAME_VAR_FMC_GC = 'fmc_gc'
    character (len = *), parameter :: NAME_ATT_FMOIST_LASTTIME = 'fmc_fmoist_lasttime'
    character (len = *), parameter :: NAME_ATT_FMOIST_NEXTTIME = 'fmc_fmoist_nexttime'
    real, parameter :: RESTART_INTERVAL_TOL = 1.0e-6

  contains

    module procedure Build_restart_file_name

      file_restart = 'fire_restart_'//trim (restart_datetime)//'.nc'

    end procedure Build_restart_file_name

    module procedure Handle_restart

      real :: restart_interval_seconds, restart_interval_error
      character (len = 256) :: msg
      logical :: do_initialize


      do_initialize = .false.
      if (present (initialize)) do_initialize = initialize

      if (do_initialize .or. .not. this%is_restart_output_initialized) then
        this%restart_step_interval = -1
        if (config_flags%restart_interval > 0) then
          this%restart_step_interval = nint (real (config_flags%restart_interval) / this%dt)
          restart_interval_seconds = this%restart_step_interval * this%dt
          restart_interval_error = abs (restart_interval_seconds - real (config_flags%restart_interval))

          if (this%restart_step_interval <= 0 .or. &
              restart_interval_error > max (RESTART_INTERVAL_TOL, abs (real (config_flags%restart_interval)) * RESTART_INTERVAL_TOL)) then
            write (msg, '(a, i0, a, f12.6)') 'restart_interval must map to an integer number of time steps: restart_interval = ', &
                config_flags%restart_interval, ', dt = ', this%dt
            call Stop_simulation (msg)
          end if
        end if
        this%is_restart_output_initialized = .true.
      end if

      if (do_initialize) return

      if (this%restart_step_interval > 0) then
        if (mod (this%itimestep, this%restart_step_interval) == 0) call this%Write_restart (config_flags)
      end if

    end procedure Handle_restart

    module procedure Init_restart_dimensions

      integer :: restart_nx, restart_ny
      integer (kind = INT32) :: att_int32
      integer :: ntasks, px, py, ierr, cart_comm, rank
      integer, dimension(2) :: coords
      character (len = 300) :: msg


      restart_nx = 0
      restart_ny = 0
      if (Is_restart_io_root (this)) then
        call Get_netcdf_att (trim (restart_file), 'global', 'nx', att_int32)
        restart_nx = att_int32
        call Get_netcdf_att (trim (restart_file), 'global', 'ny', att_int32)
        restart_ny = att_int32
      end if
      call Broadcast_restart_integer (this, restart_nx)
      call Broadcast_restart_integer (this, restart_ny)

      ids0 = 1
      ide0 = restart_nx
      jds0 = 1
      jde0 = restart_ny

#ifdef DM_PARALLEL
      if (.not. this%is_cfbm_comm_set) call Stop_simulation ('The MPI CFBM communicator has not been set')

      call Mpi_comm_size (this%cfbm_comm, ntasks, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems getting the number of MPI tasks')
      this%ntasks = ntasks

      call Calc_tasks_in_x_and_y (this%ntasks, restart_nx, restart_ny, px, py)
      this%px = px
      this%py = py
      write (msg, '(a25, 2(1x, i5))') 'MPI TASKS in x and y =', this%px, this%py
      call Print_message (msg)

      call Mpi_cart_create (this%cfbm_comm, N_DIMS, [this%px, this%py], PERIODS, REORDER, cart_comm, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems with Mpi_cart_create')
      this%cart_comm = cart_comm

      call Mpi_comm_rank (this%cart_comm, rank, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems with Mpi_comm_rank ')

      call Mpi_cart_coords (this%cart_comm, rank, N_DIMS, coords, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems with Mpi_cart_coords')

      call Calc_patch_dims (restart_nx, restart_ny, this%px, this%py, coords, ips, ipe, jps, jpe)
#else
      ips = ids0
      ipe = ide0
      jps = jds0
      jpe = jde0
#endif

    end procedure Init_restart_dimensions

    module procedure Init_restart_projection

      integer :: restart_map_proj, restart_sr_x, restart_sr_y
      integer (kind = INT32) :: att_int32
      real (kind = REAL32) :: att_real32
      real :: restart_dx, restart_dy, restart_cen_lat, restart_cen_lon, restart_stand_lon, restart_true_lat_1, restart_true_lat_2


      restart_dx = 0.0
      restart_dy = 0.0
      restart_map_proj = 0
      restart_sr_x = 0
      restart_sr_y = 0
      restart_cen_lat = 0.0
      restart_cen_lon = 0.0
      restart_stand_lon = 0.0
      restart_true_lat_1 = 0.0
      restart_true_lat_2 = 0.0

      if (Is_restart_io_root (this)) then
        call Get_netcdf_att (trim (restart_file), 'global', 'dx', att_real32)
        restart_dx = att_real32
        call Get_netcdf_att (trim (restart_file), 'global', 'dy', att_real32)
        restart_dy = att_real32
        call Get_netcdf_att (trim (restart_file), 'global', 'map_proj', att_int32)
        restart_map_proj = att_int32
        call Get_netcdf_att (trim (restart_file), 'global', 'sr_x', att_int32)
        restart_sr_x = att_int32
        call Get_netcdf_att (trim (restart_file), 'global', 'sr_y', att_int32)
        restart_sr_y = att_int32
        call Get_netcdf_att (trim (restart_file), 'global', 'cen_lat', att_real32)
        restart_cen_lat = att_real32
        call Get_netcdf_att (trim (restart_file), 'global', 'cen_lon', att_real32)
        restart_cen_lon = att_real32
        call Get_netcdf_att (trim (restart_file), 'global', 'stand_lon', att_real32)
        restart_stand_lon = att_real32
        call Get_netcdf_att (trim (restart_file), 'global', 'true_lat_1', att_real32)
        restart_true_lat_1 = att_real32
        call Get_netcdf_att (trim (restart_file), 'global', 'true_lat_2', att_real32)
        restart_true_lat_2 = att_real32
      end if

      call Broadcast_restart_real (this, restart_dx)
      call Broadcast_restart_real (this, restart_dy)
      call Broadcast_restart_integer (this, restart_map_proj)
      call Broadcast_restart_integer (this, restart_sr_x)
      call Broadcast_restart_integer (this, restart_sr_y)
      call Broadcast_restart_real (this, restart_cen_lat)
      call Broadcast_restart_real (this, restart_cen_lon)
      call Broadcast_restart_real (this, restart_stand_lon)
      call Broadcast_restart_real (this, restart_true_lat_1)
      call Broadcast_restart_real (this, restart_true_lat_2)

      this%dx = restart_dx
      this%dy = restart_dy

      if (restart_map_proj /= 1) call Stop_simulation ('Restart map projection is not supported')

      if (restart_sr_x <= 0 .or. restart_sr_y <= 0) call Stop_simulation ('Restart subgrid ratios must be positive')
      if (mod (this%nx, restart_sr_x) /= 0 .or. mod (this%ny, restart_sr_y) /= 0) &
          call Stop_simulation ('Restart subgrid ratios do not divide fire-grid dimensions')
      if (this%nx / restart_sr_x <= 1 .or. this%ny / restart_sr_y <= 1) &
          call Stop_simulation ('Restart subgrid ratios imply invalid atmospheric dimensions')

      this%cen_lat = restart_cen_lat
      this%cen_lon = restart_cen_lon

      proj = proj_lc_t (cen_lat = this%cen_lat , cen_lon = this%cen_lon, dx = this%dx * restart_sr_x, &
          dy = this%dy * restart_sr_y, &
          standard_lon = restart_stand_lon, true_lat_1 = restart_true_lat_1, &
          true_lat_2 = restart_true_lat_2, nx = this%nx / restart_sr_x - 1, ny = this%ny / restart_sr_y - 1)

      call this%Init_latlons (proj, srx = restart_sr_x, sry = restart_sr_y)

    end procedure Init_restart_projection

    module procedure Read_restart_static_fields

      call Read_restart_field_2d (trim (restart_file), 'zsf', this%nx, this%ny, &
          this%ifms, this%ifme, this%jfms, this%jfme, this%ifps, this%ifpe, this%jfps, this%jfpe, this%cfbm_comm, this%zsf)
      call Read_restart_field_2d (trim (restart_file), 'dzdxf', this%nx, this%ny, &
          this%ifms, this%ifme, this%jfms, this%jfme, this%ifps, this%ifpe, this%jfps, this%jfpe, this%cfbm_comm, this%dzdxf)
      call Read_restart_field_2d (trim (restart_file), 'dzdyf', this%nx, this%ny, &
          this%ifms, this%ifme, this%jfms, this%jfme, this%ifps, this%ifpe, this%jfps, this%jfpe, this%cfbm_comm, this%dzdyf)
      call Read_restart_field_2d (trim (restart_file), 'nfuel_cat', this%nx, this%ny, &
          this%ifms, this%ifme, this%jfms, this%jfme, this%ifps, this%ifpe, this%jfps, this%jfpe, this%cfbm_comm, this%nfuel_cat)
      call Read_restart_field_2d (trim (restart_file), 'fz0', this%nx, this%ny, &
          this%ifms, this%ifme, this%jfms, this%jfme, this%ifps, this%ifpe, this%jfps, this%jfpe, this%cfbm_comm, this%fz0)

    end procedure Read_restart_static_fields

    subroutine Require_restart_mpi_comm (this)

      implicit none

      class (state_fire_t), intent (in) :: this


#ifdef DM_PARALLEL
      if (.not. this%is_cfbm_comm_set) call Stop_simulation ('The MPI CFBM communicator has not been set')
#endif

    end subroutine Require_restart_mpi_comm

    function Is_restart_io_root (this) result (am_root)

      implicit none

      class (state_fire_t), intent (in) :: this
      logical :: am_root

      integer :: rank, ierr


#ifdef DM_PARALLEL
      call Require_restart_mpi_comm (this)
      call Mpi_comm_rank (this%cfbm_comm, rank, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems with Mpi_comm_rank ')
      am_root = rank == RESTART_IO_ROOT
#else
      am_root = .true.
#endif

    end function Is_restart_io_root

    subroutine Restart_io_barrier (this)

      implicit none

      class (state_fire_t), intent (in) :: this

      integer :: ierr


#ifdef DM_PARALLEL
      call Require_restart_mpi_comm (this)
      call MPI_Barrier (this%cfbm_comm, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems with MPI_Barrier ')
#endif

    end subroutine Restart_io_barrier

    subroutine Broadcast_restart_integer (this, value)

      implicit none

      class (state_fire_t), intent (in) :: this
      integer, intent (in out) :: value

      integer :: ierr


#ifdef DM_PARALLEL
      call Require_restart_mpi_comm (this)
      call MPI_Bcast (value, 1, MPI_INTEGER, RESTART_IO_ROOT, this%cfbm_comm, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems with MPI_Bcast for restart integer metadata')
#endif

    end subroutine Broadcast_restart_integer

    subroutine Broadcast_restart_real (this, value)

      implicit none

      class (state_fire_t), intent (in) :: this
      real, intent (in out) :: value

      integer :: packed_value


#ifdef DM_PARALLEL
      packed_value = 0
      if (Is_restart_io_root (this)) packed_value = transfer (value, packed_value)
      call Broadcast_restart_integer (this, packed_value)
      value = transfer (packed_value, value)
#endif

    end subroutine Broadcast_restart_real

    module procedure Read_restart

      type (datetime_t) :: datetime_restart, datetime_check
      character (len = :), allocatable :: file_restart
      integer (kind = INT32) :: att_int, restart_year, restart_month, restart_day, restart_hour, restart_minute, restart_second, &
          start_year, start_month, start_day, start_hour, start_minute, start_second
      integer :: ij, expected_sr_x, expected_sr_y, ierr, rank
      logical, parameter :: DEBUG_LOCAL = .false.


      if (DEBUG_LOCAL) call Print_message ('Entering Read_restart...')

#ifdef DM_PARALLEL
      call Require_restart_mpi_comm (this)
      call Mpi_comm_rank (this%cfbm_comm, rank, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems with Mpi_comm_rank ')
#else
      rank = 0
#endif

      if (config_flags%ideal_opt /= 0 .and. config_flags%ideal_opt /= 1) &
          call Stop_simulation ('Read_restart is implemented for standalone idealized and real runs only')

      datetime_restart = datetime_t (config_flags%start_year, config_flags%start_month, config_flags%start_day, &
          config_flags%start_hour, config_flags%start_minute, config_flags%start_second)
      file_restart = Build_restart_file_name (datetime_restart%datetime)

      if (rank == 0) then
        call Is_netcdf_file_present (file_restart)

        call Validate_restart_integer (file_restart, 'restart_year', config_flags%start_year)
        call Validate_restart_integer (file_restart, 'restart_month', config_flags%start_month)
        call Validate_restart_integer (file_restart, 'restart_day', config_flags%start_day)
        call Validate_restart_integer (file_restart, 'restart_hour', config_flags%start_hour)
        call Validate_restart_integer (file_restart, 'restart_minute', config_flags%start_minute)
        call Validate_restart_integer (file_restart, 'restart_second', config_flags%start_second)

        call Validate_restart_integer (file_restart, 'nx', this%nx)
        call Validate_restart_integer (file_restart, 'ny', this%ny)
        call Validate_restart_real (file_restart, 'dt', this%dt)
        call Validate_restart_real (file_restart, 'dx', this%dx)
        call Validate_restart_real (file_restart, 'dy', this%dy)
        call Validate_restart_real (file_restart, 'cen_lat', this%cen_lat)
        call Validate_restart_real (file_restart, 'cen_lon', this%cen_lon)
        expected_sr_x = nint (this%proj%dx / this%dx)
        expected_sr_y = nint (this%proj%dy / this%dy)
        call Validate_restart_integer (file_restart, 'map_proj', 1)
        call Validate_restart_integer (file_restart, 'sr_x', expected_sr_x)
        call Validate_restart_integer (file_restart, 'sr_y', expected_sr_y)
        call Validate_restart_integer (file_restart, 'ideal_opt', config_flags%ideal_opt)
        call Validate_restart_integer (file_restart, 'fuel_opt', config_flags%fuel_opt)
        call Validate_restart_integer (file_restart, 'ros_opt', config_flags%ros_opt)
        call Validate_restart_integer (file_restart, 'fmc_opt', config_flags%fmc_opt)
        call Validate_restart_integer (file_restart, 'emis_opt', config_flags%emis_opt)
        call Validate_restart_integer (file_restart, 'fire_upwinding', config_flags%fire_upwinding)
        call Validate_restart_integer (file_restart, 'fire_upwinding_reinit', config_flags%fire_upwinding_reinit)
        call Validate_restart_integer (file_restart, 'fire_lsm_reinit_iter', config_flags%fire_lsm_reinit_iter)
        call Validate_restart_real (file_restart, 'fire_viscosity', config_flags%fire_viscosity)
        call Validate_restart_real (file_restart, 'fire_viscosity_bg', config_flags%fire_viscosity_bg)
        call Validate_restart_integer (file_restart, 'fire_viscosity_ngp', config_flags%fire_viscosity_ngp)
        call Validate_restart_real (file_restart, 'fire_viscosity_band', config_flags%fire_viscosity_band)
        call Validate_restart_real (file_restart, 'reinit_pseudot_coef', config_flags%reinit_pseudot_coef)

        select case (config_flags%ideal_opt)
          case (0)
            call Validate_restart_real (file_restart, 'stand_lon', this%proj%standard_lon)
            call Validate_restart_real (file_restart, 'true_lat_1', this%proj%true_lat_1)
            call Validate_restart_real (file_restart, 'true_lat_2', this%proj%true_lat_2)

          case (1)
            call Validate_restart_real (file_restart, 'stand_lon', config_flags%stand_lon)
            call Validate_restart_real (file_restart, 'true_lat_1', config_flags%true_lat_1)
            call Validate_restart_real (file_restart, 'true_lat_2', config_flags%true_lat_2)
        end select

        call Get_netcdf_att (file_restart, 'global', 'start_year', start_year)
        call Get_netcdf_att (file_restart, 'global', 'start_month', start_month)
        call Get_netcdf_att (file_restart, 'global', 'start_day', start_day)
        call Get_netcdf_att (file_restart, 'global', 'start_hour', start_hour)
        call Get_netcdf_att (file_restart, 'global', 'start_minute', start_minute)
        call Get_netcdf_att (file_restart, 'global', 'start_second', start_second)

        call Get_netcdf_att (file_restart, 'global', 'restart_year', restart_year)
        call Get_netcdf_att (file_restart, 'global', 'restart_month', restart_month)
        call Get_netcdf_att (file_restart, 'global', 'restart_day', restart_day)
        call Get_netcdf_att (file_restart, 'global', 'restart_hour', restart_hour)
        call Get_netcdf_att (file_restart, 'global', 'restart_minute', restart_minute)
        call Get_netcdf_att (file_restart, 'global', 'restart_second', restart_second)

        call Get_netcdf_att (file_restart, 'global', 'itimestep', att_int)
      end if

      call Broadcast_restart_integer (this, start_year)
      call Broadcast_restart_integer (this, start_month)
      call Broadcast_restart_integer (this, start_day)
      call Broadcast_restart_integer (this, start_hour)
      call Broadcast_restart_integer (this, start_minute)
      call Broadcast_restart_integer (this, start_second)
      call Broadcast_restart_integer (this, restart_year)
      call Broadcast_restart_integer (this, restart_month)
      call Broadcast_restart_integer (this, restart_day)
      call Broadcast_restart_integer (this, restart_hour)
      call Broadcast_restart_integer (this, restart_minute)
      call Broadcast_restart_integer (this, restart_second)
      call Broadcast_restart_integer (this, att_int)
      this%itimestep = att_int

      this%datetime_start = datetime_t (start_year, start_month, start_day, start_hour, start_minute, start_second)
      this%datetime_now = datetime_t (restart_year, restart_month, restart_day, restart_hour, restart_minute, restart_second)

      datetime_check = this%datetime_start
      call datetime_check%Add_seconds (this%itimestep * this%dt)
      if (datetime_check /= this%datetime_now) call Stop_simulation ('Restart clock is inconsistent with itimestep and dt')

      call Set_next_datetime_after_restart (this%datetime_next_output, config_flags%interval_output)
      call Set_next_datetime_after_restart (this%datetime_next_atm_update, config_flags%interval_atm)

      call Read_restart_field (file_restart, 'lfn', this%lfn)
      call Read_restart_field (file_restart, 'lfn_hist', this%lfn_hist)
      call Read_restart_field (file_restart, 'lfn_0', this%lfn_0)
      call Read_restart_field (file_restart, 'lfn_1', this%lfn_1)
      call Read_restart_field (file_restart, 'lfn_2', this%lfn_2)
      call Read_restart_field (file_restart, 'lfn_s0', this%lfn_s0)
      call Read_restart_field (file_restart, 'lfn_s1', this%lfn_s1)
      call Read_restart_field (file_restart, 'lfn_s2', this%lfn_s2)
      call Read_restart_field (file_restart, 'lfn_s3', this%lfn_s3)
      call Read_restart_field (file_restart, 'lfn_out', this%lfn_out)
      call Read_restart_field (file_restart, 'tign_g', this%tign_g)
      call Read_restart_field (file_restart, 'fuel_frac', this%fuel_frac)
      call Read_restart_field (file_restart, 'fire_area', this%fire_area)
      call Read_restart_field (file_restart, 'fuel_frac_burnt_dt', this%fuel_frac_burnt_dt)
      call Read_restart_field (file_restart, 'fgrnhfx', this%fgrnhfx)
      call Read_restart_field (file_restart, 'fgrnqfx', this%fgrnqfx)
      call Read_restart_field (file_restart, 'fcanhfx', this%fcanhfx)
      call Read_restart_field (file_restart, 'fcanqfx', this%fcanqfx)
      call Read_restart_field (file_restart, 'flame_length', this%flame_length)
      call Read_restart_field (file_restart, 'ros', this%ros)
      call Read_restart_field (file_restart, 'ros_front', this%ros_front)
      call Read_restart_field (file_restart, 'emis_smoke', this%emis_smoke)
      call Read_restart_field (file_restart, 'fmc_g', this%fmc_g)
      call Read_restart_field (file_restart, 'fuel_load_g', this%fuel_load_g)
      call Read_restart_field (file_restart, 'fuel_time', this%fuel_time)
      call Read_restart_field (file_restart, 'zsf', this%zsf)
      call Read_restart_field (file_restart, 'dzdxf', this%dzdxf)
      call Read_restart_field (file_restart, 'dzdyf', this%dzdyf)
      call Read_restart_field (file_restart, 'nfuel_cat', this%nfuel_cat)
      call Read_restart_field (file_restart, 'uf', this%uf)
      call Read_restart_field (file_restart, 'vf', this%vf)
      call Read_restart_field (file_restart, 'fz0', this%fz0)

      if (config_flags%ideal_opt == 0) then
        call Read_restart_field (file_restart, 'fire_t2', this%fire_t2)
        call Read_restart_field (file_restart, 'fire_q2', this%fire_q2)
        call Read_restart_field (file_restart, 'fire_psfc', this%fire_psfc)
        call Read_restart_field (file_restart, 'fire_rain', this%fire_rain)
        if (config_flags%fmoist_run) then
          call Read_restart_field (file_restart, 'fire_t2_old', this%fire_t2_old)
          call Read_restart_field (file_restart, 'fire_q2_old', this%fire_q2_old)
          call Read_restart_field (file_restart, 'fire_psfc_old', this%fire_psfc_old)
          call Read_restart_field (file_restart, 'fire_rain_old', this%fire_rain_old)
        end if
      end if
      if (config_flags%fmoist_run) call Read_restart_fmc (file_restart)

      call Exchange_restart_halos ()

      if (allocated (this%ros_param) .and. allocated (this%fuels)) then
        do ij = 1, this%num_tiles
          call this%ros_param%Set_params (this%ifms, this%ifme, this%jfms, this%jfme, this%i_start(ij), this%i_end(ij), &
              this%j_start(ij), this%j_end(ij), this%fuels, this%nfuel_cat, this%fmc_g)
        end do
      end if

      if (DEBUG_LOCAL) call Print_message ('Leaving Read_restart...')

    contains

      subroutine Exchange_restart_halos ()

        implicit none


#ifdef DM_PARALLEL
        call Exchange_restart_field (this%lfn)
        call Exchange_restart_field (this%lfn_hist)
        call Exchange_restart_field (this%lfn_0)
        call Exchange_restart_field (this%lfn_1)
        call Exchange_restart_field (this%lfn_2)
        call Exchange_restart_field (this%lfn_s0)
        call Exchange_restart_field (this%lfn_s1)
        call Exchange_restart_field (this%lfn_s2)
        call Exchange_restart_field (this%lfn_s3)
        call Exchange_restart_field (this%lfn_out)
        call Exchange_restart_field (this%tign_g)
        call Exchange_restart_field (this%fuel_frac)
        call Exchange_restart_field (this%fire_area)
        call Exchange_restart_field (this%fuel_frac_burnt_dt)
        call Exchange_restart_field (this%fgrnhfx)
        call Exchange_restart_field (this%fgrnqfx)
        call Exchange_restart_field (this%fcanhfx)
        call Exchange_restart_field (this%fcanqfx)
        call Exchange_restart_field (this%flame_length)
        call Exchange_restart_field (this%ros)
        call Exchange_restart_field (this%ros_front)
        call Exchange_restart_field (this%emis_smoke)
        call Exchange_restart_field (this%fmc_g)
        call Exchange_restart_field (this%fuel_load_g)
        call Exchange_restart_field (this%fuel_time)
        call Exchange_restart_field (this%zsf)
        call Exchange_restart_field (this%dzdxf)
        call Exchange_restart_field (this%dzdyf)
        call Exchange_restart_field (this%nfuel_cat)
        call Exchange_restart_field (this%uf)
        call Exchange_restart_field (this%vf)
        call Exchange_restart_field (this%fz0)

        if (config_flags%ideal_opt == 0) then
          call Exchange_restart_field (this%fire_t2)
          call Exchange_restart_field (this%fire_q2)
          call Exchange_restart_field (this%fire_psfc)
          call Exchange_restart_field (this%fire_rain)
          if (config_flags%fmoist_run) then
            call Exchange_restart_field (this%fire_t2_old)
            call Exchange_restart_field (this%fire_q2_old)
            call Exchange_restart_field (this%fire_psfc_old)
            call Exchange_restart_field (this%fire_rain_old)
          end if
        end if

        if (config_flags%fmoist_run) call Exchange_restart_fmc_halos ()
#endif

      end subroutine Exchange_restart_halos

      subroutine Exchange_restart_field (var)

        implicit none

        real, dimension(this%ifms:this%ifme, this%jfms:this%jfme), intent (in out) :: var


#ifdef DM_PARALLEL
        call Do_halo_exchange_with_corners (var, this%ifms, this%ifme, this%jfms, this%jfme, &
            this%ifps, this%ifpe, this%jfps, this%jfpe, N_POINTS_IN_HALO, this%cart_comm)
#endif

      end subroutine Exchange_restart_field

      subroutine Exchange_restart_fmc_halos ()

        implicit none

        integer :: k, n_moisture_classes


#ifdef DM_PARALLEL
        if (.not. allocated (this%fmc_param)) call Stop_simulation ('FMC restart halo exchange requires allocated fmc_param')

        select type (fmc_param => this%fmc_param)
          type is (fmc_wrffire_t)
            if (.not. allocated (fmc_param%fmc_gc)) call Stop_simulation ('FMC restart halo exchange requires allocated fmc_gc')

            n_moisture_classes = size (fmc_param%fmc_gc, 2)
            do k = 1, n_moisture_classes
              call Do_halo_exchange_with_corners (fmc_param%fmc_gc(this%ifms:this%ifme, k, this%jfms:this%jfme), &
                  this%ifms, this%ifme, this%jfms, this%jfme, &
                  this%ifps, this%ifpe, this%jfps, this%jfpe, N_POINTS_IN_HALO, this%cart_comm)
            end do

          class default
            call Stop_simulation ('Restart halo exchange is not implemented for selected FMC component')
        end select
#endif

      end subroutine Exchange_restart_fmc_halos

      subroutine Read_restart_field (file_name, var_name, var)

        implicit none

        character (len = *), intent (in) :: file_name, var_name
        real, dimension(this%ifms:this%ifme, this%jfms:this%jfme), intent (in out) :: var


        call Get_netcdf_var_mpi (file_name, this%cfbm_comm, this%nx, this%ny, &
            this%ifps, this%ifpe, this%jfps, this%jfpe, var_name, var(this%ifps:this%ifpe, this%jfps:this%jfpe))

      end subroutine Read_restart_field

      subroutine Read_restart_fmc (file_name)

        implicit none

        character (len = *), intent (in) :: file_name

        real (kind = REAL32) :: att_real32
        real :: fmoist_lasttime, fmoist_nexttime
        integer :: n_moisture_classes


        if (.not. allocated (this%fmc_param)) call Stop_simulation ('FMC restart read requires allocated fmc_param')

        select type (fmc_param => this%fmc_param)
          type is (fmc_wrffire_t)
            if (.not. allocated (fmc_param%fmc_gc)) call Stop_simulation ('FMC restart read requires allocated fmc_gc')

            n_moisture_classes = size (fmc_param%fmc_gc, 2)
            call Get_netcdf_var_mpi (file_name, this%cfbm_comm, this%nx, this%ny, n_moisture_classes, &
                this%ifps, this%ifpe, this%jfps, this%jfpe, NAME_VAR_FMC_GC, &
                fmc_param%fmc_gc(this%ifps:this%ifpe, 1:n_moisture_classes, this%jfps:this%jfpe))

            fmoist_lasttime = 0.0
            fmoist_nexttime = 0.0
            if (rank == 0) then
              call Get_netcdf_att (file_name, 'global', NAME_ATT_FMOIST_LASTTIME, att_real32)
              fmoist_lasttime = att_real32
              call Get_netcdf_att (file_name, 'global', NAME_ATT_FMOIST_NEXTTIME, att_real32)
              fmoist_nexttime = att_real32
            end if
            call Broadcast_restart_real (this, fmoist_lasttime)
            call Broadcast_restart_real (this, fmoist_nexttime)
            fmc_param%fmoist_lasttime = fmoist_lasttime
            fmc_param%fmoist_nexttime = fmoist_nexttime

          class default
            call Stop_simulation ('Restart read is not implemented for selected FMC component')
        end select

      end subroutine Read_restart_fmc

      subroutine Set_next_datetime_after_restart (datetime_next, interval_seconds)

        implicit none

        type (datetime_t), intent (in out) :: datetime_next
        integer, intent (in) :: interval_seconds

        datetime_next = this%datetime_now
        if (interval_seconds > 0) then
          datetime_next = this%datetime_start
          do while (datetime_next <= this%datetime_now)
            call datetime_next%Add_seconds (interval_seconds)
          end do
        end if

      end subroutine Set_next_datetime_after_restart

    end procedure Read_restart

    subroutine Read_restart_field_2d (file_name, var_name, nx, ny, ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe, cfbm_comm, var)

      implicit none

      character (len = *), intent (in) :: file_name, var_name
      integer, intent (in) :: nx, ny, ifms, ifme, jfms, jfme, ifps, ifpe, jfps, jfpe, cfbm_comm
      real, dimension(ifms:ifme, jfms:jfme), intent (in out) :: var


      call Get_netcdf_var_mpi (file_name, cfbm_comm, nx, ny, ifps, ifpe, jfps, jfpe, &
          var_name, var(ifps:ifpe, jfps:jfpe))

    end subroutine Read_restart_field_2d

    subroutine Validate_restart_integer (file_name, att_name, expected_value)

      implicit none

      character (len = *), intent (in) :: file_name, att_name
      integer, intent (in) :: expected_value

      integer (kind = INT32) :: att_value
      character (len = :), allocatable :: msg


      call Get_netcdf_att (file_name, 'global', att_name, att_value)
      if (att_value /= expected_value) then
        msg = 'Restart metadata mismatch: '//trim (att_name)
        call Stop_simulation (msg)
      end if

    end subroutine Validate_restart_integer

    subroutine Validate_restart_real (file_name, att_name, expected_value)

      implicit none

      character (len = *), intent (in) :: file_name, att_name
      real, intent (in) :: expected_value

      real (kind = REAL32) :: att_value, tolerance
      character (len = :), allocatable :: msg


      call Get_netcdf_att (file_name, 'global', att_name, att_value)
      tolerance = max (1.0e-6_REAL32, abs (real (expected_value, kind = REAL32)) * 1.0e-6_REAL32)
      if (abs (att_value - real (expected_value, kind = REAL32)) > tolerance) then
        msg = 'Restart metadata mismatch: '//trim (att_name)
        call Stop_simulation (msg)
      end if

    end subroutine Validate_restart_real

    module procedure Write_restart

      character (len = :), allocatable :: file_restart
      integer :: start_year, start_month, start_day, start_hour, start_minute, start_second, &
          restart_year, restart_month, restart_day, restart_hour, restart_minute, restart_second
      integer :: restart_sr_x, restart_sr_y, rank, ierr
      logical, parameter :: DEBUG_LOCAL = .false.


      if (DEBUG_LOCAL) call Print_message ('Entering Write_restart...')

#ifdef DM_PARALLEL
      if (.not. this%is_cfbm_comm_set) call Stop_simulation ('The MPI CFBM communicator has not been set')
      call Mpi_comm_rank (this%cfbm_comm, rank, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems with Mpi_comm_rank ')
#else
      rank = 0
#endif

      if (config_flags%ideal_opt /= 0 .and. config_flags%ideal_opt /= 1) &
          call Stop_simulation ('Write_restart is implemented for standalone idealized and real runs only')

      file_restart = Build_restart_file_name (this%datetime_now%datetime)

      call this%datetime_start%Get_datetime_as_ints (start_year, start_month, start_day, start_hour, start_minute, start_second)
      call this%datetime_now%Get_datetime_as_ints (restart_year, restart_month, restart_day, restart_hour, restart_minute, restart_second)
      restart_sr_x = nint (this%proj%dx / this%dx)
      restart_sr_y = nint (this%proj%dy / this%dy)
      if (restart_sr_x <= 0 .or. restart_sr_y <= 0) call Stop_simulation ('Restart subgrid ratios must be positive')

      if (rank == 0) then
        call Create_netcdf_file (file_name = file_restart)
        call Add_netcdf_dim (file_restart, NAME_DIM_X, this%nx)
        call Add_netcdf_dim (file_restart, NAME_DIM_Y, this%ny)

        call Add_netcdf_att (file_restart, 'global', 'start_year', int (start_year, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'start_month', int (start_month, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'start_day', int (start_day, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'start_hour', int (start_hour, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'start_minute', int (start_minute, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'start_second', int (start_second, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'restart_year', int (restart_year, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'restart_month', int (restart_month, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'restart_day', int (restart_day, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'restart_hour', int (restart_hour, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'restart_minute', int (restart_minute, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'restart_second', int (restart_second, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'itimestep', int (this%itimestep, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'nx', int (this%nx, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'ny', int (this%ny, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'dt', real (this%dt, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'dx', real (this%dx, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'dy', real (this%dy, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'cen_lat', real (this%cen_lat, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'cen_lon', real (this%cen_lon, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'map_proj', int (1, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'sr_x', int (restart_sr_x, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'sr_y', int (restart_sr_y, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'stand_lon', real (this%proj%standard_lon, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'true_lat_1', real (this%proj%true_lat_1, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'true_lat_2', real (this%proj%true_lat_2, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'ideal_opt', int (config_flags%ideal_opt, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'fuel_opt', int (config_flags%fuel_opt, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'ros_opt', int (config_flags%ros_opt, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'fmc_opt', int (config_flags%fmc_opt, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'emis_opt', int (config_flags%emis_opt, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'fire_upwinding', int (config_flags%fire_upwinding, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'fire_upwinding_reinit', int (config_flags%fire_upwinding_reinit, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'fire_lsm_reinit_iter', int (config_flags%fire_lsm_reinit_iter, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'fire_viscosity', real (config_flags%fire_viscosity, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'fire_viscosity_bg', real (config_flags%fire_viscosity_bg, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'fire_viscosity_ngp', int (config_flags%fire_viscosity_ngp, kind = INT32))
        call Add_netcdf_att (file_restart, 'global', 'fire_viscosity_band', real (config_flags%fire_viscosity_band, kind = REAL32))
        call Add_netcdf_att (file_restart, 'global', 'reinit_pseudot_coef', real (config_flags%reinit_pseudot_coef, kind = REAL32))
      end if

      call Restart_io_barrier (this)

      call Add_restart_field ('lfn', this%lfn)
      call Add_restart_field ('lfn_hist', this%lfn_hist)
      call Add_restart_field ('lfn_0', this%lfn_0)
      call Add_restart_field ('lfn_1', this%lfn_1)
      call Add_restart_field ('lfn_2', this%lfn_2)
      call Add_restart_field ('lfn_s0', this%lfn_s0)
      call Add_restart_field ('lfn_s1', this%lfn_s1)
      call Add_restart_field ('lfn_s2', this%lfn_s2)
      call Add_restart_field ('lfn_s3', this%lfn_s3)
      call Add_restart_field ('lfn_out', this%lfn_out)
      call Add_restart_field ('tign_g', this%tign_g)
      call Add_restart_field ('fuel_frac', this%fuel_frac)
      call Add_restart_field ('fire_area', this%fire_area)
      call Add_restart_field ('fuel_frac_burnt_dt', this%fuel_frac_burnt_dt)
      call Add_restart_field ('fgrnhfx', this%fgrnhfx)
      call Add_restart_field ('fgrnqfx', this%fgrnqfx)
      call Add_restart_field ('fcanhfx', this%fcanhfx)
      call Add_restart_field ('fcanqfx', this%fcanqfx)
      call Add_restart_field ('flame_length', this%flame_length)
      call Add_restart_field ('ros', this%ros)
      call Add_restart_field ('ros_front', this%ros_front)
      call Add_restart_field ('emis_smoke', this%emis_smoke)
      call Add_restart_field ('fmc_g', this%fmc_g)
      call Add_restart_field ('fuel_load_g', this%fuel_load_g)
      call Add_restart_field ('fuel_time', this%fuel_time)
      call Add_restart_field ('zsf', this%zsf)
      call Add_restart_field ('dzdxf', this%dzdxf)
      call Add_restart_field ('dzdyf', this%dzdyf)
      call Add_restart_field ('nfuel_cat', this%nfuel_cat)
      call Add_restart_field ('uf', this%uf)
      call Add_restart_field ('vf', this%vf)
      call Add_restart_field ('fz0', this%fz0)

      if (config_flags%ideal_opt == 0) then
        call Add_restart_field ('fire_t2', this%fire_t2)
        call Add_restart_field ('fire_q2', this%fire_q2)
        call Add_restart_field ('fire_psfc', this%fire_psfc)
        call Add_restart_field ('fire_rain', this%fire_rain)
        if (config_flags%fmoist_run) then
          call Add_restart_field ('fire_t2_old', this%fire_t2_old)
          call Add_restart_field ('fire_q2_old', this%fire_q2_old)
          call Add_restart_field ('fire_psfc_old', this%fire_psfc_old)
          call Add_restart_field ('fire_rain_old', this%fire_rain_old)
        end if
      end if
      if (config_flags%fmoist_run) call Write_restart_fmc ()

      call Restart_io_barrier (this)

      if (DEBUG_LOCAL) call Print_message ('Leaving Write_restart...')

    contains

      subroutine Add_restart_field (var_name, var)

        implicit none

        character (len = *), intent (in) :: var_name
        real, dimension(this%ifms:this%ifme, this%jfms:this%jfme), intent (in) :: var


        call Add_netcdf_var_mpi (file_restart, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, &
            var_name, var(this%ifps:this%ifpe, this%jfps:this%jfpe))

      end subroutine Add_restart_field

      subroutine Write_restart_fmc ()

        implicit none

        character (len = 32), dimension(3) :: dim_names_fmc_gc
        integer :: n_moisture_classes


        if (.not. allocated (this%fmc_param)) call Stop_simulation ('FMC restart write requires allocated fmc_param')

        select type (fmc_param => this%fmc_param)
          type is (fmc_wrffire_t)
            if (.not. allocated (fmc_param%fmc_gc)) call Stop_simulation ('FMC restart write requires allocated fmc_gc')

            n_moisture_classes = size (fmc_param%fmc_gc, 2)
            if (rank == 0) call Add_netcdf_dim (file_restart, NAME_DIM_MOISTURE_CLASS, n_moisture_classes)
            call Restart_io_barrier (this)

            dim_names_fmc_gc(1) = NAME_DIM_X
            dim_names_fmc_gc(2) = NAME_DIM_MOISTURE_CLASS
            dim_names_fmc_gc(3) = NAME_DIM_Y
            call Add_netcdf_var_mpi (file_restart, dim_names_fmc_gc, this%cfbm_comm, this%nx, this%ny, n_moisture_classes, &
                this%ifps, this%ifpe, this%jfps, this%jfpe, NAME_VAR_FMC_GC, &
                fmc_param%fmc_gc(this%ifps:this%ifpe, 1:n_moisture_classes, this%jfps:this%jfpe))

            if (rank == 0) then
              call Add_netcdf_att (file_restart, 'global', NAME_ATT_FMOIST_LASTTIME, &
                  real (fmc_param%fmoist_lasttime, kind = REAL32))
              call Add_netcdf_att (file_restart, 'global', NAME_ATT_FMOIST_NEXTTIME, &
                  real (fmc_param%fmoist_nexttime, kind = REAL32))
            end if

          class default
            call Stop_simulation ('Restart write is not implemented for selected FMC component')
        end select

      end subroutine Write_restart_fmc

    end procedure Write_restart

  end submodule state_restart_submod
