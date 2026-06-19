  module state_mod

#ifdef DM_PARALLEL
    use mpi
#endif
    use constants_mod, only : PI
    use datetime_mod, only : datetime_t
    use fmc_mod, only : fmc_t
    use fmc_wrffire_mod, only : fmc_wrffire_t
    use fuel_mod, only : fuel_t, Crosswalk_from_scottburgan_to_anderson
    use geogrid_mod, only : geogrid_t
    use ignition_line_mod, only : ignition_line_t
    use namelist_mod, only : namelist_t
    use netcdf_mod, only : Create_netcdf_file, Add_netcdf_att, Add_netcdf_dim, Add_netcdf_var_mpi, Get_netcdf_att, &
        Get_netcdf_var_mpi, Is_netcdf_file_present, NAME_DIM_X, NAME_DIM_Y
    use proj_lc_mod, only : proj_lc_t
    use ros_mod, only : ros_t
    use stderrout_mod, only : Stop_simulation, Print_message
    use tiles_mod, only : Calc_tiles_dims
    use wrfdata_mod, only : wrfdata_t, G, RERADIUS
    use mpi_mod, only : Calc_tasks_in_x_and_y, Calc_patch_dims, Distribute_var2d, Do_halo_exchange_with_corners, &
        Print_cart_info, Sum_across_mpi_tasks, topology_dim_order
    use, intrinsic :: iso_fortran_env, only : INT32, REAL32

    implicit none

    private

    public :: state_fire_t, N_POINTS_IN_HALO, Build_restart_file_name

    integer, parameter :: N_POINTS_IN_HALO = 5, N_DIMS = 2
    integer, parameter :: RESTART_IO_ROOT = 0
    character (len = *), parameter :: NAME_DIM_MOISTURE_CLASS = 'moisture_class'
    character (len = *), parameter :: NAME_VAR_FMC_GC = 'fmc_gc'
    character (len = *), parameter :: NAME_ATT_FMOIST_LASTTIME = 'fmc_fmoist_lasttime'
    character (len = *), parameter :: NAME_ATT_FMOIST_NEXTTIME = 'fmc_fmoist_nexttime'
    logical, dimension(2), parameter :: PERIODS = [ .false., .false. ]
    logical, parameter :: REORDER = .true. ! Allow MPI recording tasks for performance

    type :: state_fire_t
      integer :: ifds, ifde, jfds, jfde, kfds, kfde, ifms, ifme, jfms, jfme, kfms, kfme, &
                 ifts, ifte, jfts, jfte, kfts, kfte, ifps, ifpe, jfps, jfpe, kfps, kfpe
      real :: dx = 200.0 , dy = 200.0
      real :: dt = 2.0              ! "TEMPORAL RESOLUTION"      "SECONDS"
      integer :: itimestep = 0
      integer :: num_tiles = 1
      type (datetime_t) :: datetime_start, datetime_end, datetime_now, datetime_next_output, datetime_next_atm_update

      real, dimension(:, :), allocatable :: uf ! W-E winds used in fire module
      real, dimension(:, :), allocatable :: vf ! S-N winds used in fire module
      real, dimension(:, :), allocatable :: zsf    ! terrain height
      real, dimension(:, :), allocatable :: dzdxf  ! terrain grad
      real, dimension(:, :), allocatable :: dzdyf  ! terrain grad
      real, dimension(:, :), allocatable :: fmc_g  ! fuel moisture, ground
      real, dimension(:, :), allocatable :: lfn    ! "level function" "1"
      real, dimension(:, :), allocatable :: lfn_hist ! "level function history" "1"
      real, dimension(:, :), allocatable :: lfn_0 ! "level function for time integration, step 0" "1"
      real, dimension(:, :), allocatable :: lfn_1 ! "level function for time integration, step 1" "1"
      real, dimension(:, :), allocatable :: lfn_2 ! "level function for time integration, step 2" "1"
      real, dimension(:, :), allocatable :: lfn_s0 ! "level set sign function from LSM integration" "1"
      real, dimension(:, :), allocatable :: lfn_s1 ! "level set function for reinitialization integration" "1"
      real, dimension(:, :), allocatable :: lfn_s2 ! "level set function for reinitialization integration" "1"
      real, dimension(:, :), allocatable :: lfn_s3 ! "level set function for reinitialization integration" "1"
      real, dimension(:, :), allocatable :: lfn_out
      real, dimension(:, :), allocatable :: fuel_load_g ! [kg m-2]
      real, dimension(:, :), allocatable :: flame_length ! "fire flame length" "m"
      real, dimension(:, :), allocatable :: ros_front ! "rate of spread at fire front" "m/s"
      real, dimension(:, :), allocatable :: tign_g ! "ignition time on ground" "s"
      real, dimension(:, :), allocatable :: fuel_frac ! "fuel remaining" "1"
      real, dimension(:, :), allocatable :: fire_area ! "fraction of cell area on fire" "1"
      real, dimension(:, :), allocatable :: fuel_frac_burnt_dt ! "fraction of fuel burnt on current dt" "-"
      real, dimension(:, :), allocatable :: fgrnhfx ! "heat flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: fgrnqfx ! "moisture flux from ground fire" "W/m^2"
      real, dimension(:, :), allocatable :: fcanhfx ! "heat flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: fcanqfx ! "moisture flux from crown fire" "W/m^2"
      real, dimension(:, :), allocatable :: ros ! "rate of spread" "m/s"
      real, dimension(:, :), allocatable :: lats   ! "latitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lons   ! "longitude of midpoints of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lats_c ! "latitude of corners of fire cells" "degrees"
      real, dimension(:, :), allocatable :: lons_c ! "longitude of corners of fire cells" "degrees"
      real, dimension(:, :), allocatable :: fz0 ! "roughness length of fire cells" "m"
        ! nfuel_cat is external classification metadata from ideal input,
        ! geogrid, WRF coupling, or restart files. fuel_index is the validated
        ! internal physics-table row. They must not be conflated because
        ! WRF-Fire-style mutation of category fields makes outputs and restarts
        ! scientifically ambiguous.
      real, dimension(:, :), allocatable :: nfuel_cat ! "fuel data"
      integer, dimension(:, :), allocatable :: fuel_index ! resolved internal fuel row
      real, dimension(:, :), allocatable :: fuel_time ! "fuel"
      real, dimension(:, :), allocatable :: emis_smoke
      real, dimension(:, :), allocatable :: grad_norm_ls ! Gracient norm of the level set function used to propagate level set function
      real, dimension(:, :), allocatable :: grad_norm_reinit ! Gracient norm of the level set function used to reinitilize the level set function

      class (fuel_t), allocatable :: fuels
      class (ros_t), allocatable :: ros_param
      type (ignition_line_t) :: ignition_lines
      class (fmc_t), allocatable :: fmc_param
      type (proj_lc_t) :: proj

        ! New vars defined on fire grid for NUOPC coupling
      real, dimension(:, :), allocatable :: fire_psfc       ! "Surface Pressure"  "Pa"
      real, dimension(:, :), allocatable :: fire_rain       ! "Accumulated total rain"  "mm"
      real, dimension(:, :), allocatable :: fire_t2         ! "TEMP at 2 M"       "K"
      real, dimension(:, :), allocatable :: fire_q2         ! "Value of 2m specific humidity" "kg/kg"
      real, dimension(:, :), allocatable :: fire_rh_fire    ! "relative humidity, diagnostics" ""
      real, dimension(:, :), allocatable :: fire_psfc_old   ! "Surface Pressure, previous value"  "Pa"
      real, dimension(:, :), allocatable :: fire_rain_old   ! "Accumulated total rain, previous value"  "mm"
      real, dimension(:, :), allocatable :: fire_t2_old     ! "TEMP at 2 M, previous value"       "K"
      real, dimension(:, :), allocatable :: fire_q2_old     ! "Value of 2m specific humidity, previous value" "kg/kg"

      integer, dimension(:), allocatable :: i_start, i_end, j_start, j_end

      real :: unit_fxlong, unit_fxlat
      real :: fuelmc_g_lh
      integer :: nx ! "number of longitudinal grid points" "1"
      integer :: ny ! "number of latitudinal grid points" "1"
      real :: cen_lat, cen_lon

        ! Performance stats
      real :: grad_norm_residual_sq_sum
      real :: grad_norm_residual_sq_sum_band
      real :: grad_norm_residual_rms_band

        ! Output
      integer :: output_level

        ! For MPI tasks
      integer :: cfbm_comm ! The MPI communicator before the domain decomposition
      logical :: is_cfbm_comm_set = .false.
      integer :: cart_comm ! The MPI communicator with the domain decomposition
      integer :: ntasks ! Number of MPI tasks
      integer :: px, py ! Number of MPI tasks in X and Y, respectively
    contains
      procedure, public :: Allocate_vars => Allocate_vars
      procedure, public :: Apply_wafs => Apply_wafs
      procedure, public :: Convert_sb_to_ander => Convert_scottburgan_to_anderson
      procedure, public :: Handle_output => Handle_output
      procedure, public :: Handle_wrfdata_update => Handle_wrfdata_update
      procedure, public :: Init_fuel_vars => Init_fuel_vars
      procedure, public :: Initialization => Init_domain
      procedure, public :: Init_ignition_lines => Init_ignition_lines
      procedure :: Init_latlons => Init_latlons
      procedure :: Init_tiles => Init_tiles
      procedure :: Init_tiles_in_wrf => Init_tiles_in_wrf
      procedure :: Interpolate_vars_atm_to_fire => Interpolate_vars_atm_to_fire
      procedure, public :: Print => Print_domain ! private
      procedure, public :: Print_tiles => Print_tiles
      procedure, public :: Read_restart => Read_restart
      procedure, public :: Resolve_fuel_indices => Resolve_fuel_indices
      procedure, public :: Save_state => Save_state
      procedure, public :: Set_vars_to_default => Set_vars_to_default
      procedure, public :: Set_mpi_comm_cfbm => Set_mpi_comm_cfbm
      procedure, public :: Set_time_stamps => Set_time_stamps
      procedure, public :: Write_restart => Write_restart
    end type state_fire_t

  contains

    subroutine Allocate_vars (this, ifms, ifme, jfms, jfme)

      implicit none

      class (state_fire_t), intent(in out) :: this
      integer, intent (in) :: ifms, ifme, jfms, jfme


      allocate (this%uf(ifms:ifme, jfms:jfme))
      allocate (this%vf(ifms:ifme, jfms:jfme))
      allocate (this%fmc_g(ifms:ifme, jfms:jfme))
      allocate (this%lfn(ifms:ifme, jfms:jfme))
      allocate (this%lfn_hist(ifms:ifme, jfms:jfme))
      allocate (this%lfn_0(ifms:ifme, jfms:jfme))
      allocate (this%lfn_1(ifms:ifme, jfms:jfme))
      allocate (this%lfn_2(ifms:ifme, jfms:jfme))
      allocate (this%lfn_s0(ifms:ifme, jfms:jfme))
      allocate (this%lfn_s1(ifms:ifme, jfms:jfme))
      allocate (this%lfn_s2(ifms:ifme, jfms:jfme))
      allocate (this%lfn_s3(ifms:ifme, jfms:jfme))
      allocate (this%lfn_out(ifms:ifme, jfms:jfme))
      allocate (this%fuel_load_g(ifms:ifme, jfms:jfme))
      allocate (this%flame_length(ifms:ifme, jfms:jfme))
      allocate (this%ros_front(ifms:ifme, jfms:jfme))
      allocate (this%tign_g(ifms:ifme, jfms:jfme))
      allocate (this%fuel_frac(ifms:ifme, jfms:jfme))
      allocate (this%fire_area(ifms:ifme, jfms:jfme))
      allocate (this%fuel_frac_burnt_dt(ifms:ifme, jfms:jfme))
      allocate (this%fgrnhfx(ifms:ifme, jfms:jfme))
      allocate (this%fgrnqfx(ifms:ifme, jfms:jfme))
      allocate (this%fcanhfx(ifms:ifme, jfms:jfme))
      allocate (this%fcanqfx(ifms:ifme, jfms:jfme))
      allocate (this%ros(ifms:ifme, jfms:jfme))
      allocate (this%fz0(ifms:ifme, jfms:jfme))
      allocate (this%fuel_time(ifms:ifme, jfms:jfme))
      allocate (this%fire_psfc(ifms:ifme, jfms:jfme))
      allocate (this%fire_rain(ifms:ifme, jfms:jfme))
      allocate (this%fire_t2(ifms:ifme, jfms:jfme))
      allocate (this%fire_q2(ifms:ifme, jfms:jfme))
      allocate (this%fire_rh_fire(ifms:ifme, jfms:jfme))
      allocate (this%fire_psfc_old(ifms:ifme, jfms:jfme))
      allocate (this%fire_rain_old(ifms:ifme, jfms:jfme))
      allocate (this%fire_t2_old(ifms:ifme, jfms:jfme))
      allocate (this%fire_q2_old(ifms:ifme, jfms:jfme))
      allocate (this%zsf(ifms:ifme, jfms:jfme))
      allocate (this%dzdxf(ifms:ifme, jfms:jfme))
      allocate (this%dzdyf(ifms:ifme, jfms:jfme))
      allocate (this%nfuel_cat(ifms:ifme, jfms:jfme))
      allocate (this%emis_smoke(ifms:ifme, jfms:jfme))
      allocate (this%grad_norm_ls(ifms:ifme, jfms:jfme))
      allocate (this%grad_norm_reinit(ifms:ifme, jfms:jfme))
      allocate (this%fuel_index(ifms:ifme, jfms:jfme))

    end subroutine Allocate_vars

    subroutine Apply_wafs (this)

      implicit none

      class (state_fire_t), intent(in out) :: this

      integer :: i, j, ij, ifts, ifte, jfts, jfte
      real :: waf


      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, i, j, ifts, ifte, jfts, jfte, waf)
      do ij = 1, this%num_tiles
        ifts = this%i_start(ij)
        ifte = this%i_end(ij)
        jfts = this%j_start(ij)
        jfte = this%j_end(ij)

        do j = jfts, jfte
          do i = ifts, ifte
            waf = this%fuels%waf(this%fuel_index(i, j))
            this%uf(i, j) = waf * this%uf(i, j)
            this%vf(i, j) = waf * this%vf(i, j)
          end do
        end do
      end do
      !$OMP END PARALLEL DO

    end subroutine Apply_wafs

    subroutine Convert_scottburgan_to_anderson (this)

      implicit none

      class (state_fire_t), intent(in out) :: this

      integer :: i, j, ij, ifts, ifte, jfts, jfte


      ! Explicit legacy compatibility helper only. Normal CFBM initialization
      ! preserves nfuel_cat as external metadata and resolves fuel_index instead
      ! of mutating category fields.
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, i, j, ifts, ifte, jfts, jfte)
      do ij = 1, this%num_tiles
        ifts = this%i_start(ij)
        ifte = this%i_end(ij)
        jfts = this%j_start(ij)
        jfte = this%j_end(ij)

        do j = jfts, jfte
          do i = ifts, ifte
            this%nfuel_cat(i, j) = real (Crosswalk_from_scottburgan_to_anderson (int (this%nfuel_cat(i, j))))
          end do
        end do
      end do
      !$OMP END PARALLEL DO

    end subroutine Convert_scottburgan_to_anderson

    subroutine Resolve_fuel_indices (this)

      implicit none

      class (state_fire_t), intent(in out) :: this

      integer, parameter :: MAX_SAMPLES = 8
      integer :: i, j, idx, local_unknown_count, sample_count
      integer, dimension(MAX_SAMPLES) :: sample_i, sample_j, sample_code
      real :: local_unknown_real, global_unknown_real
      character (len = 512) :: msg


      if (.not. allocated (this%fuels)) call Stop_simulation ('Resolve_fuel_indices requires initialized fuels')
      if (.not. allocated (this%fuel_index)) call Stop_simulation ('Resolve_fuel_indices requires allocated fuel_index')

      local_unknown_count = 0
      sample_count = 0

      ! Resolve and validate before any table access. This avoids the legacy
      ! WRF-Fire pattern of indexing lookup tables with raw categories before
      ! bounds checking. Unknown future, urban, or custom classifications must
      ! abort because they require explicit scientific mapping; silent no-fuel
      ! fallback would hide land-cover errors.
      do j = this%jfps, this%jfpe
        do i = this%ifps, this%ifpe
          idx = this%fuels%Resolve_fuel_index (int (this%nfuel_cat(i, j)))
          this%fuel_index(i, j) = idx
          if (idx <= 0) then
            local_unknown_count = local_unknown_count + 1
            if (sample_count < MAX_SAMPLES) then
              sample_count = sample_count + 1
              sample_i(sample_count) = i
              sample_j(sample_count) = j
              sample_code(sample_count) = int (this%nfuel_cat(i, j))
            end if
          end if
        end do
      end do

      local_unknown_real = real (local_unknown_count)
      call Sum_across_mpi_tasks (local_unknown_real, this%cart_comm, global_unknown_real)

      if (global_unknown_real > 0.0) then
        write (msg, '(a, i0, a, i0)') 'Unknown fuel categories: local_count=', local_unknown_count, &
            ' global_count=', nint (global_unknown_real)
        call Print_message (trim (msg))
        do i = 1, sample_count
          write (msg, '(a, i0, a, i0, a, i0)') '  sample fuel code ', sample_code(i), ' at i=', sample_i(i), ' j=', sample_j(i)
          call Print_message (trim (msg))
        end do
        call Stop_simulation ('Unknown nfuel_cat values require explicit calibrated fuel or nonburnable mapping')
      end if

    end subroutine Resolve_fuel_indices

    subroutine Handle_output (this, config_flags)

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags

      logical, parameter :: DEBUG_LOCAL = .false.


      if (this%datetime_now == this%datetime_next_output) then
        if (DEBUG_LOCAL) call Print_message ('Writing output...')
        call this%Save_state ()
        call this%datetime_next_output%Add_seconds (config_flags%interval_output)
      end if

    end subroutine Handle_output

    subroutine Handle_wrfdata_update (this, wrf, config_flags)

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      type (wrfdata_t), intent (in out) :: wrf

      logical, parameter :: DEBUG_LOCAL = .false.


      if (DEBUG_LOCAL) call Print_message ('Entering Handle_wrfdata_update...')

      If_update_atm: if (this%datetime_now == this%datetime_next_atm_update) then
        if (DEBUG_LOCAL) call Print_message ('  Updating WRF atm state...')
        if (DEBUG_LOCAL) call this%datetime_now%Print_datetime ()

        call wrf%Update_atm_state (this%datetime_now, config_flags)

        if (DEBUG_LOCAL) call Print_message ('  Interpolating WRF vars...')
        call this%Interpolate_vars_atm_to_fire(wrf, config_flags)

        call this%datetime_next_atm_update%Add_seconds (config_flags%interval_atm)

      end if If_update_atm

      if (DEBUG_LOCAL) call Print_message ('Leaving Handle_wrfdata_update...')

    end subroutine Handle_wrfdata_update

    subroutine Init_domain (this, config_flags, geogrid, &
                            ifds, ifde, ifms, ifme, ifps, ifpe, &
                            jfds, jfde, jfms, jfme, jfps, jfpe, &
                            kfds, kfde, kfms, kfme, kfps, kfpe, &
                            kfts, kfte, ide, jde, &
                            cen_lat, cen_lon, truelat1, truelat2, stand_lon, &
                            dx, dy, sr_x, sr_y, nfuel_cat, zsf, dzdxf, dzdyf, restart_file)


      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      type (geogrid_t), intent (in out), optional :: geogrid
      character (len = *), intent (in), optional :: restart_file
      integer, intent (in), optional :: ifds, ifde, ifms, ifme, ifps, ifpe, &
                                        jfds, jfde, jfms, jfme, jfps, jfpe, &
                                        kfds, kfde, kfms, kfme, kfps, kfpe, &
                                        kfts, kfte, sr_x, sr_y, ide, jde
      real, intent (in), optional :: cen_lat, cen_lon, truelat1, truelat2, stand_lon, dx, dy
      real, dimension(:, :), intent (in), optional :: nfuel_cat, zsf, dzdxf, dzdyf

      integer, parameter :: INIT_MODE_NONE = 0, INIT_MODE_GEOGRID = 1, INIT_MODE_WRF = 2, INIT_MODE_IDEAL = 3, INIT_MODE_RESTART = 4
      type (proj_lc_t) :: proj
      logical, parameter :: DEBUG_LOCAL = .false.
      integer :: ids0, ide0, jds0, jde0, i, j, init_mode, px, py, ntasks, ierr, cart_comm, rank, ips, ipe, jps, jpe, &
          is_lfn_init_allocated, restart_nx, restart_ny
      integer :: restart_map_proj, restart_sr_x, restart_sr_y
      integer (kind = INT32) :: att_int32
      real (kind = REAL32) :: att_real32
      real :: restart_dx, restart_dy, restart_cen_lat, restart_cen_lon, restart_stand_lon, restart_true_lat_1, restart_true_lat_2
      integer, dimension(2) :: coords
      character (len = 300) :: msg


      if (DEBUG_LOCAL) call Print_message ('Entering Init_domain...')

      init_mode = INIT_MODE_NONE
      if (config_flags%ideal_opt == 1) init_mode = INIT_MODE_IDEAL
      if (present (geogrid)) init_mode = INIT_MODE_GEOGRID
      if (present (restart_file)) init_mode = INIT_MODE_RESTART
      if (present (ifds) .and. present (ifde) .and. present (ifms) .and. present (ifme) .and. present (ifps) .and. present (ifpe) .and. &
          present (jfds) .and. present (jfde) .and. present (jfms) .and. present (jfme) .and. present (jfps) .and. present (jfpe) .and. &
          present (kfds) .and. present (kfde) .and. present (kfms) .and. present (kfme) .and. present (kfps) .and. present (kfpe) .and. &
          present (kfts) .and. present (kfte) .and. present (ide) .and. present (jde) .and. &
          present (cen_lat) .and. present (cen_lon) .and. present (truelat1) .and. present (truelat2) .and. present (stand_lon) .and. &
          present (dx) .and. present (dy) .and. present (sr_x) .and. present (sr_y) .and. present (nfuel_cat) .and. present (zsf) .and. &
          present (dzdxf) .and. present (dzdyf)) &
          init_mode = INIT_MODE_WRF

      if (init_mode == INIT_MODE_NONE) &
          call Stop_simulation ('Not enough information to initialize domain')

        ! Set dimensions
      if (DEBUG_LOCAL) call Print_message ('  Setting dimensions...')
      Set_dims: select case (init_mode)
        case (INIT_MODE_GEOGRID, INIT_MODE_IDEAL, INIT_MODE_RESTART)

          if (init_mode == INIT_MODE_GEOGRID) then

            ids0 = geogrid%ifds
            ide0 = geogrid%ifde
            jds0 = geogrid%jfds
            jde0 = geogrid%jfde

#ifdef DM_PARALLEL

            if (.not. this%is_cfbm_comm_set) call Stop_simulation ('The MPI CFBM communicator has not been set')

            call Mpi_comm_size (this%cfbm_comm, ntasks, ierr)
            if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems getting the number of MPI tasks')
            this%ntasks = ntasks

            call Calc_tasks_in_x_and_y (this%ntasks, ide0, jde0, px, py)
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

            call Calc_patch_dims (ide0, jde0, this%px, this%py, coords, ips, ipe, jps, jpe)

              ! Distribute global vars of geogrid from Rank0 to the other tasks
            call Distribute_var2d (geogrid%elevations, ips, ipe, jps, jpe, this%cart_comm)
            call Distribute_var2d (geogrid%dz_dxs, ips, ipe, jps, jpe, this%cart_comm)
            call Distribute_var2d (geogrid%dz_dys, ips, ipe, jps, jpe, this%cart_comm)
            call Distribute_var2d (geogrid%fuel_cats, ips, ipe, jps, jpe, this%cart_comm)

              ! Distribute lfn_init if available
           if (rank == 0 .and. allocated (geogrid%lfn_init)) then
             is_lfn_init_allocated = 1
           else
             is_lfn_init_allocated = 0
           end if
           call MPI_Bcast(is_lfn_init_allocated, 1, MPI_INTEGER, 0, this%cart_comm, ierr)

           if (is_lfn_init_allocated == 1) call Distribute_var2d (geogrid%lfn_init, ips, ipe, jps, jpe, this%cart_comm)

           ! Other atm vars in geogrid derived type that may not be needed: xlat, xlong, xlat_c, xlong_c
#else
            ips = ids0
            ipe = ide0
            jps = jds0
            jpe = jde0
#endif

          else if (init_mode == INIT_MODE_IDEAL) then

            ids0 = 1
            ide0 = config_flags%nx
            jds0 = 1
            jde0 = config_flags%ny

#ifdef DM_PARALLEL
            call Mpi_comm_size (this%cfbm_comm, ntasks, ierr)
            if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems getting the number of MPI tasks')
            this%ntasks = ntasks

            call Calc_tasks_in_x_and_y (this%ntasks, config_flags%nx, config_flags%ny, px, py)
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

            call Calc_patch_dims (config_flags%nx, config_flags%ny, this%px, this%py, coords, ips, ipe, jps, jpe)

#else
            ips = ids0
            ipe = ide0
            jps = jds0
            jpe = jde0
#endif
          else if (init_mode == INIT_MODE_RESTART) then

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
          end if 

          this%ifds = ids0
          this%ifde = ide0
          this%ifms = ips - N_POINTS_IN_HALO
          this%ifme = ipe + N_POINTS_IN_HALO
          this%ifps = ips
          this%ifpe = ipe

          this%jfds = jds0
          this%jfde = jde0
          this%jfms = jps - N_POINTS_IN_HALO
          this%jfme = jpe + N_POINTS_IN_HALO
          this%jfps = jps
          this%jfpe = jpe

          this%kfds = 1
          this%kfde = config_flags%kde
          this%kfms = 1
          this%kfme = config_flags%kde
          this%kfps = 1
          this%kfpe = config_flags%kde
          this%kfts = 1
          this%kfte = config_flags%kde

          call this%Init_tiles (config_flags)

        case (INIT_MODE_WRF)
          this%ifds = ifds
          this%ifde = ifde
          this%ifms = ifms
          this%ifme = ifme
          this%ifps = ifps
          this%ifpe = ifpe

          this%jfds = jfds
          this%jfde = jfde
          this%jfms = jfms
          this%jfme = jfme
          this%jfps = jfps
          this%jfpe = jfpe

          this%kfds = kfds
          this%kfde = kfde
          this%kfms = kfms
          this%kfme = kfme
          this%kfps = kfps
          this%kfpe = kfpe
          this%kfts = kfts
          this%kfte = kfte

          call this%Init_tiles_in_wrf (config_flags, sr_x, sr_y)

#ifdef DM_PARALLEL
!          call Mpi_comm_size (this%cfbm_comm, ntasks, ierr)
!          if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems getting the number of MPI tasks in WRF')
!          this%ntasks = ntasks

          this%cart_comm = this%cfbm_comm
            ! Chaning the way halo exchange is done
          topology_dim_order = 1

            ! Debug the topology here
          if (DEBUG_LOCAL) call Print_cart_info (this%cart_comm)
#endif

        case default

          call Stop_simulation ('Not ready to complete fire state initialization 1')

      end select Set_dims

      write (msg, '(a11, 4(a4, i7))') &
          'CFBM Patch:', ' IPS', this%ifps, ' IPE', this%ifpe, ' JPS', this%jfps, ' JPE', this%jfpe
      call Print_message (msg)

      call this%Print_tiles ()

      this%nx = this%ifde
      this%ny = this%jfde
      this%dt = config_flags%dt
      this%fuelmc_g_lh = config_flags%fuelmc_g_lh

        ! Init memory
      if (DEBUG_LOCAL) call Print_message ('  Allocating memory...')
      call this%Allocate_vars (this%ifms, this%ifme, this%jfms, this%jfme)

        ! Set projection
      if (DEBUG_LOCAL) call Print_message ('  Setting projection...')
      Set_proj: select case (init_mode)
        case (INIT_MODE_GEOGRID)
          proj = geogrid%Get_atm_proj ()
          call this%Init_latlons (proj, srx = geogrid%sr_x, sry = geogrid%sr_y)

          this%cen_lat = geogrid%cen_lat
          this%cen_lon = geogrid%cen_lon

          this%dx = geogrid%dx / geogrid%sr_x
          this%dy = geogrid%dy / geogrid%sr_y

        case (INIT_MODE_WRF)
          proj = proj_lc_t (cen_lat = cen_lat , cen_lon = cen_lon, dx = dx, dy = dy, &
            standard_lon = stand_lon, true_lat_1 = truelat1, true_lat_2 = truelat2, nx = ide - 1, ny = jde - 1)
          call this%Init_latlons (proj, srx = sr_x, sry = sr_y)

          this%cen_lat = cen_lat
          this%cen_lon = cen_lon

          this%dx = dx / sr_x
          this%dy = dy / sr_y

        case (INIT_MODE_IDEAL)
          this%dx = config_flags%dx
          this%dy = config_flags%dy

          this%cen_lat = config_flags%cen_lat
          this%cen_lon = config_flags%cen_lon

          proj = proj_lc_t (cen_lat = this%cen_lat , cen_lon = this%cen_lon, dx = this%dx, dy = this%dy, &
              standard_lon = config_flags%stand_lon, true_lat_1 = config_flags%true_lat_1, &
              true_lat_2 = config_flags%true_lat_2, nx = config_flags%nx, ny = config_flags%ny)

          call this%Init_latlons (proj)

        case (INIT_MODE_RESTART)
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

        case default
          call Stop_simulation ('Not ready to complete fire state initialization 2')

      end select Set_proj
      this%proj = proj

        ! Init vars
      if (DEBUG_LOCAL) call Print_message ('  Initializing default variables...')
      call this%Set_vars_to_default (config_flags)

      if (DEBUG_LOCAL) call Print_message ('  Setting topo and fuels...')
      Set_topo_fuels: select case (init_mode)
        case (INIT_MODE_GEOGRID)
          this%zsf(this%ifps:this%ifpe, this%jfps:this%jfpe) = geogrid%elevations
          this%dzdxf(this%ifps:this%ifpe, this%jfps:this%jfpe) = geogrid%dz_dxs
          this%dzdyf(this%ifps:this%ifpe, this%jfps:this%jfpe) = geogrid%dz_dys
          this%nfuel_cat(this%ifps:this%ifpe, this%jfps:this%jfpe) = geogrid%fuel_cats

          if (config_flags%fire_is_real_perim) then
            if (allocated (geogrid%lfn_init)) then
              this%lfn_hist(this%ifps:this%ifpe, this%jfps:this%jfpe) = geogrid%lfn_init
            else
              Call Stop_simulation ('Attenting to initialize fire from given  perimeter but no initialization data present')
            end if
          end if

        case (INIT_MODE_WRF)
          this%zsf(this%ifms:this%ifme, this%jfms:this%jfme) = zsf
          this%dzdxf(this%ifms:this%ifme, this%jfms:this%jfme) = dzdxf
          this%dzdyf(this%ifms:this%ifme, this%jfms:this%jfme) = dzdyf
          this%nfuel_cat(this%ifms:this%ifme, this%jfms:this%jfme) = nfuel_cat
          if (config_flags%fire_is_real_perim) &
              !this%lfn_hist(this%ifms:this%ifme, this%jfms:this%jfme) = lfn_hist
              call Stop_simulation ('Not ready to initialize from fire perimeter inside WRF')

        case (INIT_MODE_IDEAL)
          do j = this%jfps, this%jfpe
            do i = this%ifps, this%ifpe
              this%zsf(i, j) = config_flags%elevation + &
                               (i - this%ifds) * config_flags%dz_dx * config_flags%dx + &
                               (j - this%jfds) * config_flags%dz_dy * config_flags%dy
            end do
          end do
          this%dzdxf(this%ifps:this%ifpe, this%jfps:this%jfpe) = config_flags%dz_dx
          this%dzdyf(this%ifps:this%ifpe, this%jfps:this%jfpe) = config_flags%dz_dy
          this%nfuel_cat(this%ifps:this%ifpe, this%jfps:this%jfpe) = config_flags%fuel_cat

          if (config_flags%fire_is_real_perim) &
              call Stop_simulation ('Not ready to initialize from fire perimeter in idealized mode')

        case (INIT_MODE_RESTART)
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

        case default
          call Stop_simulation ('Not ready to complete fire state initialization 3')

      end select Set_topo_fuels

        ! Set clock
      if (DEBUG_LOCAL) call Print_message ('  Setting clock...')
      call this%Set_time_stamps (config_flags)

        ! Output
      this%output_level = config_flags%output_level

      if (DEBUG_LOCAL) call this%Print()

      if (DEBUG_LOCAL) call Print_message ('Leaving Init_domain...')

    end subroutine Init_domain

    subroutine Init_fuel_vars (this)

      implicit none

      class (state_fire_t), intent(in out) :: this

      integer :: ij, i, j, ifts, ifte, jfts, jfte, k


      !$OMP PARALLEL DO   &
      !$OMP PRIVATE (ij, i, j, k, ifts, ifte, jfts, jfte)
      do ij = 1, this%num_tiles
        ifts = this%i_start(ij)
        ifte = this%i_end(ij)
        jfts = this%j_start(ij)
        jfte = this%j_end(ij)
        do j = jfts, jfte
          do i = ifts, ifte
            k = this%fuel_index(i, j)
            if (this%fuels%fgi_lh(k) == 0.0) then
              this%fuel_load_g(i, j) = this%fuels%fgi(k)
            else
              this%fuel_load_g(i, j) = this%fuels%Effective_dead_load(k, this%fuelmc_g_lh)
            end if
            if (k == this%fuels%no_fuel_cat) then
                ! Just what was there before
              this%fuel_time(i, j) = 7.0 / 0.85
            else
                ! set fuel time constant (the e-folding time)
                ! burn time from fuels: weight=1000 => 40% decrease over 10 min
                ! fuel decreases as exp(-t/fuel_time)
                ! exp(-600*0.85/1000) = approx 0.6
              this%fuel_time(i, j) = this%fuels%weight(k) / 0.85
            end if
          end do
        end do
      end do
      !$OMP END PARALLEL DO

    end subroutine Init_fuel_vars

    subroutine Init_ignition_lines (this, config_flags)

      implicit none

      class (state_fire_t), intent (in out) :: this
      type (namelist_t), intent (in) :: config_flags


      call this%ignition_lines%Init (config_flags)

    end subroutine Init_ignition_lines

    subroutine Init_latlons (this, proj, srx, sry)

      implicit none

      class (state_fire_t), intent (in out) :: this
      type (proj_lc_t), intent(in) :: proj
      integer, optional :: srx, sry

      real, parameter :: OFFSET = 0.5
      integer :: i, j, sr_x, sr_y, iend, jend
      real :: i_atm, j_atm, offset_corners_x, offset_corners_y


      if (present (srx) .and. present (sry)) then
        sr_x = srx
        sr_y = sry
      else
        sr_x = 1
        sr_y = 1
      end if

      allocate (this%lons(this%ifms:this%ifme, this%jfms:this%jfme))
      allocate (this%lats(this%ifms:this%ifme, this%jfms:this%jfme))

      iend = this%ifpe + 1
      jend = this%jfpe + 1

      allocate (this%lons_c(this%ifps:iend, this%jfps:jend))
      allocate (this%lats_c(this%ifps:iend, this%jfps:jend))

      offset_corners_x = (1.0 / real (sr_x)) / 2.0
      offset_corners_y = (1.0 / real (sr_y)) / 2.0

      do j = this%jfps, this%jfpe
        do i = this%ifps, this%ifpe
          i_atm = (i - OFFSET) / sr_x + OFFSET
          j_atm = (j - OFFSET) / sr_y + OFFSET
          call proj%Calc_latlon (i = i_atm, j = j_atm, lat = this%lats(i, j), lon = this%lons(i, j))
          call proj%Calc_latlon (i = i_atm - offset_corners_x, j = j_atm - offset_corners_y, &
              lat = this%lats_c(i, j), lon = this%lons_c(i, j))
        end do
      end do

        ! Right hand side of the patch
      do j = this%jfps, this%jfpe
        i_atm = (this%ifde - OFFSET) / sr_x + OFFSET
        j_atm = (j - OFFSET) / sr_y + OFFSET
        call proj%Calc_latlon (i = i_atm + offset_corners_x, j = j_atm - offset_corners_y, &
            lat = this%lats_c(iend, j), lon = this%lons_c(iend, j))
      end do

        ! Top of the patch
      do i = this%ifps, this%ifpe
        i_atm = (i - OFFSET) / sr_x + OFFSET
         j_atm = (this%jfde - OFFSET) / sr_y + OFFSET
        call proj%Calc_latlon (i = i_atm - offset_corners_x, j = j_atm + offset_corners_y, &
            lat = this%lats_c(i, jend), lon = this%lons_c(i, jend))
      end do

        ! top right corner
      i_atm = (this%ifpe - OFFSET) / sr_x + OFFSET
      j_atm = (this%jfpe - OFFSET) / sr_y + OFFSET
      call proj%Calc_latlon (i = i_atm + offset_corners_x, j = j_atm + offset_corners_y, &
          lat = this%lats_c(iend, jend), lon = this%lons_c(iend, jend))

    end subroutine Init_latlons

    subroutine Init_tiles (this, config_flags)

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags


      this%num_tiles = config_flags%num_tiles
      call Calc_tiles_dims (this%ifps, this%ifpe, this%jfps, this%jfpe, this%num_tiles, config_flags%tile_strategy, &
          this%i_start, this%i_end, this%j_start, this%j_end)

      if (this%num_tiles /= config_flags%num_tiles) then
        call Stop_simulation ('Not able to use the number of tiles specified')
      end if

    end subroutine Init_tiles

    subroutine Init_tiles_in_wrf (this, config_flags, sr_x, sr_y)

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (namelist_t), intent (in) :: config_flags
      integer, intent (in) :: sr_x, sr_y

      integer :: ips, ipe, jps, jpe, ij


      this%num_tiles = config_flags%num_tiles
      ips = (this%ifps - 1) / sr_x + 1
      ipe = this%ifpe / sr_x
      jps = (this%jfps - 1) / sr_y + 1
      jpe = this%jfpe / sr_y
      call Calc_tiles_dims (ips, ipe, jps, jpe, this%num_tiles, config_flags%tile_strategy, &
          this%i_start, this%i_end, this%j_start, this%j_end)

      if (this%num_tiles /= config_flags%num_tiles) then
        call Stop_simulation ('Not able to use the number of tiles specified')
      end if

      do ij = 1, this%num_tiles
        this%i_start(ij) = this%i_start(ij) * sr_x - sr_x + 1
        this%i_end(ij) = this%i_end(ij) * sr_x
        this%j_start(ij) = this%j_start(ij) * sr_y - sr_y + 1
        this%j_end(ij) = this%j_end(ij) * sr_y
      end do

    end subroutine Init_tiles_in_wrf

    subroutine Interpolate_vars_atm_to_fire (this, wrf, config_flags)

      implicit none

      class (state_fire_t), intent(in out) :: this
      type (wrfdata_t), intent(in out) :: wrf
      type (namelist_t), intent (in) :: config_flags

      integer :: i, j


      if (.not. allocated (this%lats) .or. .not. allocated (this%lons)) &
          call Stop_simulation ('Init lats/lons before calling hinterp atm variables')

      call wrf%Interp_var2grid (this%lats, this%lons, this%ifms, this%ifme, this%jfms, this%jfme, &
          config_flags%num_tiles, this%i_start, this%i_end, this%j_start, this%j_end, &
          'ua', config_flags%hinterp_opt, this%uf)

      call wrf%Interp_var2grid (this%lats, this%lons, this%ifms, this%ifme, this%jfms, this%jfme, &
          config_flags%num_tiles, this%i_start, this%i_end, this%j_start, this%j_end, &
          'va', config_flags%hinterp_opt, this%vf)

      if (config_flags%wind_vinterp_opt == 1) then
        call this%Apply_wafs ()
        call wrf%Destroy_u10 ()
        call wrf%Destroy_v10 ()
      end if

      call wrf%Interp_var2grid (this%lats, this%lons, this%ifms, this%ifme, this%jfms, this%jfme, &
          config_flags%num_tiles, this%i_start, this%i_end, this%j_start, this%j_end, &
          't2', config_flags%hinterp_opt, this%fire_t2)

      call wrf%Interp_var2grid (this%lats, this%lons, this%ifms, this%ifme, this%jfms, this%jfme, &
          config_flags%num_tiles, this%i_start, this%i_end, this%j_start, this%j_end, &
          'q2', config_flags%hinterp_opt, this%fire_q2)

      call wrf%Interp_var2grid (this%lats, this%lons, this%ifms, this%ifme, this%jfms, this%jfme, &
          config_flags%num_tiles, this%i_start, this%i_end, this%j_start, this%j_end, &
          'psfc', config_flags%hinterp_opt, this%fire_psfc)

      call wrf%Interp_var2grid (this%lats, this%lons, this%ifms, this%ifme, this%jfms, this%jfme, &
          config_flags%num_tiles, this%i_start, this%i_end, this%j_start, this%j_end, &
          'rain', config_flags%hinterp_opt, this%fire_rain)

    end subroutine Interpolate_vars_atm_to_fire

    subroutine Print_domain (this)

      use, intrinsic :: iso_fortran_env, only : OUTPUT_UNIT

      implicit none

      class (state_fire_t), intent(in out) :: this


      write (OUTPUT_UNIT, *) ''

      write (OUTPUT_UNIT, *) 'ifds = ', this%ifds, 'ifde = ', this%ifde
      write (OUTPUT_UNIT, *) 'jfds = ', this%jfds, 'jfde = ', this%jfde
      write (OUTPUT_UNIT, *) 'kfds = ', this%kfds, 'kfde = ', this%kfde

      write (OUTPUT_UNIT, *) 'ifms = ', this%ifms, 'ifme = ', this%ifme
      write (OUTPUT_UNIT, *) 'jfms = ', this%jfms, 'jfme = ', this%jfme
      write (OUTPUT_UNIT, *) 'kfms = ', this%kfms, 'kfme = ', this%kfme

      write (OUTPUT_UNIT, *) 'ifps = ', this%ifps, 'ifpe = ', this%ifpe
      write (OUTPUT_UNIT, *) 'jfps = ', this%jfps, 'jfpe = ', this%jfpe
!      write (OUTPUT_UNIT, *) 'kfps = ', this%kfps, 'kfpe = ', this%kfpe

!      write (OUTPUT_UNIT, *) 'ifts = ', this%ifts, 'ifte = ', this%ifte
!      write (OUTPUT_UNIT, *) 'jfts = ', this%jfts, 'jfte = ', this%jfte
      write (OUTPUT_UNIT, *) 'kfts = ', this%kfts, 'kfte = ', this%kfte

      write (OUTPUT_UNIT, *) ''

    end subroutine Print_domain

    subroutine Print_tiles (this)

      implicit none

      class (state_fire_t), intent(in) :: this

      integer :: ij
      character (len = 300) :: msg


      do ij = 1, this%num_tiles
        write (msg, '(a10, 1x, i3, a4, i7, a4, i7, a4, i7, a4, i7)') &
            'CFBM TILE', ij, ' IS', this%i_start(ij), ' IE', this%i_end(ij), ' JS', this%j_start(ij), ' JE', this%j_end(ij)
        call Print_message (trim (msg))
      end do

    end subroutine Print_tiles

    function Build_restart_file_name (restart_datetime) result (file_restart)

      implicit none

      character (len = *), intent (in) :: restart_datetime

      character (len = :), allocatable :: file_restart


      file_restart = 'fire_restart_'//trim (restart_datetime)//'.nc'

    end function Build_restart_file_name

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

    subroutine Broadcast_restart_logical (this, value)

      implicit none

      class (state_fire_t), intent (in) :: this
      logical, intent (in out) :: value

      integer :: flag


#ifdef DM_PARALLEL
      flag = 0
      if (Is_restart_io_root (this)) then
        if (value) then
          flag = 1
        else
          flag = 0
        end if
      end if
      call Broadcast_restart_integer (this, flag)
      value = flag /= 0
#endif

    end subroutine Broadcast_restart_logical

    subroutine Broadcast_restart_failure (this, is_valid, failure_code, failure_msg)

      implicit none

      class (state_fire_t), intent (in) :: this
      logical, intent (in out) :: is_valid
      integer, intent (in out) :: failure_code
      character (len = *), intent (in out) :: failure_msg

      integer :: ierr, n, msg_len
      integer, dimension(:), allocatable :: msg_codes


      call Broadcast_restart_logical (this, is_valid)
      call Broadcast_restart_integer (this, failure_code)
#ifdef DM_PARALLEL
      call Require_restart_mpi_comm (this)
      msg_len = len (failure_msg)
      allocate (msg_codes(msg_len))
      if (Is_restart_io_root (this)) then
        do n = 1, msg_len
          msg_codes(n) = iachar (failure_msg(n:n))
        end do
      end if
      call MPI_Bcast (msg_codes(1), msg_len, MPI_INTEGER, RESTART_IO_ROOT, this%cfbm_comm, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems with MPI_Bcast for restart failure metadata')
      if (.not. Is_restart_io_root (this)) then
        do n = 1, msg_len
          failure_msg(n:n) = achar (msg_codes(n))
        end do
      end if
      deallocate (msg_codes)
#endif

    end subroutine Broadcast_restart_failure

    subroutine Read_restart (this, config_flags)

      implicit none

      class (state_fire_t), intent (in out) :: this
      type (namelist_t), intent (in) :: config_flags

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
        ! Restart files preserve external, interpretable nfuel_cat values. The
        ! internal fuel_index is re-derived after every restart read so stale
        ! table rows from a previous executable or fuel option cannot drive ROS.
        call this%Resolve_fuel_indices ()
        do ij = 1, this%num_tiles
          call this%ros_param%Set_params (this%ifms, this%ifme, this%jfms, this%jfme, this%i_start(ij), this%i_end(ij), &
              this%j_start(ij), this%j_end(ij), this%fuels, this%fuel_index, this%fmc_g)
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

    end subroutine Read_restart

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

    subroutine Write_restart (this, config_flags)

      implicit none

      class (state_fire_t), intent (in) :: this
      type (namelist_t), intent (in) :: config_flags

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

    end subroutine Write_restart

    subroutine Save_state (this)

      implicit none

      class (state_fire_t), intent (in) :: this

      character (len = :), allocatable :: file_output
      integer :: rank, ierr
      logical, parameter :: DEBUG_LOCAL = .false.


      if (DEBUG_LOCAL) call Print_message ('Entering Save_state...')

#ifdef DM_PARALLEL
      call Mpi_comm_rank (this%cfbm_comm, rank, ierr)
      if (ierr /= MPI_SUCCESS) call Stop_simulation ('Problems with Mpi_comm_rank ')
#else
      rank = 0
#endif

      if (DEBUG_LOCAL) call Print_message ('  Creating output file...')
      file_output='fire_output_'//this%datetime_now%datetime//'.nc'
      if (rank == 0) then
        call Create_netcdf_file (file_name = file_output)

        call Add_netcdf_dim (file_output, NAME_DIM_X, this%nx)
        call Add_netcdf_dim (file_output, NAME_DIM_Y, this%ny)
      end if

      if (DEBUG_LOCAL) call Print_message ('  Saving variables...')
      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'lats', &
          this%lats(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'lons', &
          this%lons(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fgrnhfx', &
          this%fgrnhfx(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fgrnqfx', &
          this%fgrnqfx(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fire_area', &
          this%fire_area(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fuel_frac_burnt_dt', &
          this%fuel_frac_burnt_dt(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fuel_frac', &
          this%fuel_frac(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'emis_smoke', &
          this%emis_smoke(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fire_t2', &
          this%fire_t2(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fire_q2', &
          this%fire_q2(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fire_psfc', &
          this%fire_psfc(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fire_rain', &
          this%fire_rain(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fz0', &
          this%fz0(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'fmc_g', &
          this%fmc_g(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'uf', &
          this%uf(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'vf', &
          this%vf(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'zsf', &
          this%zsf(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'lfn', &
          this%lfn(this%ifps:this%ifpe, this%jfps:this%jfpe))

      call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'nfuel_cat', &
          this%nfuel_cat(this%ifps:this%ifpe, this%jfps:this%jfpe))

      if (this%output_level > 0) then
          call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'grad_norm_ls', &
              this%grad_norm_ls(this%ifps:this%ifpe, this%jfps:this%jfpe))

          call Add_netcdf_var_mpi (file_output, this%cfbm_comm, this%nx, this%ny, this%ifps, this%ifpe, this%jfps, this%jfpe, 'grad_norm_reinit', &
              this%grad_norm_reinit(this%ifps:this%ifpe, this%jfps:this%jfpe))
      end if

      if (DEBUG_LOCAL) call Print_message ('Leaving Save_state...')

    end subroutine Save_state

    subroutine Set_mpi_comm_cfbm (this, mpi_comm_cfbm)

      implicit none

      class (state_fire_t), intent (in out) :: this
      integer, intent (in) :: mpi_comm_cfbm


      this%cfbm_comm = mpi_comm_cfbm
      this%is_cfbm_comm_set = .true.

    end subroutine Set_mpi_comm_cfbm

    subroutine Set_time_stamps (this, config_flags)

      implicit none

      class (state_fire_t), intent (in out) :: this
      type (namelist_t), intent (in) :: config_flags


      this%datetime_start = datetime_t (config_flags%start_year, config_flags%start_month, config_flags%start_day, &
          config_flags%start_hour, config_flags%start_minute, config_flags%start_second)
      this%datetime_end = datetime_t (config_flags%end_year, config_flags%end_month, config_flags%end_day, &
          config_flags%end_hour, config_flags%end_minute, config_flags%end_second)
      this%datetime_now = this%datetime_start

      this%datetime_next_output = this%datetime_start
      call this%datetime_next_output%Add_seconds (config_flags%interval_output)

      this%datetime_next_atm_update = this%datetime_start

    end subroutine Set_time_stamps

    subroutine Set_vars_to_default (this, config_flags)

      implicit none

      class (state_fire_t), intent (in out) :: this
      type (namelist_t), intent (in) :: config_flags


      if (config_flags%ideal_opt == 1) then
        this%uf(this%ifps:this%ifpe, this%jfps:this%jfpe) = config_flags%zonal_wind
        this%vf(this%ifps:this%ifpe, this%jfps:this%jfpe) = config_flags%meridional_wind
      else
        this%uf = 0.0
        this%vf = 0.0
      end if
      this%fmc_g = config_flags%fuelmc_g
        ! Init lfn more than the largest domain side
      this%lfn(this%ifps:this%ifpe, this%jfps:this%jfpe) = 2.0 * &
          max ((this%ifde - this%ifds + 1) * this%dx, (this%jfde - this%jfds + 1) * this%dy)
        ! Init tign_g a bit into the future
      this%tign_g(this%ifps:this%ifpe, this%jfps:this%jfpe) = epsilon (this%tign_g)

      this%fuel_frac(this%ifps:this%ifpe, this%jfps:this%jfpe) = 1.0
      this%fire_area(this%ifps:this%ifpe, this%jfps:this%jfpe) = 0.0

      this%emis_smoke = 0.0

      this%unit_fxlat = 2.0 * PI / (360.0 * RERADIUS)  ! earth circumference in m / 360 degrees
      this%unit_fxlong = cos (this%cen_lat * 2.0 * PI / 360.0) * this%unit_fxlat  ! latitude

    end subroutine Set_vars_to_default

  end module state_mod
