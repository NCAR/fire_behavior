program wind_state_unit

  ! Exercise state-level wind selection on a nonsquare fire patch. The 10 m
  ! case checks horizontal mapping followed by fuel-specific adjustment and
  ! release of the input arrays. The invalid case must stop before using winds.
  use state_mod, only : state_fire_t
  use wrfdata_mod, only : wrfdata_t
  use namelist_mod, only : namelist_t
  use interp_mod, only : VINTERP_WINDS_FROM_10M_WINDS, HINTERP_BILINEAR
  use fuel_anderson_mod, only : fuel_anderson_t
#ifdef DM_PARALLEL
  use mpi
#endif

  implicit none

  type (state_fire_t) :: fire
  type (wrfdata_t) :: atmosphere
  type (namelist_t) :: config
  character (len = 20) :: scenario
  integer :: ierr

#ifdef DM_PARALLEL
  call MPI_Init (ierr)
#endif
  call get_command_argument (1, scenario)

  fire%ifms = 1
  fire%ifme = 3
  fire%jfms = 1
  fire%jfme = 2
  fire%num_tiles = 1
  fire%i_start = [1]
  fire%i_end = [3]
  fire%j_start = [1]
  fire%j_end = [2]
  allocate (fire%lats(3,2), fire%lons(3,2), fire%fz0(3,2))
  allocate (fire%uf(3,2), fire%vf(3,2), fire%nfuel_cat(3,2))
  allocate (fire%fire_t2(3,2), fire%fire_q2(3,2), fire%fire_psfc(3,2), fire%fire_rain(3,2))
  fire%lats = 40.0
  fire%lons = -105.0
  fire%nfuel_cat(:,1) = 1.0
  fire%nfuel_cat(:,2) = 2.0
  allocate (fuel_anderson_t :: fire%fuels)
  fire%fuels%waf = [0.25, 0.5]

  atmosphere%cen_lat = 40.0
  atmosphere%cen_lon = -105.0
  atmosphere%stand_lon = -105.0
  atmosphere%truelat1 = 30.0
  atmosphere%truelat2 = 60.0
  atmosphere%dx = 100.0
  atmosphere%dy = 100.0
  allocate (atmosphere%lats(2,2), atmosphere%lons(2,2))
  allocate (atmosphere%u10(2,2), atmosphere%v10(2,2), atmosphere%z0(2,2))
  allocate (atmosphere%t2(2,2), atmosphere%q2(2,2), atmosphere%psfc(2,2), atmosphere%rain(2,2))
  atmosphere%u10(1,:) = 4.0
  atmosphere%u10(2,:) = 12.0
  atmosphere%v10 = -4.0
  atmosphere%z0 = 0.1
  atmosphere%t2 = 300.0
  atmosphere%q2 = 0.005
  atmosphere%psfc = 90000.0
  atmosphere%rain = 0.0
  config%hinterp_opt = HINTERP_BILINEAR
  config%wind_vinterp_opt = VINTERP_WINDS_FROM_10M_WINDS
  if (scenario == 'invalid') config%wind_vinterp_opt = -1

  call fire%Interpolate_vars_atm_to_fire (atmosphere, config)
  if (scenario == 'invalid') error stop 'invalid wind option was accepted'

  ! The common target is halfway between the source columns: U=8, V=-4.
  ! Each row then receives its own WAF, applied exactly once.
  if (any (fire%uf(:,1) /= 2.0) .or. any (fire%uf(:,2) /= 4.0)) error stop '10 m U adjustment differs'
  if (any (fire%vf(:,1) /= -1.0) .or. any (fire%vf(:,2) /= -2.0)) error stop '10 m V adjustment differs'
  if (allocated(atmosphere%u10) .or. allocated(atmosphere%v10)) error stop '10 m winds were not released'
  print *, 'state wind checks passed'
#ifdef DM_PARALLEL
  call MPI_Finalize (ierr)
#endif

end program wind_state_unit
