program fmc_validation_unit

  ! Exercise validation at the moisture-component boundary with two owned tiles.
  ! Invalid fuel loads must be rejected when fixed averaging weights are built.
  ! Invalid class initial values must be diagnosed after the parallel update and
  ! before averaging, including a live class with zero weight in Anderson fuel 1.
  ! Independently restored fmc_g must be checked even when every class is valid.
  ! Restored zero and wetter-than-extinction states remain valid and unchanged;
  ! unavailable halos and nonburnable cells must not enter the validity checks.

  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_value, ieee_quiet_nan, &
      ieee_positive_inf, ieee_negative_inf
  use fmc_wrffire_mod, only : fmc_wrffire_t
  use fuel_anderson_mod, only : fuel_anderson_t
  use ros_wrffire_mod, only : ros_wrffire_t
#ifdef DM_PARALLEL
  use mpi
#endif

  implicit none

  type (fmc_wrffire_t) :: moisture
  type (fuel_anderson_t) :: fuels
  type (ros_wrffire_t) :: ros
  real :: rain(0:5,0:3), t(0:5,0:3), q(0:5,0:3), p(0:5,0:3)
  real :: rain_old(0:5,0:3), t_old(0:5,0:3), q_old(0:5,0:3), p_old(0:5,0:3)
  real :: rh(0:5,0:3), fmc(0:5,0:3), category(0:5,0:3), nan, invalid, restored
  integer :: i, j, ierr
  integer :: i_start(2) = [1,1], i_end(2) = [4,4], j_start(2) = [1,2], j_end(2) = [1,2]
  character (len = 40) :: scenario

#ifdef DM_PARALLEL
  call MPI_Init (ierr)
#endif
  call get_command_argument (1, scenario)
  nan = ieee_value (0.0, ieee_quiet_nan)
  select case (trim (scenario))
    case ('valid', 'zero_load', 'load_overflow', 'restored_zero', 'restored_wet')
      invalid = 0.0
    case ('class_negative', 'load_negative', 'restored_negative')
      invalid = -1.0
    case ('class_nan', 'load_nan', 'restored_nan')
      invalid = nan
    case ('class_inf', 'load_inf', 'restored_inf')
      invalid = ieee_value (0.0, ieee_positive_inf)
    case ('class_neginf', 'load_neginf', 'restored_neginf')
      invalid = ieee_value (0.0, ieee_negative_inf)
    case default
      error stop 'unknown moisture validation scenario'
  end select
  call fuels%Initialization (1.0)
  ! Perturb the input to initialization rather than corrupting fixed weights
  ! after they have been validated. No moisture advance should be needed to fail.
  if (index (scenario, 'load_') == 1) fuels%fgi_1h(1) = invalid
  if (scenario == 'load_overflow') then
    fuels%fgi_1h(1) = huge (1.0)
    fuels%fgi_10h(1) = huge (1.0)
  else if (scenario == 'zero_load') then
    fuels%fgi_1h(1) = 0.0
    fuels%fgi_10h(1) = 0.0
    fuels%fgi_100h(1) = 0.0
    fuels%fgi_1000h(1) = 0.0
    fuels%fgi_live(1) = 0.0
  end if
  call moisture%Init (fuels, 0.08, 0.30, 0, 5, 0, 3, 0, 4.0)
  if (index (scenario, 'load_') == 1) error stop 'invalid averaging fuel load was accepted'
  call ros%Init (0, 5, 0, 3)
  category = nan
  category(1:4,1:2) = 1.0
  category(4,2) = real (fuels%no_fuel_cat)
  fmc = nan
  moisture%fmc_gc = nan

  if (index (scenario, 'restored_') == 1) then
    restored = 0.0
    if (scenario == 'restored_wet') restored = 2.0
    fmc(1:4,1:2) = restored
    moisture%fmc_gc(1:4,:,1:2) = restored
    fmc(4,2) = nan
    moisture%fmc_gc(4,:,2) = nan
    call moisture%Check_moisture_classes (0,5,0,3,1,4,1,2,category,fuels%no_fuel_cat,'Restart')
    ! The restart reader loads fmc_g independently of the valid class array and
    ! validates it through the common binding inherited from the FMC base type.
    if (scenario /= 'restored_zero' .and. scenario /= 'restored_wet') fmc(2,1) = invalid
    call moisture%Check_fmc (0,5,0,3,1,4,1,2,category,fuels%no_fuel_cat,fmc,'Restart')
    if (scenario /= 'restored_zero' .and. scenario /= 'restored_wet') error stop 'invalid restored moisture was accepted'
    if (any (fmc(1:3,1:2) /= restored)) error stop 'restored moisture was modified'
    if (any (moisture%fmc_gc(1:3,:,1:2) /= restored)) error stop 'restored class moisture was modified'
  else
    rain = nan
    t = nan
    q = nan
    p = nan
    rain_old = nan
    t_old = nan
    q_old = nan
    p_old = nan
    rain(1:4,1:2) = 0.0
    t(1:4,1:2) = 300.0
    q(1:4,1:2) = 0.008
    p(1:4,1:2) = 90000.0

    if (index (scenario, 'class_') == 1) moisture%fmc_gc_initial_value(5) = invalid

    call moisture%Advance_fmc_model (0,4.0,1,4.0,0,5,0,3,i_start,i_end,j_start,j_end,2, &
        rain,t,q,p,rain_old,t_old,q_old,p_old,rh,0.08,fmc,category,fuels,ros)
    if (scenario /= 'valid' .and. scenario /= 'zero_load') error stop 'invalid moisture update was accepted'
    if (scenario == 'zero_load') then
      ! An empty category retains the existing all-zero weights and mean.
      if (any (moisture%fmc_gw(1,:) /= 0.0)) error stop 'nonzero weights for empty fuel category'
      if (any (fmc(1:3,1:2) /= 0.0)) error stop 'nonzero mean for empty fuel category'
    end if
    do j = 1, 2
      do i = 1, 4
        if (int (category(i,j)) == fuels%no_fuel_cat) cycle
        if (.not. ieee_is_finite (fmc(i,j)) .or. fmc(i,j) < 0.0) error stop 'invalid effective moisture'
        if (.not. ieee_is_finite (ros%iboros(i,j)) .or. ros%iboros(i,j) < 0.0) error stop 'invalid reaction'
      end do
    end do
  end if

  print *, 'moisture validation checks passed: ', trim (scenario)
#ifdef DM_PARALLEL
  call MPI_Finalize (ierr)
#endif

end program fmc_validation_unit
