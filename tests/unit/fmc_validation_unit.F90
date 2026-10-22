program fmc_validation_unit

  ! Exercise validation at the moisture-component boundary with two owned tiles.
  ! Invalid class initial values produce invalid parameterization output; invalid
  ! averaging weights separately exercise the effective-moisture check. Both
  ! failures must be diagnosed after the parallel update and before ROS uses them.
  ! Restored zero and wetter-than-extinction states remain valid and unchanged;
  ! unavailable halos and nonburnable cells must not enter the validity checks.

  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_value, ieee_quiet_nan, &
      ieee_positive_inf, ieee_negative_inf
  use fmc_mod, only : Check_fmc
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
  call fuels%Initialization (1.0)
  call moisture%Init (fuels, 0.08, 0.30, 0, 5, 0, 3, 0, 4.0)
  call ros%Init (0, 5, 0, 3)
  nan = ieee_value (0.0, ieee_quiet_nan)
  category = nan
  category(1:4,1:2) = 1.0
  category(4,2) = real (fuels%no_fuel_cat)
  fmc = nan
  moisture%fmc_gc = nan

  if (scenario == 'restored_zero' .or. scenario == 'restored_wet') then
    restored = 0.0
    if (scenario == 'restored_wet') restored = 2.0
    fmc(1:4,1:2) = restored
    moisture%fmc_gc(1:4,:,1:2) = restored
    fmc(4,2) = nan
    moisture%fmc_gc(4,:,2) = nan
    call moisture%Check_moisture_classes (0,5,0,3,1,4,1,2,category,fuels%no_fuel_cat,'Restart')
    call Check_fmc (0,5,0,3,1,4,1,2,category,fuels%no_fuel_cat,fmc,'Restart')
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

    select case (trim (scenario))
      case ('valid')
        invalid = 0.0
      case ('class_negative', 'average_negative')
        invalid = -1.0
      case ('class_nan', 'average_nan')
        invalid = nan
      case ('class_inf', 'average_inf')
        invalid = ieee_value (0.0, ieee_positive_inf)
      case ('class_neginf', 'average_neginf')
        invalid = ieee_value (0.0, ieee_negative_inf)
      case default
        error stop 'unknown moisture validation scenario'
    end select
    if (index (scenario, 'class_') == 1) moisture%fmc_gc_initial_value(5) = invalid
    if (index (scenario, 'average_') == 1) moisture%fmc_gw(1,:) = invalid

    call moisture%Advance_fmc_model (0,4.0,1,4.0,0,5,0,3,i_start,i_end,j_start,j_end,2, &
        rain,t,q,p,rain_old,t_old,q_old,p_old,rh,0.08,fmc,category,fuels,ros)
    if (scenario /= 'valid') error stop 'invalid moisture update was accepted'
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
