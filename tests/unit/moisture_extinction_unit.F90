program moisture_extinction_unit

  ! Check the Rothermel extinction correction for all 13 Anderson fuel categories.
  ! Moisture is water mass per dry fuel mass and must remain unchanged, including
  ! valid values above extinction. Reaction intensity, its residence-time product
  ! (iboros), and base spread must be exactly zero at and above extinction, while
  ! valid values below extinction retain the existing damping polynomial.
  ! Downstream checks cover spread and flame length, including the independent
  ! chaparral spread law and nonburnable category 14.

  use, intrinsic :: iso_fortran_env, only : REAL64
  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_value, ieee_quiet_nan, &
      ieee_positive_inf, ieee_negative_inf
  use constants_mod, only : CMBCNST, CONVERT_J_PER_KG_TO_BTU_PER_POUND
  use fuel_anderson_mod, only : fuel_anderson_t
  use ros_wrffire_mod, only : ros_wrffire_t
  use fire_physics_mod, only : Calc_flame_length
#ifdef DM_PARALLEL
  use mpi
#endif

  implicit none

  type(fuel_anderson_t) :: fuels
  type(ros_wrffire_t) :: params
  real :: category(0:15,0:3), moisture(0:15,0:3), original(1:14,1:2)
  real :: spread(0:15,0:3), flame(0:15,0:3), front(0:15,0:3), area(0:15,0:3)
  real :: ir, expected_ir, expected_r0, expected_iboros, factor, nan, wind, slope
  integer :: i, j, tile, sample, direction, ierr
  character(len=40) :: scenario

#ifdef DM_PARALLEL
  call MPI_Init(ierr)
#endif
  call get_command_argument(1, scenario)
  call fuels%Initialization(1.0)
  call params%Init(0,15,0,3)
  nan = ieee_value(0.0, ieee_quiet_nan)
  ! Unavailable halos and the nonburnable cell must not enter fuel physics.
  category = nan
  moisture = nan
  do i = 1, 14
    category(i,1:2) = real(i)
  end do
  area = 0.5

  ! Sample dry-to-moist fuel below extinction and the nearest representable values
  ! on each side of the threshold. These boundary cases distinguish an exact
  ! extinction cutoff from polynomial cancellation or an unintended moisture cap.
  do sample = 1, 4
    select case (trim(scenario))
    case ('below')
      factor = 0.25 * (sample - 1)
    case ('edge')
      factor = 1.0
    case ('at')
      factor = 1.0
    case ('above')
      factor = real(sample + 1)
    case default
      factor = 0.5
    end select
    do i = 1, 13
      moisture(i,1:2) = factor * fuels%fuelmce(i)
      if (scenario == 'edge') moisture(i,1:2) = nearest(fuels%fuelmce(i), -1.0)
      if (scenario == 'above' .and. sample == 1) moisture(i,1:2) = nearest(fuels%fuelmce(i), 1.0)
    end do
    ! Each invalid-input scenario must stop in Set_params with the specific
    ! diagnostic registered in CMakeLists.txt. A generic failure or silent
    ! acceptance does not establish that the input validation is correct.
    select case (trim(scenario))
    case ('negative_moisture')
      moisture(1,1) = -0.01
    case ('nan_moisture')
      moisture(1,1) = nan
    case ('inf_moisture')
      moisture(1,1) = ieee_value(0.0, ieee_positive_inf)
    case ('neginf_moisture')
      moisture(1,1) = ieee_value(0.0, ieee_negative_inf)
    case ('zero_extinction')
      fuels%fuelmce(1) = 0.0
    case ('negative_extinction')
      fuels%fuelmce(1) = -0.1
    case ('nan_extinction')
      fuels%fuelmce(1) = nan
    case ('inf_extinction')
      fuels%fuelmce(1) = ieee_value(0.0, ieee_positive_inf)
    case ('neginf_extinction')
      fuels%fuelmce(1) = ieee_value(0.0, ieee_negative_inf)
    case ('below', 'edge', 'at', 'above')
    case default
      error stop 'unknown extinction scenario'
    end select
    original = moisture(1:14,1:2)
    ! Exercise disjoint owned tiles, concurrently in OpenMP builds. Set_params
    ! also reconstructs derived spread parameters after restart; checkpoint I/O
    ! and continuous-versus-restarted evolution are covered by regression cases.
    !$omp parallel do private(tile)
    do tile = 1, 2
      call params%Set_params(0,15,0,3,1,14,tile,tile,fuels,category,moisture)
    end do
    !$omp end parallel do
    if (index(scenario, 'moisture') > 0 .or. index(scenario, 'extinction') > 0) &
        error stop 'invalid input was accepted'
    if (any(moisture(1:13,1:2) /= original(1:13,1:2))) error stop 'physical moisture was modified'

    do i = 1, 13
      call Reference_reaction(i, moisture(i,1), expected_ir, expected_r0, expected_iboros)
      do j = 1, 2
        ! Recover reaction intensity from its stored residence-time product.
        ir = params%iboros(i,j) / (1055.0 / (0.3048**2 * 60.0) * 1.e-3 * (60.0 * 384.0 / fuels%savr(i)))
        call Check_value(ir, expected_ir, 'reaction intensity')
        call Check_value(params%iboros(i,j), expected_iboros, 'iboros')
        call Check_value(params%r_0(i,j), expected_r0, 'base spread')
        if (scenario == 'at' .or. scenario == 'above') then
          if (ir /= 0.0 .or. params%iboros(i,j) /= 0.0 .or. params%r_0(i,j) /= 0.0) &
              error stop 'nonzero reaction at or above extinction'
        end if
      end do
      ! Hexadecimal values permit an exact before/after comparison below extinction.
      if (scenario == 'below') write(*,'(a,2i3,2(1x,z8.8))') 'below signature ', sample, i, &
          transfer(params%iboros(i,1),0), transfer(params%r_0(i,1),0)
    end do
    if (any(params%iboros(14,1:2) /= 0.0) .or. any(params%r_0(14,1:2) /= 0.0)) &
        error stop 'nonburnable cell has a reaction'

    do direction = 1, 3
      wind = 0.0
      slope = 0.0
      if (direction > 1) wind = 3.0
      if (direction == 3) slope = 0.2
      do j = 1, 2
        do i = 1, 14
          spread(i,j) = params%Calc_ros(0,15,0,3,i,j,1.0,0.0,wind,0.0,slope,0.0)
        end do
      end do
      call Calc_flame_length(1,14,1,2,0,15,0,3,spread,params%iboros,flame,front,area)
      if (any(.not. ieee_is_finite(spread(1:14,1:2))) .or. any(spread(1:14,1:2) < 0.0)) &
          error stop 'invalid spread rate'
      if (any(.not. ieee_is_finite(flame(1:14,1:2))) .or. any(flame(1:14,1:2) < 0.0)) &
          error stop 'invalid flame length'
      if (any(front(1:14,1:2) /= spread(1:14,1:2))) error stop 'front spread differs'
      do i = 1, 13
        if (moisture(i,1) >= fuels%fuelmce(i)) then
          if (any(flame(i,1:2) /= 0.0)) error stop 'nonzero extinction flame length'
          ! Anderson 4 uses the independent empirical chaparral spread law.
          if (fuels%ichap(i) == 0 .and. any(spread(i,1:2) /= 0.0)) error stop 'Rothermel spread at extinction'
        else
          if (any(flame(i,1:2) <= 0.0 .and. spread(i,1:2) > 0.0)) error stop 'missing below-extinction flame'
        end if
        if (fuels%ichap(i) /= 0) call Check_value(spread(i,1), &
            min(max(1.2974 * wind**1.41, 0.03333) * wind / (wind + tiny(wind)), 6.0), 'chaparral spread')
      end do
      if (any(flame(14,1:2) /= 0.0) .or. any(spread(14,1:2) /= 0.0)) error stop 'nonburnable propagation'
    end do
  end do
  print *, 'moisture extinction checks passed: ', trim(scenario)
#ifdef DM_PARALLEL
  call MPI_Finalize(ierr)
#endif

contains

  subroutine Reference_reaction(k, m, intensity, base_ros, integrated)
    integer, intent(in) :: k
    real, intent(in) :: m
    real, intent(out) :: intensity, base_ros, integrated
    real(REAL64) :: load, depth, packing, optimum, sigma, a, rate, damping, flux, heat
    real :: ratio
    ! Evaluate the dimensional Rothermel expressions in double precision.
    ! Retain single-precision damping to test the existing polynomial even
    ! one representable moisture value below extinction, where it cancels.
    ratio = m / fuels%fuelmce(k)
    damping = 0.0_REAL64
    if (m < fuels%fuelmce(k)) damping = 1.0 - 2.59 * ratio + 5.11 * ratio**2 - 3.52 * ratio**3
    sigma = fuels%savr(k)
    load = real(fuels%fgi(k), REAL64) / (1.0_REAL64 + m) * 0.3048_REAL64**2 * 2.205_REAL64
    depth = real(fuels%fueldepthm(k), REAL64) / 0.3048_REAL64
    packing = load / (depth * fuels%fueldens(k))
    optimum = 3.348_REAL64 * sigma**(-0.8189_REAL64)
    a = 1.0_REAL64 / (4.774_REAL64 * sigma**0.1_REAL64 - 7.27_REAL64)
    rate = sigma**1.5_REAL64 / (495.0_REAL64 + 0.0594_REAL64 * sigma**1.5_REAL64) * &
        (packing / optimum)**a * exp(a * (1.0_REAL64 - packing / optimum))
    heat = CMBCNST * CONVERT_J_PER_KG_TO_BTU_PER_POUND
    intensity = rate * load / (1.0_REAL64 + fuels%st(k)) * heat * damping * &
        0.174_REAL64 * real(fuels%se(k), REAL64)**(-0.19_REAL64)
    integrated = intensity * 1055.0_REAL64 / (0.3048_REAL64**2 * 60.0_REAL64) * &
        1.e-3_REAL64 * (60.0_REAL64 * 384.0_REAL64 / sigma)
    flux = exp((0.792_REAL64 + 0.681_REAL64 * sqrt(sigma)) * (packing + 0.1_REAL64)) / &
        (192.0_REAL64 + 0.2595_REAL64 * sigma)
    base_ros = intensity * flux / (load / depth * exp(-138.0_REAL64 / sigma) * &
        (250.0_REAL64 + 1116.0_REAL64 * m))
  end subroutine Reference_reaction

  subroutine Check_value(value, expected, quantity)
    real, intent(in) :: value, expected
    character(len=*), intent(in) :: quantity
    if (.not. ieee_is_finite(value)) error stop 'nonfinite reaction quantity'
    if (value < 0.0) error stop 'negative reaction quantity'
    ! Bound accumulated single-precision rounding against the independent
    ! double-precision calculation, without a nonzero absolute-error floor.
    if (abs(value - expected) > 64.0 * epsilon(value) * abs(expected)) then
      print *, quantity, value, expected
      error stop 'reaction reference mismatch'
    end if
  end subroutine Check_value

end program moisture_extinction_unit
