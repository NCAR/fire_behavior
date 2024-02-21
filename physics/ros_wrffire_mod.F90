  module ros_wrffire_mod

    use constants_mod, only : CMBCNST, CONVERT_J_PER_KG_TO_BTU_PER_POUND
    use fuel_anderson_mod, only: fuel_anderson_t, N_FUEL_CAT, NO_FUEL_CAT
    use stderrout_mod, only: Crash, Message
    use state_mod, only : state_fire_t
    use namelist_mod, only: namelist_t

    implicit none

    private

    public :: ros_wrffire_t

      ! fuelheat: fuel particle low heat content [btu/lb]
    real, parameter :: FUELHEAT = CMBCNST * CONVERT_J_PER_KG_TO_BTU_PER_POUND
    integer, parameter :: FIRE_ADVECTION = 1 ! "0 = fire spread computed from normal wind speed/slope, 1 = fireline particle speed projected on normal" "0"

    type :: ros_wrffire_t
    contains
      procedure, public :: Calc_ros => Calc_ros_wrffire
      procedure, public :: Set_ros_parameters => Set_ros_parameters_wrffire
    end type ros_wrffire_t

  contains

    subroutine Calc_ros_wrffire (this, ros_base, ros_wind, ros_slope, nvx, nvy, i, j, grid)

      implicit none

      ! m/s = (ft/min) * 0.3048 / 60.0 = (ft/min) * .00508
      ! ft/min = m/s * 2.2369 * 88.0 = m/s *  196.850

      class (ros_wrffire_t), intent (in) :: this
      real, intent (out) :: ros_base, ros_wind, ros_slope
      real, intent (in) :: nvx, nvy
      integer, intent(in) :: i, j
      type (state_fire_t), intent (in) :: grid

      real :: speed, tanphi ! windspeed and slope in the direction normal to the fireline
      real :: umid, phis, phiw, spdms, umidm, excess
      real :: ros_back
      real, parameter :: ROS_MAX = 6.0
      real :: cor_wind, cor_slope


      if (FIRE_ADVECTION /= 0) then
          ! wind speed is total speed 
        speed = sqrt (grid%uf(i, j) * grid%uf(i, j) + grid%vf(i, j) * grid%vf(i, j)) + tiny (speed)
          ! slope is total slope
        tanphi = sqrt (grid%dzdxf(i, j) * grid%dzdxf(i, j) + grid%dzdyf(i, j) * grid%dzdyf(i, j)) + tiny (tanphi)
          ! cos of wind and spread, if >0
        cor_wind =  max (0.0, (grid%uf(i, j) * nvx + grid%vf(i, j) * nvy) / speed)
          ! cos of slope and spread, if >0
        cor_slope = max (0.0, (grid%dzdxf(i, j) * nvx + grid%dzdyf(i, j) * nvy) / tanphi)
      else
          ! wind speed in spread direction
        speed = grid%uf(i, j) * nvx + grid%vf(i, j) * nvy
          ! slope in spread direction
        tanphi = grid%dzdxf(i, j) * nvx + grid%dzdyf(i, j) * nvy
        cor_wind = 1.0
        cor_slope = 1.0
      end if

      if (.not. grid%ischap(i,j) > 0.0) then
          ! Rothermel
        spdms = max (speed, 0.0)
        umidm = min (spdms, 30.0)
        umid = umidm * 196.850 ! m/s to ft/min
        phiw = umid ** grid%bbb(i, j) * grid%phiwc(i, j)
        phis = 0.0
        if (tanphi > 0.0) phis = 5.275 * (grid%betafl(i, j)) ** (-0.3) * tanphi ** 2
        ros_base = grid%r_0(i, j) * 0.00508 ! ft/min to m/s
        ros_wind = ros_base * phiw
        ros_slope = ros_base * phis
      else
        spdms = max (speed, 0.0)
        ros_back = 0.03333    ! chaparral backing fire spread rate 0.033 m/s   ! param!
          ! spread rate, m/s
        ros_wind = 1.2974 * spdms ** 1.41
        ros_wind = max (ros_wind, ros_back)
        ros_slope = 0.0
        ros_base = 0.0
      end if

      ros_wind = ros_wind * cor_wind
      ros_slope = ros_slope * cor_slope

        ! Limit the ros
      excess = ros_base + ros_wind + ros_slope - ROS_MAX
      if (excess > 0.0) then
          ! take excess out of wind and slope in proportion
        ros_wind = ros_wind - excess * ros_wind / (ros_wind + ros_slope)
        ros_slope = ros_slope - excess * ros_slope/ (ros_wind + ros_slope)
      end if

    end subroutine Calc_ros_wrffire

    subroutine Set_ros_parameters_wrffire (this, ifds, ifde, jfds, jfde, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
        fdx, fdy, nfuel_cat, fuel_time, grid, fuel_model, config_flags)

      implicit none

    !D      BMST           RATIO OF LATENT TO SENSIBLE HEAT FROM SFC BURN:
    !                        % of total fuel mass that is water (not quite
    !                        = % fuel moisture).    BMST= (H20)/(H20+DRY)
    !                        so BMST = FUELMC_G / (1 + FUELMC_G)

      class (ros_wrffire_t), intent (in) :: this
      type (state_fire_t) :: grid
      type (fuel_anderson_t) :: fuel_model
      integer, intent(in) :: ifds, ifde, jfds, jfde, ifts, ifte, jfts, jfte, ifms, ifme, jfms, jfme
      real, intent(in) :: fdx, fdy                                    ! fire mesh spacing
      type (namelist_t), intent(in) :: config_flags
      real, dimension (ifms:ifme, jfms:jfme), intent (in) :: nfuel_cat  ! fuel data
      real, dimension (ifms:ifme, jfms:jfme), intent (out) :: fuel_time ! fire params arrays


      real ::  fuelload, fueldepth, rtemp1, rtemp2, qig, epsilon, rhob, wn, betaop, e, c, &
          xifr, etas, etam, a, gammax, gamma, ratio, ir, fuelloadm,fdxinv,fdyinv, bmst
      integer:: i, j, k, kk
      character (len = 128) :: msg

      integer, parameter :: NF_SB = 204 ! maximum category on 
      integer, dimension (1:NF_SB) :: ksb ! Anderson82 + S&B2005 fuel categories array


        ! Cross walk to Scott & Burgan
      do kk = 1, NF_SB
        ksb(kk) = NO_FUEL_CAT
      end do
        ! Anderson 1982
      ksb(1) = 1
      ksb(2) = 2
      ksb(3) = 3
      ksb(4) = 4
      ksb(5) = 5
      ksb(6) = 6
      ksb(7) = 7
      ksb(8) = 8
      ksb(9) = 9
      ksb(10) = 10
      ksb(11) = 11
      ksb(12) = 12
      ksb(13) = 13
        ! Scott & Burgan crosswalks
        ! Short grass -- 1
      ksb(101) = 1
      ksb(104) = 1
      ksb(107) = 1
        ! Timber grass and understory -- 2
      ksb(102) = 2
      ksb(121) = 2
      ksb(122) = 2
      ksb(123) = 2
      ksb(124) = 2
        ! Tall grass -- 3
      ksb(103) = 3
      ksb(105) = 3
      ksb(106) = 3
      ksb(108) = 3
      ksb(109) = 3
        ! Chaparral -- 4
      ksb(145) = 4
      ksb(147) = 4
        ! Brush -- 5
      ksb(142) = 5
        ! Dormant Brushi -- 6
      ksb(141) = 6
      ksb(146) = 6
        ! Southern Rough -- 7
      ksb(143) = 7
      ksb(144) = 7
      ksb(148) = 7
      ksb(149) = 7
        ! Compact Timber Litter -- 8
      ksb(181) = 8
      ksb(183) = 8
      ksb(184) = 8
      ksb(187) = 8
        ! Hardwood Litter -- 9
      ksb(182) = 9
      ksb(186) = 9
      ksb(188) = 9
      ksb(189) = 9
        ! Timber (understory) -- 10
      ksb(161) = 10
      ksb(162) = 10
      ksb(163) = 10
      ksb(164) = 10
      ksb(165) = 10
        ! Light Logging Slash -- 11
      ksb(185) = 11
      ksb(201) = 11
        ! Medium Logging Slash -- 12
      ksb(202) = 12
        ! Heavy Logging Slash -- 13
      ksb(203) = 13
      ksb(204) = 13

      Loop_j: do j = jfts, jfte
        Loop_i: do i = ifts, ifte
          k = ksb(int (nfuel_cat(i, j)))
          if(k == NO_FUEL_CAT) then
            grid%fgip(i, j) = 0.0
            grid%ischap(i, j) = 0.0
              ! set to 1.0 to prevent grid%betafl(i,j)**(-0.3) to be Inf in fire_ros
            grid%betafl(i, j) = 1.0
            grid%bbb(i, j) = 1.0
              ! does not matter, just what was there before
            fuel_time(i, j) = 7.0 / 0.85
            grid%phiwc(i, j) = 0.0
            grid%r_0(i, j) = 0.0
              ! Ib/ROS zero for no fuel
            grid%iboros(i, j) = 0.0
          else
            if (k < 1 .or. k > N_FUEL_CAT) then
              !$OMP CRITICAL(FIRE_PHYS_CRIT)
              write(msg, '(3(a,i5))') 'nfuel_cat(', i, ',', j, ')=', k
              !$OMP END CRITICAL(FIRE_PHYS_CRIT)
              call Message (msg, config_flags%fire_print_msg)
              call Crash ('Set_fire_params: fuel category out of bounds')
            end if

              ! set fuel time constant: weight=1000 => 40% decrease over 10 min
              ! fuel decreases as exp(-t/fuel_time) 
              ! exp(-600*0.85/1000) = approx 0.6 
            fuel_time(i, j) = fuel_model%weight(k) / 0.85 ! cell based

            grid%ischap(i, j) = fuel_model%ichap(k)
            grid%fgip(i, j) = fuel_model%fgi(k)

              ! Settings of fire spread parameters from Rothermel
              ! No need to recalculate if FMC does not change
            bmst = grid%fmc_g(i, j) / (1.0 + grid%fmc_g(i, j))
              !  fuelload without moisture
            fuelloadm = (1.0 - bmst) * fuel_model%fgi(k)
            fuelload = fuelloadm * (0.3048) ** 2 * 2.205 ! to lb/ft^2
            fueldepth = fuel_model%fueldepthm(k) / 0.3048 ! to ft
              ! packing ratio
            grid%betafl(i, j) = fuelload / (fueldepth * fuel_model%fueldens(k))
              ! optimum packing ratio
            betaop = 3.348 * fuel_model%savr(k) ** (-0.8189)
              ! heat of preignition, btu/lb
            qig = 250.0 + 1116.0 * grid%fmc_g(i, j)
              ! effective heating number
            epsilon = exp (-138.0 / fuel_model%savr(k))
              ! ovendry bulk density, lb/ft^3
            rhob = fuelload/fueldepth

              ! const in wind coef
            c = 7.47 * exp (-0.133 * fuel_model%savr(k) ** 0.55)
            grid%bbb(i,j) = 0.02526 * fuel_model%savr(k) ** 0.54
            e = 0.715 * exp (-3.59e-4 * fuel_model%savr(k))
            grid%phiwc(i,j) = c * (grid%betafl(i, j) / betaop) ** (-e)

            rtemp2 = fuel_model%savr(k) ** 1.5
              ! maximum rxn vel, 1/min
            gammax = rtemp2 / (495.0 + 0.0594 * rtemp2)
              ! coef for optimum rxn vel
            a = 1.0 / (4.774 * fuel_model%savr(k) ** 0.1 - 7.27)
            ratio = grid%betafl(i,j)/betaop
              !optimum rxn vel, 1/min
            gamma = gammax * (ratio ** a) * exp(a * (1.0 - ratio))

             ! net fuel loading, lb/ft^2
            wn = fuelload/(1 + fuel_model%st(k))
            rtemp1 = grid%fmc_g(i, j) / fuel_model%fuelmce(k)
              ! moist damp coef
            etam = 1.0 - 2.59 * rtemp1 + 5.11 * rtemp1 ** 2 - 3.52 * rtemp1 ** 3
              ! mineral damping coef
            etas = 0.174 * fuel_model%se(k) ** (-0.19)
              !rxn intensity,btu/ft^2 min
            ir = gamma * wn * FUELHEAT * etam * etas
            ! irm = ir * 1055./( 0.3048**2 * 60.) * 1.e-6     !for mw/m^2
            grid%iboros(i,j) = ir * 1055.0 / ( 0.3048 ** 2 * 60.0) * 1.e-3 * (60.0 * 12.6 / fuel_model%savr(k)) ! I_R x t_r (kJ m^-2)
              ! propagating flux ratio
            xifr = exp((0.792 + 0.681 * fuel_model%savr(k) ** 0.5) &
                * (grid%betafl(i, j) + 0.1)) / (192.0 + 0.2595 * fuel_model%savr(k))

              ! r_0 is the spread rate for a fire on flat ground with no wind.
              ! default spread rate in ft/min
            grid%r_0(i, j) = ir * xifr / (rhob * epsilon * qig)
          end if
        end do Loop_i
      end do Loop_j

    end subroutine Set_ros_parameters_wrffire

  end module ros_wrffire_mod
