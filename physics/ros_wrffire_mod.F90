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

      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      !     ... calculates fire spread rate with McArthur formula or Rothermel
      !           using fuel type of fuel cell
      !      
      !         m/s =(ft/min) *.3048/60. =(ft/min) * .00508   ! conversion rate
      !         ft/min = m/s * 2.2369 * 88. = m/s *  196.850 ! conversion rate
      !      
      !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      class (ros_wrffire_t), intent (in) :: this
      real, intent(out) :: ros_base, ros_wind, ros_slope ! rate of spread contribution due to fuel, wind, and slope
      real, intent(in) :: nvx, nvy
      integer, intent(in) :: i,j         ! node mesh coordinates
      type (state_fire_t), target :: grid

      real :: speed, tanphi ! windspeed and slope in the direction normal to the fireline
      real :: umid, phis, phiw, spdms, umidm, excess
      real :: ros_back
      real, parameter::ros_max=6.
      real ::cor_wind, cor_slope


      if (FIRE_ADVECTION /= 0) then ! from flags in module_fr_fire_util
          ! wind speed is total speed 
        speed = sqrt(grid%uf(i,j) * grid%uf(i,j) + grid%vf(i,j) * grid%vf(i,j)) +tiny(speed)
          ! slope is total slope
        tanphi = sqrt(grid%dzdxf(i,j) * grid%dzdxf(i,j) + grid%dzdyf(i,j) * grid%dzdyf(i,j)) + tiny(tanphi)
          ! cos of wind and spread, if >0
        cor_wind =  max(0., (grid%uf(i,j) * nvx + grid%vf(i,j) * nvy) / speed)
          ! cos of slope and spread, if >0
        cor_slope = max(0., (grid%dzdxf(i,j) * nvx + grid%dzdyf(i,j) * nvy) / tanphi)
      else
          ! wind speed in spread direction
        speed = grid%uf(i,j) * nvx + grid%vf(i,j) * nvy
          ! slope in spread direction
        tanphi = grid%dzdxf(i,j) * nvx + grid%dzdyf(i,j) * nvy
        cor_wind = 1.
        cor_slope = 1.
      endif

      if (.not. grid%ischap(i,j) > 0.) then
        ! Fuel is not chaparral, calculate rate of spread using Rothermel formula
        ! ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
        spdms = max(speed,0.)            ! 
        umidm = min(spdms,30.)           ! max input wind spd is 30 m/s   !param!
        umid = umidm * 196.850           ! m/s to ft/min
          !  eqn.: phiw = c * umid**bbb(i,j) * (grid%betafl(i,j)/betaop)**(-e) ! wind coef
        phiw = umid**grid%bbb(i,j) * grid%phiwc(i,j) ! wind coef
        phis=0.
        if (tanphi .gt. 0.) then
        phis = 5.275 *(grid%betafl(i,j))**(-0.3) *tanphi**2   ! slope factor
        endif
        ! rosm = grid%r_0(i,j)*(1. + phiw + phis)  * .00508 ! spread rate, m/s
        ros_base = grid%r_0(i,j) * .00508
        ros_wind = ros_base*phiw
        ros_slope= ros_base*phis!
      else
        ! Chaparral fuel, spread rate only depends on windspeed, not fuel character
        spdms = max(speed,0.)
        ! rosm = 1.2974 * spdms**1.41       ! spread rate, m/s
        ! note: backing ros is 0 for chaparral without setting nozero value below
        ! sp_n=.03333  
        ! chaparral backing fire spread rate 0.033 m/s   ! param!
        ! rosm= max(rosm, sp_n)   ! no less than backing r.o.s.

        ros_back=.03333    ! chaparral backing fire spread rate 0.033 m/s   ! param!
        ros_wind = 1.2974 * spdms**1.41       ! spread rate, m/s
        ros_wind = max(ros_wind, ros_back)
        ros_slope =0.
      endif

        ! if advection, multiply by the cosines
      ros_wind = ros_wind * cor_wind
      ros_slope = ros_slope * cor_slope

        !     ----------note!  put an 6 m/s cap on max spread rate -----------
      excess = ros_base + ros_wind + ros_slope - ros_max

      if (excess > 0.)then
          ! take it out of wind and slope in proportion
        ros_wind = ros_wind - excess * ros_wind / (ros_wind + ros_slope)
        ros_slope = ros_slope - excess * ros_slope/ (ros_wind + ros_slope)
      endif

    end subroutine Calc_ros_wrffire

    subroutine Set_ros_parameters_wrffire (this, ifds, ifde, jfds, jfde, ifms, ifme, jfms, jfme, ifts, ifte, jfts, jfte, &
        fdx, fdy, nfuel_cat, fuel_time, grid, fuel_model, config_flags)

      implicit none

    !D      BMST           RATIO OF LATENT TO SENSIBLE HEAT FROM SFC BURN:
    !                        % of total fuel mass that is water (not quite
    !                        = % fuel moisture).    BMST= (H20)/(H20+DRY)
    !                        so BMST = FUELMC_G / (1 + FUELMC_G)

      class (ros_wrffire_t), intent (in) :: this
type (state_fire_t), target :: grid
type (fuel_anderson_t), target :: fuel_model
integer, intent(in)::ifds,ifde,jfds,jfde                        ! fire domain bounds
integer, intent(in)::ifts,ifte,jfts,jfte                        ! fire tile bounds
integer, intent(in)::ifms,ifme,jfms,jfme                        ! memory array bounds
real, intent(in):: fdx,fdy                                      ! fire mesh spacing
type (namelist_t), intent(in) :: config_flags
real, intent(in),dimension(ifms:ifme, jfms:jfme)::nfuel_cat  ! fuel data
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   ! fire params arrays

!*** local


real::  fuelload, fueldepth, rtemp1, rtemp2, &
        qig, epsilon, rhob, wn, betaop, e, c, &
        xifr, etas, etam, a, gammax, gamma, ratio, ir, &
        fuelloadm,fdxinv,fdyinv
real:: bmst
integer:: i,j,k
character(len=128)::msg

integer :: kk
integer,parameter :: nf_sb = 204 ! maximum category on 
integer,dimension(1:nf_sb) :: ksb ! Anderson82 + S&B2005 fuel categories array

!*** executable

!*** Scott & Burgan ***! 
! assign no fuel by default to all the categories
do kk=1,nf_sb
   ksb(kk)=14
enddo
! Anderson 1982
ksb(1)=1
ksb(2)=2
ksb(3)=3
ksb(4)=4
ksb(5)=5
ksb(6)=6
ksb(7)=7
ksb(8)=8
ksb(9)=9
ksb(10)=10
ksb(11)=11
ksb(12)=12
ksb(13)=13
! Scott & Burgan crosswalks
! Short grass -- 1
ksb(101)=1
ksb(104)=1
ksb(107)=1
! Timber grass and understory -- 2
ksb(102)=2
ksb(121)=2
ksb(122)=2
ksb(123)=2
ksb(124)=2
! Tall grass -- 3
ksb(103)=3
ksb(105)=3
ksb(106)=3
ksb(108)=3
ksb(109)=3
! Chaparral -- 4
ksb(145)=4
ksb(147)=4
! Brush -- 5
ksb(142)=5
! Dormant Brushi -- 6
ksb(141)=6
ksb(146)=6
! Southern Rough -- 7
ksb(143)=7
ksb(144)=7
ksb(148)=7
ksb(149)=7
! Compact Timber Litter -- 8
ksb(181)=8
ksb(183)=8
ksb(184)=8
ksb(187)=8
! Hardwood Litter -- 9
ksb(182)=9
ksb(186)=9
ksb(188)=9
ksb(189)=9
! Timber (understory) -- 10
ksb(161)=10
ksb(162)=10
ksb(163)=10
ksb(164)=10
ksb(165)=10
! Light Logging Slash -- 11
ksb(185)=11
ksb(201)=11
! Medium Logging Slash -- 12
ksb(202)=12
! Heavy Logging Slash -- 13
ksb(203)=13
ksb(204)=13

! ****** !

do j=jfts,jfte
   do i=ifts,ifte
     ! fuel category 
     k=ksb(int(nfuel_cat(i,j))) ! DME S&B05
     if(k.eq.NO_FUEL_CAT)then   ! no fuel 
        grid%fgip(i,j)=0.            ! no mass 
        grid%ischap(i,j)=0.
        grid%betafl(i,j)=1.          ! DME: set to 1.0 to prevent grid%betafl(i,j)**(-0.3) to be Inf in fire_ros
        grid%bbb(i,j)=1.             !
        fuel_time(i,j)=7./0.85  ! does not matter, just what was there before
        grid%phiwc(i,j)=0.
        grid%r_0(i,j)=0.             ! no fuel, no spread.
        grid%iboros(i,j)=0.   ! DME Ib/ROS zero for no fuel
     else
        if(k.lt.1.or.k.gt.N_FUEL_CAT)then
!$OMP CRITICAL(FIRE_PHYS_CRIT)
            write(msg,'(3(a,i5))')'nfuel_cat(', i ,',',j,')=',k
!$OMP END CRITICAL(FIRE_PHYS_CRIT)
            call message(msg,config_flags%fire_print_msg)
            call crash('Set_fire_params: fuel category out of bounds')
        endif

        fuel_time(i,j)=fuel_model%weight(k)/0.85 ! cell based

        ! set fuel time constant: weight=1000 => 40% decrease over 10 min
        ! fuel decreases as exp(-t/fuel_time) 
        ! exp(-600*0.85/1000) = approx 0.6 

        grid%ischap(i,j)=fuel_model%ichap(k)
        grid%fgip(i,j)=fuel_model%fgi(k)
        ! PAJM: is it possible that if fire_fmc_read == 0, needed by the FMC model, grid%fmc_g is not initialized?
        ! This line not needed since fmc_g is initialized from the config_flags in the init_state sub
        if (config_flags%fire_fmc_read == 1) grid%fmc_g(i, j) = config_flags%fuelmc_g

        !     ...Settings of fire spread parameters from Rothermel follows. These
        !        don't need to be recalculated later.

        bmst     = grid%fmc_g(i,j) / (1.+grid%fmc_g(i,j))
        fuelloadm= (1.-bmst) * fuel_model%fgi(k)  !  fuelload without moisture
        fuelload = fuelloadm * (.3048)**2 * 2.205    ! to lb/ft^2
        fueldepth = fuel_model%fueldepthm(k)/0.3048               ! to ft
        grid%betafl(i,j) = fuelload/(fueldepth * fuel_model%fueldens(k))! packing ratio
        betaop = 3.348 * fuel_model%savr(k)**(-0.8189)     ! optimum packing ratio
        qig = 250. + 1116.*grid%fmc_g(i,j)            ! heat of preignition, btu/lb
        epsilon = exp(-138./fuel_model%savr(k) )    ! effective heating number
        rhob = fuelload/fueldepth    ! ovendry bulk density, lb/ft^3

        c = 7.47 * exp( -0.133 * fuel_model%savr(k)**0.55)    ! const in wind coef
        grid%bbb(i,j) = 0.02526 * fuel_model%savr(k)**0.54      ! const in wind coef
        e = 0.715 * exp( -3.59e-4 * fuel_model%savr(k))       ! const in wind coef
        grid%phiwc(i,j) = c * (grid%betafl(i,j)/betaop)**(-e)

        rtemp2 = fuel_model%savr(k)**1.5
        gammax = rtemp2/(495. + 0.0594*rtemp2)              ! maximum rxn vel, 1/min
        a = 1./(4.774 * fuel_model%savr(k)**0.1 - 7.27)   ! coef for optimum rxn vel
        ratio = grid%betafl(i,j)/betaop
        gamma = gammax *(ratio**a) *exp(a*(1.-ratio)) !optimum rxn vel, 1/min

        wn = fuelload/(1 + fuel_model%st(k))       ! net fuel loading, lb/ft^2
        rtemp1 = grid%fmc_g(i,j)/fuel_model%fuelmce(k)
        etam = 1.-2.59*rtemp1 +5.11*rtemp1**2 -3.52*rtemp1**3  !moist damp coef
        etas = 0.174* fuel_model%se(k)**(-0.19)                ! mineral damping coef
        ir = gamma * wn * FUELHEAT * etam * etas  !rxn intensity,btu/ft^2 min
        ! irm = ir * 1055./( 0.3048**2 * 60.) * 1.e-6     !for mw/m^2
        grid%iboros(i,j) = ir * 1055./( 0.3048**2 * 60.) * 1.e-3 * (60.*12.6/fuel_model%savr(k))     ! I_R x t_r (kJ m^-2)

        xifr = exp( (0.792 + 0.681*fuel_model%savr(k)**0.5) &
            * (grid%betafl(i,j)+0.1)) /(192. + 0.2595*fuel_model%savr(k)) ! propagating flux ratio

!        ... r_0 is the spread rate for a fire on flat ground with no wind.
        grid%r_0(i,j) = ir*xifr/(rhob * epsilon *qig)    ! default spread rate in ft/min

     endif
  enddo
enddo

    end subroutine Set_ros_parameters_wrffire

  end module ros_wrffire_mod
